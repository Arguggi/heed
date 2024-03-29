{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Vty
  ( main,
  )
where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Control.Applicative (optional, (<**>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    tryPutMVar,
    tryTakeMVar,
  )
import Control.Exception (Handler (..), catches, throwIO)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Ini (lookupValue, readIniFile)
import Data.Serialize (decode, encode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Graphics.Vty as V
import Heed.Commands (AuthData (AuthData), Token (Token))
import Heed.Types (ExitType (..))
import Heed.Utils (fork_, progName)
import Heed.Vty.MainWidget (MyEvent (..), app)
import Heed.Vty.WidgetStates
  ( MyWebsocketException (DeadConnection, InvalidDataOnWs),
    defState,
  )
import Network.HTTP.Simple
  ( Request,
    defaultRequest,
    getResponseBody,
    httpLBS,
    setRequestBodyLBS,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestPort,
    setRequestSecure,
  )
import Network.Socket (PortNumber)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    flag,
    fullDesc,
    help,
    helpDoc,
    info,
    infoOption,
    long,
    metavar,
    short,
    strOption,
  )
import Paths_heed_vty (version)
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
    makeAbsolute,
    withCurrentDirectory,
  )
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (text)
import Text.Read (readEither)
import Wuss (runSecureClientWith)

main :: IO ()
main = do
  options <- execParser optsParserInfo
  unless (unForceStart . forceStart $ options) checkPermission
  configFilePath <- case configPath options of
    Nothing -> do
      xdgPath <- getXdgDirectory XdgConfig progName
      createDirectoryIfMissing True xdgPath
      return $ xdgPath </> (progName ++ ".ini")
    Just (ConfigPath path) -> return (T.unpack path)
  absoluteFilePath <- makeAbsolute configFilePath
  putStrLn $ "Reading config from path " <> absoluteFilePath
  final <-
    runExceptT $ do
      (req, host, port, secure) <- authRequest configFilePath
      liftIO $ putStrLn "Authenticating"
      liftIO . print $ req
      authCheck <- liftIO $ httpLBS req
      let respToken :: Either String Token
          respToken = decode . toStrict . getResponseBody $ authCheck
      -- Lift Either PeekException to ExceptT String
      token <- ExceptT . return $ respToken
      eventChan <- liftIO $ BChan.newBChan 200
      aliveMVar <- liftIO newEmptyMVar
      let websocketClient =
            if secure
              then secureWebsocket host port token aliveMVar
              else insecureWebsocket host port token aliveMVar
      liftIO $
        websocketClient (startApp eventChan aliveMVar)
          `catches` [ignoreHandshakeExcep, ignoreWsExcep]
  case final of
    Left e -> putStrLn e
    Right UserExit -> putStrLn "Exiting"
    Right WsDisconnect -> putStrLn "Connection to server lost, exiting"
  return ()

insecureWebsocket ::
  -- | Host
  T.Text ->
  -- | Port
  Int ->
  -- | Token value
  Token ->
  -- | MVar to be filled on pong
  MVar () ->
  (WS.ClientApp a -> IO a)
insecureWebsocket host port (Token t) aliveMVar =
  WS.runClientWith
    (T.unpack host)
    port
    "/"
    ( WS.ConnectionOptions
        (fillMVarOnPong aliveMVar)
        (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)
        True
        WSC.NoSizeLimit
        WSC.NoSizeLimit
    )
    [("auth-token", encodeUtf8 t)]

secureWebsocket ::
  -- | Host
  T.Text ->
  -- | Port
  Int ->
  -- | Token value
  Token ->
  -- | MVar to be filled on pong
  MVar () ->
  (WS.ClientApp a -> IO a)
secureWebsocket host port (Token t) aliveMVar =
  runSecureClientWith
    (T.unpack host)
    ((read . show $ port) :: PortNumber)
    "/"
    ( WS.ConnectionOptions
        (fillMVarOnPong aliveMVar)
        (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)
        True
        WSC.NoSizeLimit
        WSC.NoSizeLimit
    )
    [("auth-token", encodeUtf8 t)]

fillMVarOnPong :: MVar () -> IO ()
fillMVarOnPong aliveMVar = void $ tryPutMVar aliveMVar ()

startApp :: BChan.BChan MyEvent -> MVar () -> WS.Connection -> IO ExitType
startApp eventChan aliveMVar wsconn = do
  putStrLn "Opening websocket connection"
  -- Send pings and check if we recevied pongs after 'aliveInterval' time
  -- has passed. Sending a different integer on every ping was copied from the
  -- websocket library, not sure if necessary
  fork_ . flip iterateM_ (1 :: Integer) $ \i -> do
    WS.sendPing wsconn (T.pack . show $ i)
    threadDelay aliveInterval
    isAlive <- tryTakeMVar aliveMVar
    case isAlive of
      Nothing -> throwIO DeadConnection
      Just _ -> return $ i + 1
  -- Listen for updates from the server
  fork_ . forever $ do
    wsdata <- WS.receiveData wsconn
    case decode wsdata of
      Left _ -> throwIO InvalidDataOnWs
      Right mess -> BChan.writeBChan eventChan (WsReceive mess)
  vty <- V.mkVty mempty
  _ <- M.customMain vty (V.mkVty mempty) (Just eventChan) app (defState "" wsconn "Connecting")
  return UserExit

-- Check if connection is alive every 5 seconds
aliveInterval :: Int
aliveInterval = 5 * 1000 * 1000

-- We maybe don't wan't to exit on any 'WS.ConnectionException' in the future
-- so we'll catch them
ignoreWsExcep :: Handler ExitType
ignoreWsExcep = Handler $ \(_ :: WS.ConnectionException) -> return WsDisconnect

-- We maybe don't wan't to exit on any 'WS.ConnectionException' in the future
-- so we'll catch them
ignoreHandshakeExcep :: Handler ExitType
ignoreHandshakeExcep = Handler $ \(_ :: WS.HandshakeException) -> return WsDisconnect

type Host = T.Text

type Port = Int

type IsSecure = Bool

-- Build authencation request and return host,port,ssl status se we can start a
-- websocket connection after getting the token
authRequest ::
  -- | Configuration Folder
  FilePath ->
  ExceptT String IO (Request, Host, Port, IsSecure)
authRequest path = do
  ini <- ExceptT . readIniFile $ path
  ExceptT . return $ do
    host <- lookupValue "server" "host" ini
    portT <- lookupValue "server" "port" ini
    user <- lookupValue "auth" "username" ini
    pass <- lookupValue "auth" "password" ini
    -- Assume tls and port 443 by default
    let secure = fromRight True $ isSecure <$> lookupValue "server" "tls" ini
        port = fromRight 443 $ readEither (T.unpack portT)
    return
      ( defaultRequest & setRequestBodyLBS (fromStrict . encode $ AuthData user pass)
          & setRequestHost (encodeUtf8 host)
          & setRequestPort port
          & setRequestPath "auth"
          & setRequestMethod "POST"
          & setRequestSecure secure,
        host,
        port,
        secure
      )
  where
    isSecure x = T.toLower x == "on"

newtype ForceStart = ForceStart {unForceStart :: Bool}

newtype ConfigPath = ConfigPath T.Text

data Options = Options
  { forceStart :: ForceStart,
    configPath :: Maybe ConfigPath
  }

optsParserInfo :: ParserInfo Options
optsParserInfo = info (optsParser <**> versionParser) fullDesc

versionParser :: Parser (a -> a)
versionParser =
  infoOption
    (concat [showVersion version, " ", $(gitHash)])
    (short 'v' <> long "version" <> help "Show version")

optsParser :: Parser Options
optsParser =
  Options
    <$> forceParse
    <*> optional configParse

forceParse :: Parser ForceStart
forceParse =
  ForceStart
    <$> flag
      False
      True
      ( short 'f' <> long "force" <> helpDoc (Just (text "don't check timestamp"))
      )

configParse :: Parser ConfigPath
configParse =
  ConfigPath
    <$> strOption
      ( long "config" <> short 'c' <> metavar "CONFIG" <> help "Config file"
      )

checkPermission :: IO ()
checkPermission = do
  dataFolder <- getXdgDirectory XdgData progName
  createDirectoryIfMissing True dataFolder
  withCurrentDirectory dataFolder $ do
    fileExists <- doesFileExist "timestamp"
    if fileExists
      then do
        putStrLn "Old timestamp found"
        checkTimeDiff
      else do
        putStrLn "Creating timestamp for the first time"
        saveTimeStamp

timestampFile :: String
timestampFile = "timestamp"

pauseInterval :: NominalDiffTime
pauseInterval = 60 * 60 * 4

checkTimeDiff :: IO ()
checkTimeDiff = do
  putStrLn "Checking time diff"
  now <- getCurrentTime
  oldTimestamp :: UTCTime <- read . T.unpack <$> TIO.readFile timestampFile
  let nextAllowedTime = addUTCTime pauseInterval oldTimestamp
  if nextAllowedTime > now
    then do
      putStrLn $ "It is now: " <> show now
      putStrLn $ "You may open heed after: " <> show nextAllowedTime
      putStrLn $ "or in: " <> show (diffUTCTime nextAllowedTime now / 60) <> " minutes"
      exitSuccess
    else saveTimeStamp

saveTimeStamp :: IO ()
saveTimeStamp = do
  putStrLn "Saving now to timestamp file"
  now <- getCurrentTime
  writeFile timestampFile (show now)
