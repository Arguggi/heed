{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Vty
    ( main
    ) where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Control.Applicative ((<**>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (Handler(..), catches, throwIO)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Function ((&))
import Data.Ini (lookupValue, readIniFile)
import Data.Monoid ((<>))
import Data.Serialize (decode, encode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Graphics.Vty as V
import Heed.Commands
import Heed.Types (ExitType(..))
import Heed.Utils (fork_, progName)
import Heed.Vty.MainWidget
import Heed.Vty.WidgetStates
import Network (PortNumber)
import Network.HTTP.Simple
       (Request, defaultRequest, getResponseBody, httpLBS,
        setRequestBodyLBS, setRequestHost, setRequestMethod,
        setRequestPath, setRequestPort, setRequestSecure)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import Options.Applicative
       (Parser, ParserInfo, execParser, flag, help, info, infoOption, long, fullDesc, short, helpDoc)
import Paths_heed_vty (version)
import System.Directory
       (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory, doesFileExist, withCurrentDirectory)
import System.Exit (exitSuccess)
import Text.PrettyPrint.ANSI.Leijen (text)
import Text.Read (readEither)
import Wuss (runSecureClientWith)

main :: IO ()
main = do
    force <- execParser optsParserInfo
    unless force checkPermission
    configFolder <- getXdgDirectory XdgConfig progName
    createDirectoryIfMissing True configFolder
    final <-
        runExceptT $ do
            (req, host, port, secure) <- authRequest configFolder
            liftIO $ putStrLn "Authenticating"
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
                websocketClient (startApp eventChan aliveMVar) `catches`
                [ignoreHandshakeExcep, ignoreWsExcep]
    case final of
        Left e -> putStrLn e
        Right UserExit -> putStrLn "Exiting"
        Right WsDisconnect -> putStrLn "Connection to server lost, exiting"
    return ()

insecureWebsocket
    :: T.Text -- ^ Host
    -> Int -- ^ Port
    -> Token -- ^ Token value
    -> MVar () -- ^ MVar to be filled on pong
    -> (WS.ClientApp a -> IO a)
insecureWebsocket host port (Token t) aliveMVar =
    WS.runClientWith
        (T.unpack host)
        port
        "/"
        (WS.ConnectionOptions
            (fillMVarOnPong aliveMVar)
            (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)
            True
            WSC.NoSizeLimit
            WSC.NoSizeLimit)
        [("auth-token", encodeUtf8 t)]

secureWebsocket
    :: T.Text -- ^ Host
    -> Int -- ^ Port
    -> Token -- ^ Token value
    -> MVar () -- ^ MVar to be filled on pong
    -> (WS.ClientApp a -> IO a)
secureWebsocket host port (Token t) aliveMVar =
    runSecureClientWith
        (T.unpack host)
        ((read . show $ port) :: PortNumber)
        "/"
        (WS.ConnectionOptions
            (fillMVarOnPong aliveMVar)
            (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)
            True
            WSC.NoSizeLimit
            WSC.NoSizeLimit)
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
    _ <- M.customMain (V.mkVty mempty) (Just eventChan) app (defState "" wsconn "Connecting")
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
authRequest
    :: FilePath -- ^ Configuration Folder
    -> ExceptT String IO (Request, Host, Port, IsSecure)
authRequest configFolder = do
    ini <- ExceptT . readIniFile $ configFolder <> "/" <> progName <> ".ini"
    ExceptT . return $ do
        host <- lookupValue "server" "host" ini
        portT <- lookupValue "server" "port" ini
        user <- lookupValue "auth" "username" ini
        pass <- lookupValue "auth" "password" ini
        -- Assume tls and port 443 by default
        let secure = either (const True) id $ isSecure <$> lookupValue "server" "tls" ini
            port = either (const 443) id $ readEither (T.unpack portT)
        return
            ( defaultRequest & setRequestBodyLBS (fromStrict . encode $ AuthData user pass) &
              setRequestHost (encodeUtf8 host) &
              setRequestPort port &
              setRequestPath "auth" &
              setRequestMethod "POST" &
              setRequestSecure secure
            , host
            , port
            , secure)
  where
    isSecure x = T.toLower x == "on"

type ForceStart = Bool

optsParserInfo :: ParserInfo ForceStart
optsParserInfo = info (optsParser <**> versionParser) fullDesc

versionParser :: Parser (a -> a)
versionParser =
    infoOption
        (concat [showVersion version, " ", $(gitHash)])
        (short 'v' <> long "version" <> help "Show version")

optsParser :: Parser ForceStart
optsParser =
    flag False True
        (  short 'f'
        <> long "force"
        <> helpDoc (Just (text "don't check timestamp")))

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
