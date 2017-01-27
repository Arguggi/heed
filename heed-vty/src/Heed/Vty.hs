{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Vty where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (Handler(..), catches, throwIO)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Trans.Except
       (ExceptT(..), runExceptT, withExceptT)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Function ((&))
import Data.Ini (lookupValue, readIniFile)
import Data.Monoid ((<>))
import Data.Store (decode, encode)
import Data.Store.Core (PeekException)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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
import System.Directory
       (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import Text.Read (readEither)
import Wuss (runSecureClientWith)

main :: IO ()
main = do
    configFolder <- getXdgDirectory XdgConfig progName
    createDirectoryIfMissing True configFolder
    final <-
        runExceptT $ do
            (req, host, port, secure) <- authRequest configFolder
            liftIO $ putStrLn "Authenticating"
            authCheck <- liftIO $ httpLBS req
            let respToken :: Either PeekException Token
                respToken = decode . toStrict . getResponseBody $ authCheck
            -- Lift Either PeekException to ExceptT String
            token <- withExceptT show . ExceptT . return $ respToken
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
        (WS.ConnectionOptions $ fillMVarOnPong aliveMVar)
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
        (WS.ConnectionOptions $ fillMVarOnPong aliveMVar)
        [("auth-token", encodeUtf8 t)]

fillMVarOnPong :: MVar () -> IO ()
fillMVarOnPong aliveMVar = void $ tryPutMVar aliveMVar ()

startApp :: BChan.BChan MyEvent -> MVar () -> WS.Connection -> IO ExitType
startApp eventChan aliveMVar wsconn = do
    putStrLn "Opening websocket connection"
    -- Send pings and check if we recevied pongs after 'aliveInterval' time
    -- has passed
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

ignoreWsExcep :: Handler ExitType
ignoreWsExcep = Handler $ \(_ :: WS.ConnectionException) -> return WsDisconnect

ignoreHandshakeExcep :: Handler ExitType
ignoreHandshakeExcep = Handler $ \(_ :: WS.HandshakeException) -> return WsDisconnect

type Host = T.Text

type Port = Int

type IsSecure = Bool

authRequest
    :: FilePath -- ^ Configuration Folder
    -> ExceptT String IO (Request, Host, Port, IsSecure)
authRequest configFolder = do
    ini <- ExceptT . readIniFile $ configFolder <> "/" <> progName <> ".ini"
    ExceptT . return $ do
        host <- lookupValue "server" "host" ini
        portT <- lookupValue "server" "port" ini
           -- Assume tls by default
        secure <- return . either (const True) id $ isSecure <$> lookupValue "server" "tls" ini
        port <- return . either (const (443 :: Int)) id $ readEither (T.unpack portT)
        user <- lookupValue "auth" "username" ini
        pass <- lookupValue "auth" "password" ini
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
