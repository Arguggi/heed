{-# LANGUAGE OverloadedStrings #-}

module Heed.Vty where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Default
import Data.Function ((&))
import Data.Ini (lookupValue, readIniFile)
import Data.Monoid ((<>))
import Data.Store (decode, encode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Graphics.Vty as V
import Heed.Commands
import Heed.Utils (progName)
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
import System.Exit (die)
import Text.Read (readEither)
import Wuss (runSecureClientWith)

main :: IO ()
main = do
    configFolder <- getXdgDirectory XdgConfig progName
    createDirectoryIfMissing True configFolder
    creds <- authRequest configFolder
    case creds of
        Left e -> putStrLn $ "Invalid config file: " <> e
        Right (req, host, port, secure) -> do
            putStrLn "Authenticating"
            authCheck <- httpLBS req
            case decode . toStrict . getResponseBody $ authCheck of
                Left e -> print e
                Right (Token t) -> do
                    putStrLn "Starting heed"
                    let websocketClient =
                            if secure
                                then secureWebsocket host port t
                                else insecureWebsocket host port t
                    websocketClient startApp
                    putStrLn "Closing"

insecureWebsocket
    :: T.Text -- | Host
    -> Int -- | Port
    -> T.Text -- | Token value
    -> (WS.ClientApp () -> IO ())
insecureWebsocket host port token =
    WS.runClientWith
        (T.unpack host)
        port
        "/"
        WS.defaultConnectionOptions
        [("auth-token", encodeUtf8 token)]

secureWebsocket
    :: T.Text -- | Host
    -> Int -- | Port
    -> T.Text -- | Token value
    -> (WS.ClientApp () -> IO ())
secureWebsocket host port token =
    runSecureClientWith
        (T.unpack host)
        ((read . show $ port) :: PortNumber)
        "/"
        WS.defaultConnectionOptions
        [("auth-token", encodeUtf8 token)]

startApp :: WS.Connection -> IO ()
startApp wsconn = do
    eventChan <- BChan.newBChan 200
    _ <-
        forkIO . forever $
        do wsdata <- WS.receiveData wsconn :: IO BS.ByteString
           case decode wsdata of
               Left _ -> return ()
               Right mess -> BChan.writeBChan eventChan (WsReceive mess)
    _ <-
        M.customMain
            (V.mkVty Data.Default.def)
            (Just eventChan)
            app
            (defState "" wsconn "Connecting")
    return ()

type Host = T.Text

type Port = Int

authRequest
    :: (MonadIO m)
    => FilePath -> m (Either String (Request, Host, Port, Bool))
authRequest configFolder = do
    iniFile <- liftIO . readIniFile $ configFolder <> "/" <> progName <> ".ini"
    case iniFile of
        Left e -> liftIO . die $ "Invalid config file: " <> e
        Right ini ->
            return $
            do host <- lookupValue "server" "host" ini
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
