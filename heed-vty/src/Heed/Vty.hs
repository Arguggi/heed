{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Vty where

import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
       (ExceptT(..), runExceptT, withExceptT)
import qualified Data.ByteString as BS
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
            liftIO $ putStrLn "Starting heed"
            let websocketClient =
                    if secure
                        then secureWebsocket host port token
                        else insecureWebsocket host port token
            liftIO $ websocketClient startApp
    case final of
        Left e -> putStrLn e
        _ -> return ()
    return ()

insecureWebsocket
    :: T.Text -- ^ Host
    -> Int -- ^ Port
    -> Token -- ^ Token value
    -> (WS.ClientApp () -> IO ())
insecureWebsocket host port (Token t) =
    WS.runClientWith
        (T.unpack host)
        port
        "/"
        WS.defaultConnectionOptions
        [("auth-token", encodeUtf8 t)]

secureWebsocket
    :: T.Text -- ^ Host
    -> Int -- ^ Port
    -> Token -- ^ Token value
    -> (WS.ClientApp () -> IO ())
secureWebsocket host port (Token t) =
    runSecureClientWith
        (T.unpack host)
        ((read . show $ port) :: PortNumber)
        "/"
        WS.defaultConnectionOptions
        [("auth-token", encodeUtf8 t)]

startApp :: WS.Connection -> IO ()
startApp wsconn =
    flip finally (WS.sendClose wsconn BS.empty) $ do
        eventChan <- BChan.newBChan 200
        fork_ . forever $ do
            wsdata <- WS.receiveData wsconn :: IO BS.ByteString
            case decode wsdata of
                Left _ -> return ()
                Right mess -> BChan.writeBChan eventChan (WsReceive mess)
        _ <- M.customMain (V.mkVty mempty) (Just eventChan) app (defState "" wsconn "Connecting")
        return ()

type Host = T.Text

type Port = Int

authRequest
    :: FilePath -- ^ Configuration Folder
    -> ExceptT String IO (Request, Host, Port, Bool)
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
