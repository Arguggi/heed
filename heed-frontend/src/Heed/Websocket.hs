module Heed.Websocket where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Aeson (decodeStrict')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Heed.Commands
import Heed.FeedListStore
import Heed.GlobalWebsocket
import Heed.GHCJS
import Heed.ItemListStore
import qualified JSDOM.EventM as E
import JSDOM.Generated.MessageEvent (getData)
import JSDOM.Generated.WebSocket
import JSDOM.Types
import React.Flux

-- | Websocket server url
wsUrl :: String
wsUrl = "ws://localhost:8080"

-- | Websocket server protocols
heedProtocol :: String
heedProtocol = "heed"

-- | Open a websocket with 'wsUrl' and use set "open" and "message" events
initWebsocket :: IO ()
initWebsocket = do
    websocket <-
        runJ $
        do ws <- newWebSocket' wsUrl heedProtocol
           _ <- E.on ws open sendInitialized
           _ <- E.on ws message commandToStore
           return ws
    putMVar heedWebsocket websocket

-- | onOpen websocket callback
sendInitialized :: ReaderT e DOM ()
sendInitialized =
    ReaderT $
    \_ ->
         liftIO $
         do putStrLn "Connection opened"
            putStrLn "Getting user info"
            --runJ $ sendString ws (toStrict . decodeUtf8 . encode $ Initialized)
            sendCommand Initialized

-- | onMessage websocket callback
--
-- Simply decodes the message and selects the right store to alter
commandToStore :: ReaderT MessageEvent DOM ()
commandToStore =
    ReaderT $
    \mess -> do
        messData <- getData mess
        tVal <- fromJSVal messData
        let command =
                fromMaybe InvalidSent $
                do justVal <- tVal
                   decodeWsMess justVal
        case command of
            InvalidSent -> liftIO $ putStrLn "Invalid command received"
            Feeds feeds -> liftIO $ alterStore feedListStore (SetFeedList feeds)
            FeedItems items -> liftIO $ alterStore itemListStore (SetItemList items)
            _ -> liftIO $ putStrLn "TODO"

-- | Decode websocket message
decodeWsMess :: T.Text -> Maybe Down
decodeWsMess val = decodeStrict' (encodeUtf8 val)
