{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decodeStrict')
import Data.Maybe (fromMaybe)
import Heed.Views
import Heed.Commands
import Heed.FeedListStore
import React.Flux
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import JSDOM.Generated.WebSocket
import JSDOM.Types
import qualified JSDOM.EventM as E
import Control.Monad.Reader
import JSDOM.Generated.MessageEvent (getData)

--
wsUrl :: String
wsUrl = "ws://localhost:8080"

protocols :: Maybe [String]
protocols = Just ["heed"]

heedProtocol :: String
heedProtocol = "heed"

main :: IO ()
main = do
    reactRender "heedapp" heedApp ()
    _ <-
        flip runJSM undefined $
        do ws <- newWebSocket' wsUrl heedProtocol
           _ <- E.on ws open logToConsole
           _ <- E.on ws message commandToStore
           return ()
    return ()

-- onOpen websocket callback
logToConsole :: ReaderT e DOM ()
logToConsole = ReaderT $ \_ -> liftIO $ putStrLn "Ciao"

-- onMessage websocket callback
commandToStore :: ReaderT MessageEvent DOM ()
commandToStore =
    ReaderT $
    \mess -> do
        messData <- getData mess
        tVal <- fromJSVal messData
        let command =
                fromMaybe InvalidReceived $
                do justVal <- tVal
                   decodeWsMess justVal
        case command of
            InvalidReceived -> liftIO $ putStrLn "Invalid command received"
            Feeds feeds -> liftIO $ alterStore feedListStore (SetFeedList feeds)

decodeWsMess :: T.Text -> Maybe Down
decodeWsMess val = decodeStrict' (encodeUtf8 val)
