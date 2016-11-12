{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Heed.Views
import React.Flux
--import JavaScript.Web.WebSocket
import JSDOM.Generated.WebSocket
import JSDOM.Types
--import GHCJS.DOM.WebSocket
--
wsUrl :: String
wsUrl = "ws://localhost:8080"

protocols :: Maybe [String]
protocols = Just ["heed"]

heedProtocol :: String
heedProtocol = "heed"

main :: IO ()
main = do
    ws <- runJSM (newWebSocket' wsUrl heedProtocol) undefined
    reactRender "heedapp" heedApp ()
