{-# LANGUAGE OverloadedStrings #-}

module Main where

import Heed.Keybindings
import Heed.Views
import Heed.Websocket
import React.Flux

-- | Open websocket with server, starts react and setups keybindings
main :: IO ()
main = do
    initWebsocket
    reactRender "heedapp" heedApp ()
    initKeybindings
