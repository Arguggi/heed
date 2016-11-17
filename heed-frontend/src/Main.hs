{-# LANGUAGE OverloadedStrings #-}

module Main where

import Heed.Keybindings
import Heed.Views
import Heed.Websocket
import React.Flux

main :: IO ()
main = do
    initWebsocket
    reactRender "heedapp" heedApp ()
    initKeybindings
