{-# LANGUAGE OverloadedStrings #-}

module Heed.Views where

import React.Flux

import Heed.FeedListStore

-- | The controller view and also the top level of the Heed app.
heedApp :: ReactView ()
heedApp = defineControllerView "Heed app" feedListStore $ \_ () -> h1_ "hello"
