{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Default (def)
import qualified Data.Text as T
import Heed.Commands
import Heed.Views
import React.Flux

main :: IO ()
main = reactRender "heedapp" heedApp ()
