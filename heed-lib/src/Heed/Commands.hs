{-# LANGUAGE DeriveGeneric #-}

module Heed.Commands
  ( Up(..)
  , Down(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data Up = GetFeeds deriving (Generic, Show)

instance FromJSON Up
instance ToJSON Up

data Down
    = NewItems
    | Testing
    | Name T.Text
    deriving (Generic, Show)


instance FromJSON Down
instance ToJSON Down
