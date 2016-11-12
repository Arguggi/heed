{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heed.Commands where

import Control.DeepSeq
import Data.Aeson
import Data.Int
import qualified Data.Text as T
import GHC.Generics
import Heed.Database

data Up
    = GetFeeds
    | GetFeedItems FeedItemIdH
    | ItemRead FeedItemIdH
    deriving (Generic, Show)

instance FromJSON Up

instance ToJSON Up

data Down
    = NewItems
    | Feeds [ReactFeedInfo]
    deriving (Generic, Show)

instance FromJSON Down

instance ToJSON Down


data ReactFeedInfo' a b c
    = ReactFeedInfo'
    { feedListId :: a
    , feedListName :: b
    , feedListUnread :: c
    } deriving (Generic, Show)

type ReactFeedInfo = ReactFeedInfo' Int T.Text Int64

instance FromJSON ReactFeedInfo

instance ToJSON ReactFeedInfo

instance NFData ReactFeedInfo
