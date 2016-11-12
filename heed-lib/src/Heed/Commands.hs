{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heed.Commands where

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
    | Feeds [FeedList]
    deriving (Generic, Show)

instance FromJSON Down

instance ToJSON Down


data FeedList' a b c
    = FeedList'
    { feedListId :: a
    , feedListName :: b
    , feedListUnread :: c
    } deriving (Generic, Show)

type FeedList = FeedList' Int T.Text Int64

instance FromJSON FeedList

instance ToJSON FeedList
