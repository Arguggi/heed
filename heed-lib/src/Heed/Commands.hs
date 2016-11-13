{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heed.Commands where

import Control.DeepSeq
import Data.Aeson
import Data.Time.Clock
import Data.Int
import qualified Data.Text as T
import GHC.Generics
import Heed.Database

data Up
    = Initialized
    | GetFeedItems Int
    | ItemRead FeedItemIdH
    | InvalidReceived
    deriving (Generic, Show)

instance FromJSON Up

instance ToJSON Up

data Down
    = Feeds [ReactFeedInfo]
    | FeedItems [ReactItemInfo]
    | Status HeedUserName
    | InvalidSent
    deriving (Generic, Show)

instance FromJSON Down

instance ToJSON Down

type HeedUserName = T.Text

data ReactFeedInfo' a b c = ReactFeedInfo'
    { feedListId :: a
    , feedListName :: b
    , feedListUnread :: c
    } deriving (Generic, Show)

type ReactFeedInfo = ReactFeedInfo' Int T.Text Int64

instance FromJSON ReactFeedInfo

instance ToJSON ReactFeedInfo

instance NFData ReactFeedInfo


data ReactItemInfo' a b c d e = ReactItemInfo'
    { itemInfoId :: a
    , itemInfoTitle :: b
    , itemInfoLink :: c
    , itemInfoDate :: d
    , itemInfoComments :: e
    } deriving (Generic, Show)

type ReactItemInfo = ReactItemInfo' Int T.Text T.Text UTCTime (Maybe T.Text)

instance FromJSON ReactItemInfo

instance ToJSON ReactItemInfo

instance NFData ReactItemInfo
