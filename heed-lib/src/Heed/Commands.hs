{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heed.Commands where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Int
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

type HeedUserName = T.Text

data ReactFeedInfo' a b c = ReactFeedInfo'
    { _feedListId :: a
    , _feedListName :: b
    , _feedListUnread :: c
    } deriving (Generic, Show)

type ReactFeedInfo = ReactFeedInfo' Int T.Text Int64

makeLenses ''ReactFeedInfo'

instance Eq ReactFeedInfo where
    a == b = _feedListId a == _feedListId b

instance FromJSON ReactFeedInfo

instance ToJSON ReactFeedInfo

instance NFData ReactFeedInfo

data ReactItemInfo' a b c d e = ReactItemInfo'
    { _itemInfoId :: a
    , _itemInfoTitle :: b
    , _itemInfoLink :: c
    , _itemInfoDate :: d
    , _itemInfoComments :: e
    } deriving (Generic, Show)

type ReactItemInfo = ReactItemInfo' Int T.Text T.Text UTCTime (Maybe T.Text)

makeLenses ''ReactItemInfo'

instance Eq ReactItemInfo where
    a == b = _itemInfoId a == _itemInfoId b

instance FromJSON ReactItemInfo

instance ToJSON ReactItemInfo

instance NFData ReactItemInfo

class IsSelected a  where
    isSelected :: a -> Maybe a -> Bool

instance IsSelected ReactFeedInfo where
    isSelected _ Nothing = False
    isSelected selId (Just feedInfo) = _feedListId selId == _feedListId feedInfo

instance IsSelected ReactItemInfo where
    isSelected _ Nothing = False
    isSelected selId (Just itemInfo) = _itemInfoId selId == _itemInfoId itemInfo

data Up
    = Initialized
    | GetFeedItems Int
    | ItemRead Int
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
