{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heed.Database where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import GHC.Generics

type Url = Text

-- Type name guide:
-- W = Write
-- R = Read
-- H = Haskell world
-- WO = Write Optional
-- Users Table
data User a b c d = User
    { userId :: a -- PGInt4
    , userName :: b -- Text
    , userPassword :: c -- Bytestring
    , userEmail :: c -- Text
    }

newtype UserId a = UserId
    { getUserId :: a
    } deriving (Functor)

type UserH = User (UserId Int) Text Text Text

-----------------------------------------
-- Feeds Table
--
data FeedInfo a b c d e = FeedInfo
    { feedInfoId :: a -- PGInt4
    , feedInfoName :: b -- Text
    , feedInfoUrl :: c -- Url
    , feedInfoUpdateEvery :: d -- Minutes
    , feedInfoLastUpdated :: e -- timestamp
    } deriving (Show, Generic)

newtype FeedInfoId a = FeedInfoId
    { getFeedInfoId :: a
    } deriving (Functor, Show, Generic)

instance FromJSON FeedInfoIdH

instance ToJSON FeedInfoIdH

type FeedInfoIdH = FeedInfoId Int

type FeedInfoHR = FeedInfo (FeedInfoId Int) Text Text Int UTCTime

type FeedInfoHW = FeedInfo (FeedInfoId (Maybe Int)) Text Text Int UTCTime

instance FromJSON FeedInfoHR

instance ToJSON FeedInfoHR

----------------------------
-- Feeds <-> Users (Subscriptions)
data Subscription a b = Subscription
    { subscriptionFeedId :: a -- PGInt4
    , subscriptionUserId :: b -- PGInt4
    }

----------------------------
-- Feed items
data FeedItem a b c d e f = FeedItem
    { feedItemId :: a -- PGInt4
    , feedItemFeedId :: b -- PGInt4
    , feedItemTitle :: c -- Text
    , feedItemUrl :: d -- Url
    , feedItemDate :: e -- UTC
    , feedItemComments :: f -- Maybe Url
    }

newtype FeedItemId a = FeedItemId
    { getFeedItemId :: a
    } deriving (Functor, Show, Generic)

type FeedItemIdH = FeedItemId Int

instance ToJSON FeedItemIdH

instance FromJSON FeedItemIdH

type FeedItemHW = FeedItem (FeedItemId (Maybe Int)) (FeedInfoId (Maybe Int)) Text Url UTCTime (Maybe Url)

type FeedItemHR = FeedItem (FeedItemId Int) (FeedInfoId Int) Text Url UTCTime (Maybe Url)

----------------------------
-- Unread items
data UnreadItem a b = UnreadItem
    { unreadFeedItemId :: a -- PGInt4
    , unreadUserId :: b -- PGInt4
    }

data AuthToken a b = AuthToken
    { authTokenHeedUserId :: a
    , authTokenToken :: b
    }
