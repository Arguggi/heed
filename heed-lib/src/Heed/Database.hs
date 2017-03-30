{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Database definitions
-}
module Heed.Database
    ( Url
    -- * User info info
    , User(..)
    , UserH
    , UserW
    , UserR
    , userTable
    -- ** User Lenses
    , userId
    , userName
    , userPassword
    , userEmail
    -- * UserId newtype
    , UserId(..)
    , UserIdColumnWO
    , UserIdColumnW
    , UserIdColumnR
    -- ** UserId lenses
    , getUserId
    -- * Feed Info info
    , FeedInfo(..)
    , FeedInfoHW
    , FeedInfoHR
    , FeedInfoW
    , FeedInfoR
    , afterDefTime
    , defTime
    , setTime
    , defFeedInfo
    , defUpdateEvery
    , feedInfoTable
    -- * Feed Info lenses
    , feedInfoId
    , feedInfoName
    , feedInfoUrl
    , feedInfoUpdateEvery
    , feedInfoLastUpdated
    , feedHasItemDate
    , feedNumberItems
    -- * FeedItem info
    , FeedItem(..)
    , FeedItemHR
    , FeedItemHW
    , FeedItemW
    , FeedItemR
    -- ** FeedItem lenses
    , feedItemId
    , feedItemFeedId
    , feedItemTitle
    , feedItemUrl
    , feedItemDate
    , feedItemComments
    , defFeedItem
    , feedItemTable
    -- * FeedItemId newtype
    , FeedItemId(..)
    , FeedItemIdH
    , FeedItemIdColumnWO
    , FeedItemIdColumnW
    , FeedItemIdColumnR
    -- ** FeedItemId lenses
    , getFeedItemId
    -- * FeedInfoId newtype
    , FeedInfoId(..)
    , FeedInfoIdH
    , FeedInfoIdColumnWO
    , FeedInfoIdColumnW
    , FeedInfoIdColumnR
    -- ** FeedInfoId lenses
    , getFeedInfoId
    -- * UnreadItem info
    , UnreadItem(..)
    , UnreadItemW
    , UnreadItemR
    , unreadItemTable
    -- ** UnreadItem lenses
    , unreadFeedItemId
    , unreadUserId
    -- * Authtoken info
    , AuthToken(..)
    , AuthTokenW
    , AuthTokenR
    , authTokenTable
    -- ** Authtoken Lenses
    , authTokenHeedUserId
    , authTokenToken
    -- * Subscription info
    , Subscription(..)
    , subscriptionTable
    -- ** Subscription lenses
    , subscriptionFeedId
    , subscriptionUserId
    -- * UserFeedInfoPref info
    , UserFeedInfoPref(..)
    , UserFeedInfoPrefHW
    , UserFeedInfoPrefHR
    , UserFeedInfoPrefW
    , UserFeedInfoPrefR
    , userPrefTable
    -- ** UserFeedInfoPref lenses
    , prefUserId
    , prefFeedId
    , prefName
    ) where

import Control.Lens
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import GHC.Generics
import Heed.DbEnums
import Heed.Orphans ()
import qualified Opaleye as O

-- * Define database types
-- Type name guide:
-- W = Write
-- R = Read
-- H = Haskell world
-- WO = Write Optional
-- Users Table
type Url = Text

-- | PostgreSQL User Table
data User a b c d = User
    { _userId :: a -- ^ PGInt4
    , _userName :: b -- ^ Text
    , _userPassword :: c -- ^ Bytestring
    , _userEmail :: c -- ^ Text
    }

makeLenses ''User

-- | For additional typesafety
newtype UserId a = UserId
    { _getUserId :: a
    } deriving (Functor)

makeLenses ''UserId

-- | User in Haskell Land
type UserH = User (UserId Int) Text Text Text

-- | PostgreSQL Feeds Table
data FeedInfo a b c d e f g = FeedInfo
    { _feedInfoId :: a -- ^ Autogenerated PGInt4
    , _feedInfoName :: b -- ^ Feed Name
    , _feedInfoUrl :: c -- ^ Feed url
    , _feedInfoUpdateEvery :: d -- ^ Update every minutes
    , _feedInfoLastUpdated :: e -- ^ Last time the feed was downloaded and items added to the db
    , _feedHasItemDate :: f -- ^ If the items in the feed have a publication date
    , _feedNumberItems :: g -- ^ If no publication date is present we download the last
      -- x items to check so we can check which items in the feed are new
    } deriving (Show, Generic)

makeLenses ''FeedInfo

instance (Eq b) =>
         Eq (FeedInfo a b c d e f g) where
    f1 == f2 = (f1 ^. feedInfoName) == (f2 ^. feedInfoName)

instance (Ord b) =>
         Ord (FeedInfo a b c d e f g) where
    f1 `compare` f2 = (f1 ^. feedInfoName) `compare` (f2 ^. feedInfoName)

-- | For additional typesafety
newtype FeedInfoId a = FeedInfoId
    { _getFeedInfoId :: a
    } deriving (Functor, Show, Generic, Eq, Ord)

makeLenses ''FeedInfoId

instance Serialize FeedInfoIdH

-- | 'FeedInfoId' Haskell
type FeedInfoIdH = FeedInfoId Int

-- | 'FeedInfo' Haskell read from DB
type FeedInfoHR = FeedInfo (FeedInfoId Int) Text Text Int UTCTime ItemsDate Int

-- | 'FeedInfo' Haskell write to DB (Id is missing)
type FeedInfoHW = FeedInfo (FeedInfoId (Maybe Int)) Text Text Int UTCTime ItemsDate Int

instance Serialize FeedInfoHR

instance (Serialize a) =>
         Serialize (FeedInfoId (Maybe a))

instance Serialize FeedInfoHW

-- | PostgreSQL Feeds <-> Users (Subscriptions) Table
data Subscription a b = Subscription
    { _subscriptionFeedId :: a -- ^ Foreign key on FeedInfoId
    , _subscriptionUserId :: b -- ^ Foreign key on UserId
    }

makeLenses ''Subscription

-- | PostgreSQL Feed items Table
data FeedItem a b c d e f = FeedItem
    { _feedItemId :: a -- ^ Autogenerated PGInt4
    , _feedItemFeedId :: b -- ^ Foreign Key to FeedInfoId PGInt4
    , _feedItemTitle :: c -- ^ Item title Text
    , _feedItemUrl :: d -- ^ Item url Url
    , _feedItemDate :: e -- ^ Item date
    , _feedItemComments :: f -- ^ Comment url if available (ala HN)
    }

makeLenses ''FeedItem

-- | For additional typesafety
newtype FeedItemId a = FeedItemId
    { _getFeedItemId :: a
    } deriving (Functor, Show, Generic)

makeLenses ''FeedItemId

-- | FeedItemId Haskell
type FeedItemIdH = FeedItemId Int

instance Serialize FeedItemIdH

-- | 'FeedItem' Haskell read from DB
type FeedItemHR = FeedItem (FeedItemId Int) (FeedInfoId Int) Text Url UTCTime (Maybe Url)

-- | 'FeedItem' Haskell write to DB (ItemId is missing)
type FeedItemHW = FeedItem (FeedItemId (Maybe Int)) (FeedInfoId (Maybe Int)) Text Url UTCTime (Maybe Url)

-- | Unread items
data UnreadItem a b = UnreadItem
    { _unreadFeedItemId :: a -- ^ Foreign key on FeedItemId -- PGInt4
    , _unreadUserId :: b -- ^ Foreign key on UserId -- PGInt4
    }

makeLenses ''UnreadItem

-- | PostgreSQL Authentication token table
data AuthToken a b = AuthToken
    { _authTokenHeedUserId :: a -- ^ Foreign key on UserId -- PGInt4
    , _authTokenToken :: b -- ^ Randomly generated token -- Text
    }

makeLenses ''AuthToken

$(makeAdaptorAndInstance "pUser" ''User)

$(makeAdaptorAndInstance "pUserId" ''UserId)

type UserW = User UserIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGText)

type UserR = User UserIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGText)

type UserIdColumnWO = UserId (Maybe (O.Column O.PGInt4))

type UserIdColumnW = UserId (O.Column O.PGInt4)

type UserIdColumnR = UserId (O.Column O.PGInt4)

userTable :: O.Table UserW UserR
userTable =
    O.Table
        "heed_user"
        (pUser
             User
             { _userId = pUserId (UserId (O.optional "id"))
             , _userName = O.required "username"
             , _userPassword = O.required "password"
             , _userEmail = O.required "email"
             })

-----------------------------------------
-- Feeds Table
$(makeAdaptorAndInstance "pFeedInfo" ''FeedInfo)

$(makeAdaptorAndInstance "pFeedInfoId" ''FeedInfoId)

type FeedInfoW = FeedInfo FeedInfoIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz) (O.Column PGItemsDate) (O.Column O.PGInt4)

type FeedInfoR = FeedInfo FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz) (O.Column PGItemsDate) (O.Column O.PGInt4)

type FeedInfoIdColumnWO = FeedInfoId (Maybe (O.Column O.PGInt4))

type FeedInfoIdColumnW = FeedInfoId (O.Column O.PGInt4)

type FeedInfoIdColumnR = FeedInfoId (O.Column O.PGInt4)

-- Update timestamp on feed read from DB and make it writable to DB
setTime :: UTCTime -> FeedInfoR -> FeedInfoW
setTime utc feedInfo =
    feedInfo
    { _feedInfoId = FeedInfoId $ Just (_getFeedInfoId . _feedInfoId $ feedInfo)
    , _feedInfoLastUpdated = O.pgUTCTime utc
    }

-- Default 'FeedInfoHW'
defFeedInfo :: FeedInfoHW
defFeedInfo =
    FeedInfo
    { _feedInfoId = FeedInfoId Nothing
    , _feedInfoName = ""
    , _feedInfoUrl = ""
    , _feedInfoUpdateEvery = defUpdateEvery
    , _feedInfoLastUpdated = defTime
    , _feedHasItemDate = Missing
    , _feedNumberItems = 20
    }

-- | Default Time ( == 0 seconds absolute)
defTime :: UTCTime
defTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

-- | Second test Default Time ( == 0 seconds absolute)
afterDefTime :: UTCTime
afterDefTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 10)

-- | Default update interval
defUpdateEvery :: Int
defUpdateEvery = 60

feedInfoTable :: O.Table FeedInfoW FeedInfoR
feedInfoTable =
    O.Table
        "feed_info"
        (pFeedInfo
             FeedInfo
             { _feedInfoId = pFeedInfoId (FeedInfoId (O.optional "id"))
             , _feedInfoName = O.required "name"
             , _feedInfoUrl = O.required "url"
             , _feedInfoUpdateEvery = O.required "update_every"
             , _feedInfoLastUpdated = O.required "last_updated"
             , _feedHasItemDate = O.required "has_item_date"
             , _feedNumberItems = O.required "number_items"
             })

----------------------------
-- Feeds <-> Users (Subscriptions)
$(makeAdaptorAndInstance "pSubscription" ''Subscription)

type SubscriptionW = Subscription FeedInfoIdColumnW UserIdColumnW

type SubscriptionR = Subscription FeedInfoIdColumnR UserIdColumnR

subscriptionTable :: O.Table SubscriptionW SubscriptionR
subscriptionTable =
    O.Table
        "subscription"
        (pSubscription
             Subscription
             { _subscriptionFeedId = pFeedInfoId (FeedInfoId (O.required "feed_info_id"))
             , _subscriptionUserId = pUserId (UserId (O.required "user_id"))
             })

----------------------------
-- Feed items
$(makeAdaptorAndInstance "pFeedItem" ''FeedItem)

$(makeAdaptorAndInstance "pFeedItemId" ''FeedItemId)

type FeedItemW = FeedItem FeedItemIdColumnWO FeedInfoIdColumnW (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemR = FeedItem FeedItemIdColumnR FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemIdColumnWO = FeedItemId (Maybe (O.Column O.PGInt4))

type FeedItemIdColumnW = FeedItemId (O.Column O.PGInt4)

type FeedItemIdColumnR = FeedItemId (O.Column O.PGInt4)

-- | Default feed item
defFeedItem :: FeedItemHW
defFeedItem =
    FeedItem
    { _feedItemId = FeedItemId Nothing
    , _feedItemFeedId = FeedInfoId Nothing
    , _feedItemTitle = ""
    , _feedItemUrl = ""
    , _feedItemDate = defTime
    , _feedItemComments = Nothing
    }

feedItemTable :: O.Table FeedItemW FeedItemR
feedItemTable =
    O.Table
        "feed_item"
        (pFeedItem
             FeedItem
             { _feedItemId = pFeedItemId (FeedItemId (O.optional "id"))
             , _feedItemFeedId = pFeedInfoId (FeedInfoId (O.required "feed_info_id"))
             , _feedItemTitle = O.required "title"
             , _feedItemUrl = O.required "url"
             , _feedItemDate = O.required "pub_date"
             , _feedItemComments = O.required "comment_url"
             })

----------------------------
-- Unread items
$(makeAdaptorAndInstance "pUnreadItem" ''UnreadItem)

type UnreadItemW = UnreadItem FeedItemIdColumnW UserIdColumnW

type UnreadItemR = UnreadItem FeedItemIdColumnR UserIdColumnR

unreadItemTable :: O.Table UnreadItemW UnreadItemR
unreadItemTable =
    O.Table
        "unread_item"
        (pUnreadItem
             UnreadItem
             { _unreadFeedItemId = pFeedItemId (FeedItemId (O.required "feed_item_id"))
             , _unreadUserId = pUserId (UserId (O.required "user_id"))
             })

$(makeAdaptorAndInstance "pAuthToken" ''AuthToken)

type AuthTokenW = AuthToken UserIdColumnW (O.Column O.PGText)

type AuthTokenR = AuthToken UserIdColumnR (O.Column O.PGText)

authTokenTable :: O.Table AuthTokenW AuthTokenR
authTokenTable =
    O.Table
        "auth_token"
        (pAuthToken
             AuthToken
             { _authTokenHeedUserId = pUserId (UserId (O.required "user_id"))
             , _authTokenToken = O.required "token"
             })

data UserFeedInfoPref a b c = UserFeedInfoPref
    { _prefUserId :: a -- ^ Foreign key on UserId -- PGInt4
    , _prefFeedId :: b -- ^ Foreign key on FeedInfoId -- PGInt4
    , _prefName :: c -- ^ User Feed Name
    }

makeLenses ''UserFeedInfoPref

type UserFeedInfoPrefHR = UserFeedInfoPref (UserId Int) FeedInfoIdH Text

type UserFeedInfoPrefHW = UserFeedInfoPref (UserId Int) FeedInfoIdH Text

type UserFeedInfoPrefW = UserFeedInfoPref UserIdColumnW FeedInfoIdColumnW (O.Column O.PGText)

type UserFeedInfoPrefR = UserFeedInfoPref UserIdColumnR FeedInfoIdColumnR (O.Column O.PGText)

$(makeAdaptorAndInstance "pUserFeedInfoPref" ''UserFeedInfoPref)

userPrefTable :: O.Table UserFeedInfoPrefW UserFeedInfoPrefR
userPrefTable =
    O.Table
        "user_feed_info_pref"
        (pUserFeedInfoPref
             UserFeedInfoPref
             { _prefUserId = pUserId (UserId (O.required "user_id"))
             , _prefFeedId = pFeedInfoId (FeedInfoId (O.required "feed_info_id"))
             , _prefName = O.required "feed_info_name"
             })
