{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Database definitions
module Heed.Database
  ( Url,

    -- * User info info
    User (..),
    UserH,
    UserW,
    UserR,
    userTable,

    -- ** User Lenses
    userId,
    userName,
    userPassword,
    userEmail,

    -- * UserId newtype
    UserId (..),
    UserIdColumnWO,
    UserIdColumnW,
    UserIdColumnR,

    -- ** UserId lenses
    getUserId,

    -- * Feed Info info
    FeedInfo (..),
    FeedInfoHW,
    FeedInfoHR,
    FeedInfoW,
    FeedInfoR,
    afterDefTime,
    defTime,
    setTime,
    defFeedInfo,
    defUpdateEvery,
    feedInfoTable,

    -- * Feed Info lenses
    feedInfoId,
    feedInfoName,
    feedInfoUrl,
    feedInfoUpdateEvery,
    feedInfoLastUpdated,
    feedHasItemDate,
    feedNumberItems,

    -- * FeedItem info
    FeedItem (..),
    FeedItemHR,
    FeedItemHW,
    FeedItemW,
    FeedItemR,

    -- ** FeedItem lenses
    feedItemId,
    feedItemFeedId,
    feedItemTitle,
    feedItemUrl,
    feedItemDate,
    feedItemComments,
    defFeedItem,
    feedItemTable,

    -- * FeedItemId newtype
    FeedItemId (..),
    FeedItemIdH,
    FeedItemIdColumnWO,
    FeedItemIdColumnW,
    FeedItemIdColumnR,

    -- ** FeedItemId lenses
    getFeedItemId,

    -- * FeedInfoId newtype
    FeedInfoId (..),
    FeedInfoIdH,
    FeedInfoIdColumnWO,
    FeedInfoIdColumnW,
    FeedInfoIdColumnR,

    -- ** FeedInfoId lenses
    getFeedInfoId,

    -- * UnreadItem info
    UnreadItem (..),
    UnreadItemW,
    UnreadItemR,
    unreadItemTable,

    -- ** UnreadItem lenses
    unreadFeedItemId,
    unreadUserId,

    -- * Authtoken info
    AuthToken (..),
    AuthTokenW,
    AuthTokenR,
    authTokenTable,

    -- ** Authtoken Lenses
    authTokenHeedUserId,
    authTokenToken,

    -- * Subscription info
    Subscription (..),
    subscriptionTable,

    -- ** Subscription lenses
    subscriptionFeedId,
    subscriptionUserId,

    -- * UserFeedInfoPref info
    UserFeedInfoPref (..),
    UserFeedInfoPrefHW,
    UserFeedInfoPrefHR,
    UserFeedInfoPrefW,
    UserFeedInfoPrefR,
    userPrefTable,

    -- ** UserFeedInfoPref lenses
    prefUserId,
    prefFeedId,
    prefName,
  )
where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import GHC.Generics (Generic)
import Heed.DbEnums (ItemsDate (Missing), PGItemsDate)
import Heed.Orphans ()
import Lens.Micro.Platform (makeLenses, (^.))
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
  { -- | PGInt4
    _userId :: a,
    -- | Text
    _userName :: b,
    -- | Bytestring
    _userPassword :: c,
    -- | Text
    _userEmail :: c
  }

makeLenses ''User

-- | For additional typesafety
newtype UserId a = UserId
  { _getUserId :: a
  }
  deriving (Functor)

makeLenses ''UserId

-- | User in Haskell Land
type UserH = User (UserId Int) Text Text Text

-- | PostgreSQL Feeds Table
data FeedInfo a b c d e f g = FeedInfo
  { -- | Autogenerated PGInt4
    _feedInfoId :: a,
    -- | Feed Name
    _feedInfoName :: b,
    -- | Feed url
    _feedInfoUrl :: c,
    -- | Update every minutes
    _feedInfoUpdateEvery :: d,
    -- | Last time the feed was downloaded and items added to the db
    _feedInfoLastUpdated :: e,
    -- | If the items in the feed have a publication date
    _feedHasItemDate :: f,
    -- | If no publication date is present we download the last
    -- x items to check so we can check which items in the feed are new
    _feedNumberItems :: g
  }
  deriving (Show, Generic)

makeLenses ''FeedInfo

instance
  (Eq b) =>
  Eq (FeedInfo a b c d e f g)
  where
  f1 == f2 = (f1 ^. feedInfoName) == (f2 ^. feedInfoName)

instance
  (Ord b) =>
  Ord (FeedInfo a b c d e f g)
  where
  f1 `compare` f2 = (f1 ^. feedInfoName) `compare` (f2 ^. feedInfoName)

-- | For additional typesafety
newtype FeedInfoId a = FeedInfoId
  { _getFeedInfoId :: a
  }
  deriving (Functor, Show, Generic, Eq, Ord)

makeLenses ''FeedInfoId

instance Serialize FeedInfoIdH

-- | 'FeedInfoId' Haskell
type FeedInfoIdH = FeedInfoId Int

-- | 'FeedInfo' Haskell read from DB
type FeedInfoHR = FeedInfo (FeedInfoId Int) Text Text Int UTCTime ItemsDate Int

-- | 'FeedInfo' Haskell write to DB (Id is missing)
type FeedInfoHW = FeedInfo (FeedInfoId (Maybe Int)) Text Text Int UTCTime ItemsDate Int

instance Serialize FeedInfoHR

instance
  (Serialize a) =>
  Serialize (FeedInfoId (Maybe a))

instance Serialize FeedInfoHW

-- | PostgreSQL Feeds <-> Users (Subscriptions) Table
data Subscription a b = Subscription
  { -- | Foreign key on FeedInfoId
    _subscriptionFeedId :: a,
    -- | Foreign key on UserId
    _subscriptionUserId :: b
  }

makeLenses ''Subscription

-- | PostgreSQL Feed items Table
data FeedItem a b c d e f = FeedItem
  { -- | Autogenerated PGInt4
    _feedItemId :: a,
    -- | Foreign Key to FeedInfoId PGInt4
    _feedItemFeedId :: b,
    -- | Item title Text
    _feedItemTitle :: c,
    -- | Item url Url
    _feedItemUrl :: d,
    -- | Item date
    _feedItemDate :: e,
    -- | Comment url if available (ala HN)
    _feedItemComments :: f
  }

makeLenses ''FeedItem

-- | For additional typesafety
newtype FeedItemId a = FeedItemId
  { _getFeedItemId :: a
  }
  deriving (Functor, Show, Generic)

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
  { -- | Foreign key on FeedItemId -- PGInt4
    _unreadFeedItemId :: a,
    -- | Foreign key on UserId -- PGInt4
    _unreadUserId :: b
  }

makeLenses ''UnreadItem

-- | PostgreSQL Authentication token table
data AuthToken a b = AuthToken
  { -- | Foreign key on UserId -- PGInt4
    _authTokenHeedUserId :: a,
    -- | Randomly generated token -- Text
    _authTokenToken :: b
  }

makeLenses ''AuthToken

$(makeAdaptorAndInstance "pUser" ''User)

$(makeAdaptorAndInstance "pUserId" ''UserId)

type UserW = User UserIdColumnWO (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlText)

type UserR = User UserIdColumnR (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlText)

type UserIdColumnWO = UserId (Maybe (O.Field O.SqlInt4))

type UserIdColumnW = UserId (O.Field O.SqlInt4)

type UserIdColumnR = UserId (O.Field O.SqlInt4)

userTable :: O.Table UserW UserR
userTable =
  O.table
    "heed_user"
    ( pUser
        User
          { _userId = pUserId (UserId (O.optionalTableField "id")),
            _userName = O.requiredTableField "username",
            _userPassword = O.requiredTableField "password",
            _userEmail = O.requiredTableField "email"
          }
    )

-----------------------------------------
-- Feeds Table
$(makeAdaptorAndInstance "pFeedInfo" ''FeedInfo)

$(makeAdaptorAndInstance "pFeedInfoId" ''FeedInfoId)

type FeedInfoW = FeedInfo FeedInfoIdColumnWO (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlInt4) (O.Field O.SqlTimestamptz) (O.Field PGItemsDate) (O.Field O.SqlInt4)

type FeedInfoR = FeedInfo FeedInfoIdColumnR (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlInt4) (O.Field O.SqlTimestamptz) (O.Field PGItemsDate) (O.Field O.SqlInt4)

type FeedInfoIdColumnWO = FeedInfoId (Maybe (O.Field O.SqlInt4))

type FeedInfoIdColumnW = FeedInfoId (O.Field O.SqlInt4)

type FeedInfoIdColumnR = FeedInfoId (O.Field O.SqlInt4)

-- Update timestamp on feed read from DB and make it writable to DB
setTime :: UTCTime -> FeedInfoR -> FeedInfoW
setTime utc feedInfo =
  feedInfo
    { _feedInfoId = FeedInfoId $ Just (_getFeedInfoId . _feedInfoId $ feedInfo),
      _feedInfoLastUpdated = O.sqlUTCTime utc
    }

-- Default 'FeedInfoHW'
defFeedInfo :: FeedInfoHW
defFeedInfo =
  FeedInfo
    { _feedInfoId = FeedInfoId Nothing,
      _feedInfoName = "",
      _feedInfoUrl = "",
      _feedInfoUpdateEvery = defUpdateEvery,
      _feedInfoLastUpdated = defTime,
      _feedHasItemDate = Missing,
      _feedNumberItems = 20
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
  O.table
    "feed_info"
    ( pFeedInfo
        FeedInfo
          { _feedInfoId = pFeedInfoId (FeedInfoId (O.optionalTableField "id")),
            _feedInfoName = O.requiredTableField "name",
            _feedInfoUrl = O.requiredTableField "url",
            _feedInfoUpdateEvery = O.requiredTableField "update_every",
            _feedInfoLastUpdated = O.requiredTableField "last_updated",
            _feedHasItemDate = O.requiredTableField "has_item_date",
            _feedNumberItems = O.requiredTableField "number_items"
          }
    )

----------------------------
-- Feeds <-> Users (Subscriptions)
$(makeAdaptorAndInstance "pSubscription" ''Subscription)

type SubscriptionW = Subscription FeedInfoIdColumnW UserIdColumnW

type SubscriptionR = Subscription FeedInfoIdColumnR UserIdColumnR

subscriptionTable :: O.Table SubscriptionW SubscriptionR
subscriptionTable =
  O.table
    "subscription"
    ( pSubscription
        Subscription
          { _subscriptionFeedId = pFeedInfoId (FeedInfoId (O.requiredTableField "feed_info_id")),
            _subscriptionUserId = pUserId (UserId (O.requiredTableField "user_id"))
          }
    )

----------------------------
-- Feed items
$(makeAdaptorAndInstance "pFeedItem" ''FeedItem)

$(makeAdaptorAndInstance "pFeedItemId" ''FeedItemId)

type FeedItemW = FeedItem FeedItemIdColumnWO FeedInfoIdColumnW (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlTimestamptz) (O.FieldNullable O.SqlText)

type FeedItemR = FeedItem FeedItemIdColumnR FeedInfoIdColumnR (O.Field O.SqlText) (O.Field O.SqlText) (O.Field O.SqlTimestamptz) (O.FieldNullable O.SqlText)

type FeedItemIdColumnWO = FeedItemId (Maybe (O.Field O.SqlInt4))

type FeedItemIdColumnW = FeedItemId (O.Field O.SqlInt4)

type FeedItemIdColumnR = FeedItemId (O.Field O.SqlInt4)

-- | Default feed item
defFeedItem :: FeedItemHW
defFeedItem =
  FeedItem
    { _feedItemId = FeedItemId Nothing,
      _feedItemFeedId = FeedInfoId Nothing,
      _feedItemTitle = "",
      _feedItemUrl = "",
      _feedItemDate = defTime,
      _feedItemComments = Nothing
    }

feedItemTable :: O.Table FeedItemW FeedItemR
feedItemTable =
  O.table
    "feed_item"
    ( pFeedItem
        FeedItem
          { _feedItemId = pFeedItemId (FeedItemId (O.optionalTableField "id")),
            _feedItemFeedId = pFeedInfoId (FeedInfoId (O.requiredTableField "feed_info_id")),
            _feedItemTitle = O.requiredTableField "title",
            _feedItemUrl = O.requiredTableField "url",
            _feedItemDate = O.requiredTableField "pub_date",
            _feedItemComments = O.requiredTableField "comment_url"
          }
    )

----------------------------
-- Unread items
$(makeAdaptorAndInstance "pUnreadItem" ''UnreadItem)

type UnreadItemW = UnreadItem FeedItemIdColumnW UserIdColumnW

type UnreadItemR = UnreadItem FeedItemIdColumnR UserIdColumnR

unreadItemTable :: O.Table UnreadItemW UnreadItemR
unreadItemTable =
  O.table
    "unread_item"
    ( pUnreadItem
        UnreadItem
          { _unreadFeedItemId = pFeedItemId (FeedItemId (O.requiredTableField "feed_item_id")),
            _unreadUserId = pUserId (UserId (O.requiredTableField "user_id"))
          }
    )

$(makeAdaptorAndInstance "pAuthToken" ''AuthToken)

type AuthTokenW = AuthToken UserIdColumnW (O.Field O.SqlText)

type AuthTokenR = AuthToken UserIdColumnR (O.Field O.SqlText)

authTokenTable :: O.Table AuthTokenW AuthTokenR
authTokenTable =
  O.table
    "auth_token"
    ( pAuthToken
        AuthToken
          { _authTokenHeedUserId = pUserId (UserId (O.requiredTableField "user_id")),
            _authTokenToken = O.requiredTableField "token"
          }
    )

data UserFeedInfoPref a b c = UserFeedInfoPref
  { -- | Foreign key on UserId -- PGInt4
    _prefUserId :: a,
    -- | Foreign key on FeedInfoId -- PGInt4
    _prefFeedId :: b,
    -- | User Feed Name
    _prefName :: c
  }

makeLenses ''UserFeedInfoPref

type UserFeedInfoPrefHR = UserFeedInfoPref (UserId Int) FeedInfoIdH Text

type UserFeedInfoPrefHW = UserFeedInfoPref (UserId Int) FeedInfoIdH Text

type UserFeedInfoPrefW = UserFeedInfoPref UserIdColumnW FeedInfoIdColumnW (O.Field O.SqlText)

type UserFeedInfoPrefR = UserFeedInfoPref UserIdColumnR FeedInfoIdColumnR (O.Field O.SqlText)

$(makeAdaptorAndInstance "pUserFeedInfoPref" ''UserFeedInfoPref)

userPrefTable :: O.Table UserFeedInfoPrefW UserFeedInfoPrefR
userPrefTable =
  O.table
    "user_feed_info_pref"
    ( pUserFeedInfoPref
        UserFeedInfoPref
          { _prefUserId = pUserId (UserId (O.requiredTableField "user_id")),
            _prefFeedId = pFeedInfoId (FeedInfoId (O.requiredTableField "feed_info_id")),
            _prefName = O.requiredTableField "feed_info_name"
          }
    )
