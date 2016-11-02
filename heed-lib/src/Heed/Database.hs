{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Database
  ( userTable
  , UserH
  , feedInfoTable
  , FeedInfoH
  , subscriptionTable
  , feedItemTable
  , unreadItemTable
  ) where

import Data.ByteString (ByteString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Opaleye as O

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

$(makeAdaptorAndInstance "pUser" ''User)

newtype UserId' a =
    UserId' a

$(makeAdaptorAndInstance "pUserId" ''UserId')

type UserW = User UserIdColumnWO (O.Column O.PGText) (O.Column O.PGBytea) (O.Column O.PGText)

type UserR = User UserIdColumnR (O.Column O.PGText) (O.Column O.PGBytea) (O.Column O.PGText)

type UserH = User (Maybe Int) Text ByteString Text

type UserIdColumnWO = UserId' (Maybe (O.Column O.PGInt4))

type UserIdColumnW = UserId' (O.Column O.PGInt4)

type UserIdColumnR = UserId' (O.Column O.PGInt4)

userTable :: O.Table UserW UserR
userTable =
    O.Table
        "User"
        (pUser
             User
             { userId = pUserId (UserId' (O.optional "id"))
             , userName = O.required "username"
             , userPassword = O.required "password"
             , userEmail = O.required "email"
             })

-----------------------------------------
-- Feeds Table
data FeedInfo a b c d = FeedInfo
    { feedInfoId :: a -- PGInt4
    , feedInfoName :: b -- Text
    , feedInfoUrl :: c -- Url
    , feedInfoUpdateEvery :: d -- Minutes
    }

$(makeAdaptorAndInstance "pFeedInfo" ''FeedInfo)

newtype FeedInfoId' a =
    FeedInfoId' a

$(makeAdaptorAndInstance "pFeedInfoId" ''FeedInfoId')

type FeedInfoW = FeedInfo FeedInfoIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4)

type FeedInfoR = FeedInfo FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4)

type FeedInfoH = FeedInfo (Maybe Int) Text Text Int

type FeedInfoIdColumnWO = FeedInfoId' (Maybe (O.Column O.PGInt4))

type FeedInfoIdColumnW = FeedInfoId' (O.Column O.PGInt4)

type FeedInfoIdColumnR = FeedInfoId' (O.Column O.PGInt4)

feedInfoTable :: O.Table FeedInfoW FeedInfoR
feedInfoTable =
    O.Table
        "FeedInfo"
        (pFeedInfo
             FeedInfo
             { feedInfoId = pFeedInfoId (FeedInfoId' (O.optional "id"))
             , feedInfoName = O.required "name"
             , feedInfoUrl = O.required "url"
             , feedInfoUpdateEvery = O.required "updateEvery"
             })

----------------------------
-- Feeds <-> Users (Subscriptions)
data Subscription a b = Subscription
    { subscriptionFeedId :: a -- PGInt4
    , subscriptionUserId :: b -- PGInt4
    }

$(makeAdaptorAndInstance "pSubscription" ''Subscription)

type SubscriptionW = Subscription FeedInfoIdColumnW UserIdColumnW

type SubscriptionR = Subscription FeedInfoIdColumnR UserIdColumnR

subscriptionTable :: O.Table SubscriptionW SubscriptionR
subscriptionTable =
    O.Table
        "Subscription"
        (pSubscription
             Subscription
             { subscriptionFeedId = pFeedInfoId (FeedInfoId' (O.required "feedInfoId"))
             , subscriptionUserId = pUserId (UserId' (O.required "userId"))
             })

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

$(makeAdaptorAndInstance "pFeedItem" ''FeedItem)

newtype FeedItemId' a =
    FeedItemId' a

$(makeAdaptorAndInstance "pFeedItemId" ''FeedItemId')

type FeedItemW = FeedItem FeedItemIdColumnWO FeedInfoIdColumnW (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemR = FeedItem FeedItemIdColumnR FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemIdColumnWO = FeedItemId' (Maybe (O.Column O.PGInt4))

type FeedItemIdColumnW = FeedItemId' (O.Column O.PGInt4)

type FeedItemIdColumnR = FeedItemId' (O.Column O.PGInt4)

feedItemTable :: O.Table FeedItemW FeedItemR
feedItemTable =
    O.Table
        "FeedItem"
        (pFeedItem
             FeedItem
             { feedItemId = pFeedItemId (FeedItemId' (O.optional "id"))
             , feedItemFeedId = pFeedInfoId (FeedInfoId' (O.required "feedId"))
             , feedItemTitle = O.required "title"
             , feedItemUrl = O.required "url"
             , feedItemDate = O.required "pubDate"
             , feedItemComments = O.required "commentUrl"
             })

----------------------------
-- Unread items
data UnreadItem a b = UnreadItem
    { unreadFeedItemId :: a -- PGInt4
    , unreadUserId :: b -- PGInt4
    }

$(makeAdaptorAndInstance "pUnreadItem" ''UnreadItem)

type UnreadItemW = UnreadItem FeedItemIdColumnW UserIdColumnW

type UnreadItemR = UnreadItem FeedItemIdColumnR UserIdColumnR

unreadItemTable :: O.Table UnreadItemW UnreadItemR
unreadItemTable =
    O.Table
        "UnreadItem"
        (pUnreadItem
             UnreadItem
             { unreadFeedItemId = pFeedItemId (FeedItemId' (O.required "feedItemId"))
             , unreadUserId = pUserId (UserId' (O.required "userId"))
             })
