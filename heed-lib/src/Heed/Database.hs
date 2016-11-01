{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Database
  ( userTable
  , feedTable
  , subscriptionTable
  , feedItemTable
  , unreadItemTable
  ) where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Opaleye as O

-- Users Table

data User a b c d = User
    { userId :: a -- PGInt4
    , userName :: b -- Text
    , userPassword :: c -- Bytestring
    , userEmail :: c -- Text
    }

$(makeAdaptorAndInstance "pUser" ''User)

type UserWrite = User (Maybe (O.Column O.PGInt4)) (O.Column O.PGText) (O.Column O.PGBytea) (O.Column O.PGText)
type UserRead = User (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGBytea) (O.Column O.PGText)

userTable :: O.Table UserWrite UserRead
userTable =
    O.Table
        "Users"
        (pUser
             User
             { userId = O.optional "id"
             , userName = O.required "username"
             , userPassword = O.required "password"
             , userEmail = O.required "email"
             })

-----------------------------------------
-- Feeds Table

data Feed a b c d e = Feed
    { feedId :: a -- PGInt4
    , feedName :: b -- Text
    , feedUrl :: c -- Url
    , feedAddedBy :: d -- User
    , feedUpdateEvery :: e -- Minutes
    }

$(makeAdaptorAndInstance "pFeed" ''Feed)

type FeedWrite = Feed (Maybe (O.Column O.PGInt4)) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGInt4)
type FeedRead = Feed (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGInt4)

feedTable :: O.Table FeedWrite FeedRead
feedTable =
    O.Table
        "Feeds"
        (pFeed
             Feed
             { feedId = O.optional "id"
             , feedName = O.required "name"
             , feedUrl = O.required "url"
             , feedAddedBy = O.required "AddedBy"
             , feedUpdateEvery = O.required "UpdateInterval"
             })

----------------------------
-- Feeds <-> Users (Subscriptions)

data Subscription a b = Subscription
    { subscriptionFeedId :: a -- PGInt4
    , subscriptionUserId :: b -- PGInt4
    }

$(makeAdaptorAndInstance "pSubscription" ''Subscription)

type SubscriptionWrite = Subscription (O.Column O.PGInt4) (O.Column O.PGInt4)
type SubscriptionRead = Subscription (O.Column O.PGInt4) (O.Column O.PGInt4)

subscriptionTable :: O.Table SubscriptionWrite SubscriptionRead
subscriptionTable =
    O.Table
        "Subscriptions"
        (pSubscription
             Subscription
             { subscriptionFeedId = O.required "feedId"
             , subscriptionUserId = O.required "UserId"
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

type FeedItemWrite = FeedItem (Maybe (O.Column O.PGInt4)) (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))
type FeedItemRead = FeedItem (O.Column O.PGInt4) (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

feedItemTable :: O.Table FeedItemWrite FeedItemRead
feedItemTable =
    O.Table
        "FeedItems"
        (pFeedItem
             FeedItem
             { feedItemId = O.optional "feedItemId"
             , feedItemFeedId = O.required "feedItemFeedId"
             , feedItemTitle = O.required "title"
             , feedItemUrl = O.required "url"
             , feedItemDate = O.required "date"
             , feedItemComments = O.required "commentUrl"
             })

----------------------------
-- Unread items

data UnreadItem a b = UnreadItem
    { unreadFeedItemId :: a -- PGInt4
    , unreadUserId :: b -- PGInt4
    }

$(makeAdaptorAndInstance "pUnreadItem" ''UnreadItem)

type UnreadItemWrite = UnreadItem (O.Column O.PGInt4) (O.Column O.PGInt4)
type UnreadItemRead = UnreadItem (O.Column O.PGInt4) (O.Column O.PGInt4)

unreadItemTable :: O.Table UnreadItemWrite UnreadItemRead
unreadItemTable =
    O.Table
        "UnreadItems"
        (pUnreadItem
             UnreadItem
             { unreadFeedItemId = O.required "feedItemId"
             , unreadUserId = O.required "userId"
             })
