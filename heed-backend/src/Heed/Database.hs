{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heed.Database where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified Opaleye as O

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

$(makeAdaptorAndInstance "pUser" ''User)

newtype UserId a = UserId
    { getUserId :: a
    } deriving (Functor)

$(makeAdaptorAndInstance "pUserId" ''UserId)

type UserW = User UserIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGText)

type UserR = User UserIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGText)

type UserH = User (UserId Int) Text Text Text

type UserIdColumnWO = UserId (Maybe (O.Column O.PGInt4))

type UserIdColumnW = UserId (O.Column O.PGInt4)

type UserIdColumnR = UserId (O.Column O.PGInt4)

userTable :: O.Table UserW UserR
userTable =
    O.Table
        "heed_user"
        (pUser
             User
             { userId = pUserId (UserId (O.optional "id"))
             , userName = O.required "username"
             , userPassword = O.required "password"
             , userEmail = O.required "email"
             })

-----------------------------------------
-- Feeds Table
data FeedInfo a b c d e = FeedInfo
    { feedInfoId :: a -- PGInt4
    , feedInfoName :: b -- Text
    , feedInfoUrl :: c -- Url
    , feedInfoUpdateEvery :: d -- Minutes
    , feedInfoLastUpdated :: e -- timestamp
    }

$(makeAdaptorAndInstance "pFeedInfo" ''FeedInfo)

newtype FeedInfoId a = FeedInfoId
    { getFeedInfoId :: a
    } deriving (Functor)

$(makeAdaptorAndInstance "pFeedInfoId" ''FeedInfoId)

type FeedInfoW = FeedInfo FeedInfoIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz)

type FeedInfoR = FeedInfo FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz)

type FeedInfoHR = FeedInfo (FeedInfoId Int) Text Text Int UTCTime

type FeedInfoHW = FeedInfo (FeedInfoId (Maybe Int)) Text Text Int UTCTime

type FeedInfoIdColumnWO = FeedInfoId (Maybe (O.Column O.PGInt4))

type FeedInfoIdColumnW = FeedInfoId (O.Column O.PGInt4)

type FeedInfoIdColumnR = FeedInfoId (O.Column O.PGInt4)

setTime :: UTCTime -> FeedInfoR -> FeedInfoW
setTime utc feedInfo =
    feedInfo
    { feedInfoId = FeedInfoId $ Just (getFeedInfoId . feedInfoId $ feedInfo)
    , feedInfoLastUpdated = O.pgUTCTime utc
    }

defFeedInfo :: FeedInfoHW
defFeedInfo =
    FeedInfo
    { feedInfoId = FeedInfoId Nothing
    , feedInfoName = ""
    , feedInfoUrl = ""
    , feedInfoUpdateEvery = 60
    , feedInfoLastUpdated = defTime
    }

defTime :: UTCTime
defTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

feedInfoTable :: O.Table FeedInfoW FeedInfoR
feedInfoTable =
    O.Table
        "feed_info"
        (pFeedInfo
             FeedInfo
             { feedInfoId = pFeedInfoId (FeedInfoId (O.optional "id"))
             , feedInfoName = O.required "name"
             , feedInfoUrl = O.required "url"
             , feedInfoUpdateEvery = O.required "update_every"
             , feedInfoLastUpdated = O.required "last_updated"
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
             { subscriptionFeedId = pFeedInfoId (FeedInfoId (O.required "feedInfoId"))
             , subscriptionUserId = pUserId (UserId (O.required "userId"))
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

newtype FeedItemId a =
    FeedItemId a
    deriving (Eq, Functor)

$(makeAdaptorAndInstance "pFeedItemId" ''FeedItemId)

type FeedItemW = FeedItem FeedItemIdColumnWO FeedInfoIdColumnW (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemR = FeedItem FeedItemIdColumnR FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemIdColumnWO = FeedItemId (Maybe (O.Column O.PGInt4))

type FeedItemIdColumnW = FeedItemId (O.Column O.PGInt4)

type FeedItemIdColumnR = FeedItemId (O.Column O.PGInt4)

type FeedItemHW = FeedItem (FeedItemId (Maybe Int)) (FeedInfoId (Maybe Int)) Text Url UTCTime (Maybe Url)

type FeedItemHR = FeedItem (FeedItemId Int) (FeedInfoId Int) Text Url UTCTime (Maybe Url)

defFeedItem :: FeedItemHW
defFeedItem =
    FeedItem
    { feedItemId = FeedItemId Nothing
    , feedItemFeedId = FeedInfoId Nothing
    , feedItemTitle = ""
    , feedItemUrl = ""
    , feedItemDate = defTime
    , feedItemComments = Nothing
    }

feedItemTable :: O.Table FeedItemW FeedItemR
feedItemTable =
    O.Table
        "feed_item"
        (pFeedItem
             FeedItem
             { feedItemId = pFeedItemId (FeedItemId (O.optional "id"))
             , feedItemFeedId = pFeedInfoId (FeedInfoId (O.required "feed_info_id"))
             , feedItemTitle = O.required "title"
             , feedItemUrl = O.required "url"
             , feedItemDate = O.required "pub_date"
             , feedItemComments = O.required "comment_url"
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
        "unread_item"
        (pUnreadItem
             UnreadItem
             { unreadFeedItemId = pFeedItemId (FeedItemId (O.required "feed_item_id"))
             , unreadUserId = pUserId (UserId (O.required "user_id"))
             })

data AuthToken a b = AuthToken
    { authTokenHeedUserId :: a
    , authTokenToken :: b
    }

$(makeAdaptorAndInstance "pAuthToken" ''AuthToken)

type AuthTokenW = AuthToken UserIdColumnW (O.Column O.PGText)

type AuthTokenR = AuthToken UserIdColumnR (O.Column O.PGText)

authTokenTable :: O.Table AuthTokenW AuthTokenR
authTokenTable =
    O.Table
        "auth_token"
        (pAuthToken
             AuthToken
             { authTokenHeedUserId = pUserId (UserId (O.required "user_id"))
             , authTokenToken = O.required "token"
             })
