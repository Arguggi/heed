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

--( User(..)
--, UserH
--, userTable
--, defFeedInfo
--, FeedInfo(..)
--, FeedInfoId(..)
--, FeedInfoH
--, FeedInfoHR
--, FeedInfoR
--, feedInfoHToW
--, feedInfoTable
--, subscriptionTable
--, FeedItem(..)
--, FeedItemH
--, feedItemHToW
--, defFeedItem
--, feedItemTable
--, unreadItemTable
--, runFeedInfoQuery
--, defTime
--) where
import Data.ByteString (ByteString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Database.PostgreSQL.Simple as PG
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
    } deriving (Show)

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
        "heed_user"
        (pUser
             User
             { userId = pUserId (UserId' (O.optional "id"))
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

newtype FeedInfoId a =
    FeedInfoId a
    deriving (Functor)

$(makeAdaptorAndInstance "pFeedInfoId" ''FeedInfoId)

type FeedInfoW = FeedInfo FeedInfoIdColumnWO (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz)

type FeedInfoR = FeedInfo FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGInt4) (O.Column O.PGTimestamptz)

type FeedInfoHR = FeedInfo (FeedInfoId Int) Text Text Int UTCTime

type FeedInfoH = FeedInfo (Maybe (FeedInfoId Int)) Text Text Int UTCTime

type FeedInfoIdColumnWO = FeedInfoId (Maybe (O.Column O.PGInt4))

type FeedInfoIdColumnW = FeedInfoId (O.Column O.PGInt4)

type FeedInfoIdColumnR = FeedInfoId (O.Column O.PGInt4)

feedInfoHToW :: FeedInfoH -> FeedInfoW
feedInfoHToW FeedInfo {..} =
    FeedInfo
        (FeedInfoId Nothing)
        (O.pgStrictText feedInfoName)
        (O.pgStrictText feedInfoUrl)
        (O.pgInt4 feedInfoUpdateEvery)
        (O.pgUTCTime feedInfoLastUpdated)

defFeedInfo :: FeedInfoH
defFeedInfo =
    FeedInfo
    { feedInfoId = Nothing
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

newtype FeedItemId a =
    FeedItemId a

$(makeAdaptorAndInstance "pFeedItemId" ''FeedItemId)

type FeedItemW = FeedItem FeedItemIdColumnWO FeedInfoIdColumnW (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemR = FeedItem FeedItemIdColumnR FeedInfoIdColumnR (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText))

type FeedItemIdColumnWO = FeedItemId (Maybe (O.Column O.PGInt4))

type FeedItemIdColumnW = FeedItemId (O.Column O.PGInt4)

type FeedItemIdColumnR = FeedItemId (O.Column O.PGInt4)

type FeedItemH = FeedItem (Maybe Int) (Maybe Int) Text Url UTCTime (Maybe Url)

feedItemHToW :: FeedInfoId Int -> FeedItemH -> FeedItemW
feedItemHToW (FeedInfoId feedInfoId) FeedItem {..} =
    FeedItem
        (FeedItemId Nothing)
        (FeedInfoId (O.pgInt4 feedInfoId))
        (O.pgStrictText feedItemTitle)
        (O.pgStrictText feedItemUrl)
        (O.pgUTCTime feedItemDate)
        (O.maybeToNullable (O.pgStrictText <$> feedItemComments))

defFeedItem :: FeedItemH
defFeedItem =
    FeedItem
    { feedItemId = Nothing
    , feedItemFeedId = Nothing
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
             , unreadUserId = pUserId (UserId' (O.required "user_id"))
             })

--
--
--users <- runUsersQuery dbConnection getUsers
runFeedInfoQuery :: PG.Connection -> O.Query FeedInfoR -> IO [FeedInfoHR]
runFeedInfoQuery = O.runQuery
--getUsers :: O.Query
--getUsers =
--    proc () ->
--  do users <- O.queryTable userTable -< ()
--     returnA -< userName users
