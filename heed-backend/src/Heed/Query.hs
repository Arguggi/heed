{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Query where

import Control.Arrow (returnA)
import Control.Lens hiding (from, un)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Heed.Commands
       (FeFeedInfo, FeFeedInfo'(..), FeItemInfo, FeItemInfo'(..))
import Heed.Database
import Heed.DbEnums (ItemsDate(..))
import qualified Opaleye as O
import qualified Opaleye.Trans as OT

-- | Print a sql query, for debugging only
printSql
    :: Default O.Unpackspec a a
    => O.Query a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . O.showSqlForPostgres

-- | Query used in 'getRecentItems'
getItemsFromQ :: FeedInfoId Int -> UTCTime -> O.Query FeedItemR
getItemsFromQ feedId from =
    proc () ->
  do items <- O.queryTable feedItemTable -< ()
     O.restrict -<
       _feedItemFeedId items O..=== O.constant feedId O..&&
         _feedItemDate items
         O..>= O.pgUTCTime from
     returnA -< items

-- | Query used in 'getRecentItems' when items don't have dates
-- We sort by descending time inserted and only return the last x rows
getLastItemsQ :: FeedInfoId Int -> Int -> O.Query FeedItemR
getLastItemsQ feedId number =
    O.limit number . O.orderBy (O.descNullsLast _feedItemDate) $
    proc () ->
  do items <- O.queryTable feedItemTable -< ()
     O.restrict -< _feedItemFeedId items O..=== O.constant feedId
     returnA -< items

-- | Get items after a certain 'UTCTime'
getRecentItems
    :: FeedInfoHR -- ^ Feed information, we need to know if the feed has dates and if not
       -- how many items to fetch from the db
    -> UTCTime -- ^ Start time if the feed has dates
    -> OT.Transaction [FeedItemHR]
getRecentItems feed time =
    case _feedHasItemDate feed of
        Present -> OT.query $ getItemsFromQ (_feedInfoId feed) time
        Missing -> reverse <$> OT.query (getLastItemsQ (_feedInfoId feed) (_feedNumberItems feed))

-- | Insert new feed
insertFeed
    :: FeedInfoHW -- ^ Feed information
    -> OT.Transaction [FeedInfoHR]
insertFeed newFeed = OT.insertManyReturning feedInfoTable [O.constant newFeed] id

-- | Insert new items
insertItems
    :: [FeedItemHW] -- ^ List of items
    -> FeedInfoId Int -- ^ Feed id
    -> OT.Transaction [FeedItemHR]
insertItems newItems feedId =
    OT.insertManyReturning
        feedItemTable
        (O.constant <$> (newItems & traverse . feedItemFeedId .~ feedId))
        id

-- | Update Feed last updated field, so we can schedule updates
setFeedLastUpdated
    :: FeedInfoId Int -- ^ Feed id
    -> UTCTime -- ^ Updated 'UTCTime'
    -> OT.Transaction Int64 -- ^ Number of updated feeds, should always be 1
setFeedLastUpdated feedId time =
    OT.update feedInfoTable (setTime time) (\row -> _feedInfoId row O..=== O.constant feedId)

thisFeed :: FeedInfoHW -> O.Query FeedInfoR
thisFeed fresh =
    proc () ->
  do feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -<
       _feedInfoUrl feeds O..== O.constant (_feedInfoUrl fresh)
     returnA -< feeds

runFeedInfoQuery :: O.Query FeedInfoR -> OT.Transaction [FeedInfoHR]
runFeedInfoQuery = OT.query

getUser :: T.Text -> O.Query UserR
getUser un =
    proc () ->
  do user <- O.queryTable userTable -< ()
     O.restrict -< _userName user O..=== O.constant un
     returnA -< user

getUserDb :: T.Text -> OT.Transaction (Maybe UserH)
getUserDb un = OT.queryFirst (getUser un)

--saveTokenDb :: T.Text -> UserId Int -> OT.Transaction Int64
saveTokenDb :: T.Text -> UserId Int -> OT.Transaction Int64
--saveTokenDb token uid = OT.update authTokenTable (setToken token) (filterUser uid)
saveTokenDb token uid =
    OT.update authTokenTable (\x -> x & authTokenToken .~ O.pgStrictText token) (filterUser uid)

filterUser :: UserId Int -> AuthTokenR -> O.Column O.PGBool
filterUser userid auth = _authTokenHeedUserId auth O..=== O.constant userid

verifyToken :: T.Text -> OT.Transaction (Maybe UserH)
verifyToken token =
    if token == "invalid"
        then return Nothing
        else OT.queryFirst (tokenToUser token)

tokenToUser :: T.Text -> O.Query UserR
tokenToUser token =
    proc () ->
  do users <- O.queryTable userTable -< ()
     tokens <- O.queryTable authTokenTable -< ()
     O.restrict -<
       _userId users O..=== _authTokenHeedUserId tokens O..&&
         (_authTokenToken tokens O..=== O.constant token)
     returnA -< users

getUserFeedsQ :: UserId Int -> O.Query FeedInfoR
getUserFeedsQ userid =
    proc () ->
  do subs <- O.queryTable subscriptionTable -< ()
     feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -< _subscriptionUserId subs O..=== O.constant userid
     O.restrict -< _feedInfoId feeds O..=== _subscriptionFeedId subs
     returnA -< feeds

type FeedInfoIdGrouped = O.Column O.PGInt4

type Count = O.Column O.PGInt8

getUserUnreadItems :: UserId Int -> O.Query (FeedInfoIdGrouped, Count)
getUserUnreadItems userid =
    O.aggregate (p2 (O.groupBy, O.count)) $
    proc () ->
  do unread <- O.queryTable unreadItemTable -< ()
     item <- O.queryTable feedItemTable -< ()
     O.restrict -< _unreadUserId unread O..=== O.constant userid
     O.restrict -< _feedItemId item O..=== _unreadFeedItemId unread
     returnA -< (item ^. feedItemFeedId . getFeedInfoId, O.pgInt8 1)

-- Sort after in haskell land since we don't want to use sql's sorting function
getAllUserFeedInfo :: UserId Int -> O.Query FeFeedInfoR
getAllUserFeedInfo uid =
    proc () ->
  do allfeeds <- getUserFeedsQ uid -< ()
     (fIId, unreadCount) <- getUserUnreadItems uid -< ()
     let fIName = _feedInfoName allfeeds
     O.restrict -< fIId O..=== (_getFeedInfoId . _feedInfoId $ allfeeds)
     returnA -< FeFeedInfo' fIId fIName unreadCount

getUserUnreadFeedInfo :: UserId Int -> OT.Transaction [FeFeedInfo]
getUserUnreadFeedInfo userid = OT.query $ getAllUserFeedInfo userid

getUserFeeds :: UserId Int -> OT.Transaction [FeedInfoHR]
getUserFeeds uid = OT.query $ getUserFeedsQ uid

getFeedItemsIds :: FeedInfoId Int -> O.Query FeedItemIdColumnR
getFeedItemsIds fid =
    proc () ->
  do allItems <- O.queryTable feedItemTable -< ()
     O.restrict -< O.constant fid O..=== _feedItemFeedId allItems
     returnA -< (_feedItemId allItems)

insertUnread :: [FeedItemHR] -> [UserId Int] -> OT.Transaction Int64
insertUnread newItems uids = OT.insertMany unreadItemTable $ O.constant <$> pairings
  where
    pairings = do
        item <- newItems
        user <- uids
        return $ UnreadItem (_feedItemId item) user

addSubscription :: UserId Int -> FeedInfoId Int -> OT.Transaction Int64
addSubscription uid fid = OT.insertMany subscriptionTable [O.constant $ Subscription fid uid]

getUserItems :: UserId Int -> FeedInfoId Int -> OT.Transaction [FeItemInfo]
getUserItems uid fid = OT.query (getUserItemsQ (O.constant uid) (O.constant fid))

getUserItemsQ :: UserIdColumnR -> FeedInfoIdColumnR -> O.Query FeItemInfoR
getUserItemsQ uid fid =
    O.orderBy (O.asc _itemInfoDate) $
    proc () ->
  do allItems <- O.queryTable feedItemTable -< ()
     allUnread <- O.queryTable unreadItemTable -< ()
     O.restrict -< uid O..=== _unreadUserId allUnread
     O.restrict -< fid O..=== _feedItemFeedId allItems
     O.restrict -<
       _unreadFeedItemId allUnread O..=== _feedItemId allItems
     let unreadItemId = _getFeedItemId . _feedItemId $ allItems
         unreadTitle = _feedItemTitle allItems
         unreadLink = _feedItemUrl allItems
         unreadDate = _feedItemDate allItems
         unreadComments = _feedItemComments allItems
     returnA -<
       FeItemInfo' unreadItemId unreadTitle unreadLink unreadDate
         unreadComments
         (O.pgBool False)

readFeed :: UserId Int -> FeedItemId Int -> OT.Transaction Int64
readFeed userid itemid =
    OT.delete unreadItemTable $ \cols ->
        (_unreadUserId cols O..=== O.constant userid) O..&&
        (_unreadFeedItemId cols O..=== O.constant itemid)

allFeeds :: OT.Transaction [FeedInfoHR]
allFeeds = OT.query $ O.queryTable feedInfoTable

getSubs :: FeedInfoId Int -> OT.Transaction [UserId Int]
getSubs fid =
    OT.query $
    proc () ->
  do allSubs <- O.queryTable subscriptionTable -< ()
     O.restrict -< O.constant fid O..=== _subscriptionFeedId allSubs
     returnA -< _subscriptionUserId allSubs

allItemsRead :: FeedInfoId Int -> UserId Int -> OT.Transaction Int64
allItemsRead fid uid = do
    itemsIds :: [FeedItemIdH] <- OT.query $ getFeedItemsIds fid
    let feedUnreadFilter (UnreadItem unreadIid unreadUid) =
            unreadUid O..=== O.constant uid O..&&
            O.in_ (fmap (O.constant . _getFeedItemId) itemsIds) (_getFeedItemId unreadIid)
    OT.delete unreadItemTable feedUnreadFilter

allFeedInfo :: FeedInfoIdH -> OT.Transaction [FeedInfoHR]
allFeedInfo fid =
    OT.query $
    proc () ->
  do feed <- O.queryTable feedInfoTable -< ()
     O.restrict -< (feed ^. feedInfoId) O..=== O.constant fid
     returnA -< feed
