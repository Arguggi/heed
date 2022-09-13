{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Query
  ( addSubscription,
    allFeedInfo,
    allFeeds,
    allItemsRead,
    feedUpdateInterval,
    getRecentItems,
    getSubs,
    getUser,
    getUserFeeds,
    getUserItems,
    getUserUnreadFeedInfo,
    insertFeed,
    insertItems,
    insertUnread,
    insertUserPrefName,
    printSql,
    readFeed,
    saveTokenDb,
    showSql,
    setFeedLastUpdated,
    thisFeed,
    updateFeedInterval,
    updateUserPrefName,
    userFeedName,
    verifyToken,
  )
where

import Control.Arrow (returnA)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Heed.Commands
  ( FeFeedInfo' (..),
    FeFeedInfoR,
    FeItemInfo' (..),
    FeItemInfoR,
    feedListId,
    feedListName,
  )
import Heed.Database
  ( AuthToken (_authTokenHeedUserId, _authTokenToken),
    AuthTokenR,
    FeedInfo (..),
    FeedInfoHR,
    FeedInfoHW,
    FeedInfoId (..),
    FeedInfoIdColumnR,
    FeedInfoIdH,
    FeedInfoR,
    FeedItem
      ( _feedItemComments,
        _feedItemDate,
        _feedItemFeedId,
        _feedItemId,
        _feedItemTitle,
        _feedItemUrl
      ),
    FeedItemHR,
    FeedItemHW,
    FeedItemId (_getFeedItemId),
    FeedItemIdColumnR,
    FeedItemIdH,
    FeedItemR,
    Subscription
      ( Subscription,
        _subscriptionFeedId,
        _subscriptionUserId
      ),
    UnreadItem (UnreadItem, _unreadFeedItemId, _unreadUserId),
    User (_userId, _userName),
    UserFeedInfoPrefHW,
    UserFeedInfoPrefR,
    UserId,
    UserIdColumnR,
    UserR,
    authTokenTable,
    authTokenToken,
    feedInfoId,
    feedInfoName,
    feedInfoTable,
    feedInfoUpdateEvery,
    feedItemFeedId,
    feedItemTable,
    getFeedInfoId,
    prefFeedId,
    prefName,
    prefUserId,
    setTime,
    subscriptionTable,
    unreadItemTable,
    userPrefTable,
    userTable,
  )
import Heed.DbEnums (ItemsDate (..))
import Heed.Types (MonadDb (..), execSelect)
import Lens.Micro.Platform ((&), (.~), (^.))
import qualified Opaleye as O
import qualified Opaleye.FunctionalJoin as JOIN

showSql ::
  Default O.Unpackspec a a =>
  O.Select a ->
  String
showSql = fromMaybe "Empty query" . O.showSql

-- | Print a sql query, for debugging only
printSql ::
  (Default O.Unpackspec a a, MonadIO m) =>
  O.Select a ->
  m ()
printSql = liftIO . putStrLn . showSql

-- | Query used in 'getRecentItems'
getItemsFromQ :: FeedInfoId Int -> UTCTime -> O.Select FeedItemR
getItemsFromQ feedId from =
  proc () -> do
    items <- O.selectTable feedItemTable -< ()
    O.restrict
      -<
        _feedItemFeedId items O..=== O.toFields feedId
          O..&& _feedItemDate items
          O..>= O.sqlUTCTime from
    returnA -< items

-- | Query used in 'getRecentItems' when items don't have dates
-- We sort by descending time inserted and only return the last x rows
getLastItemsQ :: FeedInfoId Int -> Int -> O.Select FeedItemR
getLastItemsQ feedId number =
  O.limit number . O.orderBy (O.descNullsLast _feedItemDate) $
    proc () -> do
      items <- O.selectTable feedItemTable -< ()
      O.restrict -< _feedItemFeedId items O..=== O.toFields feedId
      returnA -< items

-- | Get items after a certain 'UTCTime'
getRecentItems ::
  -- | Feed information, we need to know if the feed has dates and if not
  -- how many items to fetch from the db
  FeedInfoHR ->
  -- | Start time if the feed has dates
  UTCTime ->
  -- | DbConnection
  O.Select FeedItemR
getRecentItems feed time =
  case _feedHasItemDate feed of
    Present -> getItemsFromQ (_feedInfoId feed) time
    -- Get last 200
    Missing -> getLastItemsQ (_feedInfoId feed) 200

-- | Insert new feed
insertFeed ::
  -- | Feed information
  FeedInfoHW ->
  -- | Connection
  O.Insert [FeedInfoHR]
insertFeed newFeed =
  O.Insert
    { O.iTable = feedInfoTable,
      O.iRows = [O.toFields newFeed],
      O.iReturning = O.rReturning id,
      O.iOnConflict = Nothing
    }

-- | Insert new items
insertItems ::
  -- | List of items
  [FeedItemHW] ->
  -- | Feed id
  FeedInfoId Int ->
  O.Insert [FeedItemHR]
insertItems newItems feedId =
  O.Insert
    { O.iTable = feedItemTable,
      O.iRows = O.toFields <$> (newItems & traverse . feedItemFeedId .~ feedId),
      O.iReturning = O.rReturning id,
      O.iOnConflict = Nothing
    }

-- | Update Feed last updated field, so we can schedule updates
setFeedLastUpdated ::
  -- | Feed id
  FeedInfoId Int ->
  -- | Updated 'UTCTime'
  UTCTime ->
  -- | Number of updated feeds, should always be 1
  O.Update Int64
setFeedLastUpdated feedId time =
  O.Update
    { O.uTable = feedInfoTable,
      O.uUpdateWith = setTime time,
      O.uWhere = \row -> _feedInfoId row O..=== O.toFields feedId,
      O.uReturning = O.rCount
    }

thisFeed :: FeedInfoHW -> O.Select FeedInfoR
thisFeed fresh =
  proc () -> do
    feeds <- O.selectTable feedInfoTable -< ()
    O.restrict
      -<
        _feedInfoUrl feeds O..== O.toFields (_feedInfoUrl fresh)
    returnA -< feeds

getUser :: T.Text -> O.Select UserR
getUser un =
  proc () -> do
    user <- O.selectTable userTable -< ()
    O.restrict -< _userName user O..=== O.toFields un
    returnA -< user

saveTokenDb :: T.Text -> UserId Int -> O.Update Int64
saveTokenDb token uid =
  O.Update
    { O.uTable = authTokenTable,
      O.uUpdateWith = (\x -> x & authTokenToken .~ O.sqlStrictText token),
      O.uWhere = filterUser uid,
      O.uReturning = O.rCount
    }

filterUser :: UserId Int -> AuthTokenR -> O.Field O.SqlBool
filterUser userid auth = _authTokenHeedUserId auth O..=== O.toFields userid

verifyToken :: T.Text -> O.Select UserR
verifyToken token = O.limit 1 (tokenToUser token)

tokenToUser :: T.Text -> O.Select UserR
tokenToUser token =
  proc () -> do
    users <- O.selectTable userTable -< ()
    tokens <- O.selectTable authTokenTable -< ()
    O.restrict
      -<
        _userId users O..=== _authTokenHeedUserId tokens
          O..&& (_authTokenToken tokens O..=== O.toFields token)
    returnA -< users

getUserFeedsQ :: UserId Int -> O.Select FeedInfoR
getUserFeedsQ userid =
  proc () -> do
    sub <- O.selectTable subscriptionTable -< ()
    feedWithCustomName <- myLeftJoin -< ()
    O.restrict -< _subscriptionUserId sub O..=== O.toFields userid
    O.restrict
      -<
        _feedInfoId feedWithCustomName O..=== _subscriptionFeedId sub
    returnA -< feedWithCustomName
  where
    myLeftJoin = JOIN.leftJoinF unite id joinOn (O.selectTable feedInfoTable) (getAllUserPref userid)
    unite info pref = info & feedInfoName .~ (pref ^. prefName)
    joinOn info pref = (info ^. feedInfoId) O..=== (pref ^. prefFeedId)

type FeedInfoIdGrouped = O.Field O.SqlInt4

type Count = O.Field O.SqlInt8

getUserUnreadItems :: UserId Int -> O.Select (FeedInfoIdGrouped, Count)
getUserUnreadItems userid =
  O.aggregate (p2 (O.groupBy, O.count)) $
    proc () -> do
      unread <- O.selectTable unreadItemTable -< ()
      item <- O.selectTable feedItemTable -< ()
      O.restrict -< _unreadUserId unread O..=== O.toFields userid
      O.restrict -< _feedItemId item O..=== _unreadFeedItemId unread
      returnA -< (item ^. feedItemFeedId . getFeedInfoId, O.sqlInt8 1)

-- Sort after in haskell land since we don't want to use sql's sorting function
getAllUserFeedInfo :: UserId Int -> O.Select FeFeedInfoR
getAllUserFeedInfo uid =
  proc () -> do
    allfeeds <- getUserFeedsQ uid -< ()
    (fIId, unreadCount) <- getUserUnreadItems uid -< ()
    let fIName = _feedInfoName allfeeds
    O.restrict -< fIId O..=== (_getFeedInfoId . _feedInfoId $ allfeeds)
    returnA -< FeFeedInfo' fIId fIName unreadCount

getAllUserPref :: UserId Int -> O.Select UserFeedInfoPrefR
getAllUserPref uid =
  proc () -> do
    pref <- O.selectTable userPrefTable -< ()
    O.restrict -< O.toFields uid O..=== (pref ^. prefUserId)
    returnA -< pref

getUserUnreadFeedInfo :: UserId Int -> O.Select FeFeedInfoR
getUserUnreadFeedInfo userid =
  JOIN.leftJoinF unite id joinOn (getAllUserFeedInfo userid) (getAllUserPref userid)
  where
    unite info pref = info & feedListName .~ (pref ^. prefName)
    joinOn info pref = (FeedInfoId $ info ^. feedListId) O..=== pref ^. prefFeedId

getUserFeeds :: UserId Int -> O.Select FeedInfoR
getUserFeeds uid = getUserFeedsQ uid

getFeedItemsIds :: FeedInfoId Int -> O.Select FeedItemIdColumnR
getFeedItemsIds fid =
  proc () -> do
    allItems <- O.selectTable feedItemTable -< ()
    O.restrict -< O.toFields fid O..=== _feedItemFeedId allItems
    returnA -< (_feedItemId allItems)

insertUnread :: [FeedItemHR] -> [UserId Int] -> O.Insert Int64
insertUnread newItems uids =
  O.Insert
    { O.iTable = unreadItemTable,
      O.iRows = O.toFields <$> pairings,
      O.iReturning = O.rCount,
      O.iOnConflict = Nothing
    }
  where
    pairings = do
      item <- newItems
      user <- uids
      return $ UnreadItem (_feedItemId item) user

addSubscription :: UserId Int -> FeedInfoId Int -> O.Insert Int64
addSubscription uid fid =
  O.Insert
    { O.iTable = subscriptionTable,
      O.iRows = [O.toFields $ Subscription fid uid],
      O.iReturning = O.rCount,
      O.iOnConflict = Nothing
    }

getUserItems :: UserId Int -> FeedInfoId Int -> O.Select FeItemInfoR
getUserItems uid fid = getUserItemsQ (O.toFields uid) (O.toFields fid)

getUserItemsQ :: UserIdColumnR -> FeedInfoIdColumnR -> O.Select FeItemInfoR
getUserItemsQ uid fid =
  O.orderBy (O.asc _itemInfoDate) $
    proc () -> do
      allItems <- O.selectTable feedItemTable -< ()
      allUnread <- O.selectTable unreadItemTable -< ()
      O.restrict -< uid O..=== _unreadUserId allUnread
      O.restrict -< fid O..=== _feedItemFeedId allItems
      O.restrict
        -<
          _unreadFeedItemId allUnread O..=== _feedItemId allItems
      let unreadItemId = _getFeedItemId . _feedItemId $ allItems
          unreadTitle = _feedItemTitle allItems
          unreadLink = _feedItemUrl allItems
          unreadDate = _feedItemDate allItems
          unreadComments = _feedItemComments allItems
      returnA
        -<
          FeItemInfo'
            unreadItemId
            unreadTitle
            unreadLink
            unreadDate
            unreadComments
            (O.sqlBool False)

readFeed :: UserId Int -> FeedItemId Int -> O.Delete Int64
readFeed userid itemid =
  O.Delete
    { O.dTable = unreadItemTable,
      O.dWhere = \cols ->
        (_unreadUserId cols O..=== O.toFields userid)
          O..&& (_unreadFeedItemId cols O..=== O.toFields itemid),
      O.dReturning = O.rCount
    }

allFeeds :: O.Select FeedInfoR
allFeeds = O.selectTable feedInfoTable

getSubs :: FeedInfoId Int -> O.Select UserIdColumnR
getSubs fid = proc () -> do
  allSubs <- O.selectTable subscriptionTable -< ()
  O.restrict -< O.toFields fid O..=== _subscriptionFeedId allSubs
  returnA -< _subscriptionUserId allSubs

allItemsRead :: MonadDb m => FeedInfoId Int -> UserId Int -> m Int64
allItemsRead fid uid = do
  itemsIds :: [FeedItemIdH] <- execSelect $ getFeedItemsIds fid
  let feedUnreadFilter (UnreadItem unreadIid unreadUid) =
        unreadUid O..=== O.toFields uid
          O..&& O.in_ (fmap (O.toFields . _getFeedItemId) itemsIds) (_getFeedItemId unreadIid)
   in execDelete
        O.Delete
          { O.dTable = unreadItemTable,
            O.dWhere = feedUnreadFilter,
            O.dReturning = O.rCount
          }

allFeedInfo :: FeedInfoIdH -> O.Select FeedInfoR
allFeedInfo fid = proc () -> do
  feed <- O.selectTable feedInfoTable -< ()
  O.restrict -< (feed ^. feedInfoId) O..=== O.toFields fid
  returnA -< feed

userFeedName :: FeedInfoIdH -> UserId Int -> O.Select UserFeedInfoPrefR
userFeedName fid userid = proc () -> do
  pref <- O.selectTable userPrefTable -< ()
  O.restrict
    -<
      ((pref ^. prefUserId) O..=== O.toFields userid)
        O..&& ((pref ^. prefFeedId) O..=== O.toFields fid)
  returnA -< pref

insertUserPrefName :: UserFeedInfoPrefHW -> O.Insert Int64
insertUserPrefName pref =
  O.Insert
    { O.iTable = userPrefTable,
      O.iRows = [O.toFields pref],
      O.iReturning = O.rCount,
      O.iOnConflict = Nothing
    }

updateUserPrefName :: UserFeedInfoPrefHW -> O.Update Int64
updateUserPrefName pref =
  O.Update
    { O.uTable = userPrefTable,
      O.uUpdateWith = (const (O.toFields pref)),
      O.uWhere = correctRow,
      O.uReturning = O.rCount
    }
  where
    correctRow row =
      row ^. prefUserId O..=== O.toFields (pref ^. prefUserId) O..&& row ^. prefFeedId
        O..=== O.toFields (pref ^. prefFeedId)

feedUpdateInterval :: FeedInfoIdH -> O.Select (O.Field O.SqlInt4)
feedUpdateInterval fid = _feedInfoUpdateEvery <$> allFeedInfo fid

updateFeedInterval :: FeedInfoIdH -> Int -> O.Update [FeedInfoHR]
updateFeedInterval fid interval =
  O.Update
    { O.uTable = feedInfoTable,
      O.uUpdateWith = \x -> x & feedInfoUpdateEvery .~ O.toFields interval & feedInfoId .~ O.toFields (Just <$> fid),
      O.uWhere = \row -> row ^. feedInfoId O..=== O.toFields fid,
      O.uReturning = O.rReturning (\x -> x :: FeedInfoR)
    }
