{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Heed.Query where

import Control.Arrow (returnA)
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text as T
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import Heed.Commands
import Heed.Database
import qualified Opaleye as O
import qualified Opaleye.Trans as OT

-- | Print a sql query, for debugging only
printSql
    :: Default O.Unpackspec a a
    => O.Query a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . O.showSqlForPostgres

-- | Run all queries in a transaction
runTransaction
    :: (MonadIO m)
    => PG.Connection -> OT.Transaction a -> m a
runTransaction conn trans = OT.runOpaleyeT conn $ OT.transaction trans

-- | Run all queries without starting a transaction
runQueryNoT
    :: (MonadIO m)
    => PG.Connection -> OT.Transaction a -> m a
runQueryNoT conn trans = OT.runOpaleyeT conn $ OT.run trans

-- | Query used in 'getRecentItems'
getItemsFromQuery :: FeedInfoId Int -> UTCTime -> O.Query FeedItemR
getItemsFromQuery (FeedInfoId feedId) from =
    proc () ->
  do items <- O.queryTable feedItemTable -< ()
     O.restrict -<
       ((getFeedInfoId . feedItemFeedId $ items) O..== O.pgInt4 feedId)
         O..&& (feedItemDate items O..>= O.pgUTCTime from)
     returnA -< items

-- | Get items after a certain 'UTCTime'
getRecentItems
    :: FeedInfoId Int -- ^ Feed Id
    -> UTCTime -- ^ Start time
    -> OT.Transaction [FeedItemHR]
getRecentItems feedId time = OT.query $ getItemsFromQuery feedId time

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
    OT.insertManyReturning feedItemTable (O.constant <$> (setId feedId <$> newItems)) id

-- | Set feed id of an item
setId
    :: b -- ^ Feed id
    -> FeedItem a b1 c d e f -- ^ Feed item
    -> FeedItem a b c d e f
setId fid item =
    item
    { feedItemFeedId = fid
    }

-- | Update Feed last updated field, so we can schedule updates
setFeedLastUpdated
    :: FeedInfoId Int -- ^ Feed id
    -> UTCTime -- ^ Updated 'UTCTime'
    -> OT.Transaction Int64 -- ^ Number of updated feeds, should always be 1
setFeedLastUpdated feedId time =
    OT.update feedInfoTable (setTime time) (\row -> feedInfoId row O..=== O.constant feedId)

thisFeed :: FeedInfoHW -> O.Query FeedInfoR
thisFeed fresh =
    proc () ->
  do feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -<
       feedInfoUrl feeds O..== O.pgStrictText (feedInfoUrl fresh)
     returnA -< feeds

runFeedInfoQuery :: O.Query FeedInfoR -> OT.Transaction [FeedInfoHR]
runFeedInfoQuery = OT.query

getUser :: T.Text -> O.Query UserR
getUser un =
    proc () ->
  do user <- O.queryTable userTable -< ()
     O.restrict -< (userName user O..== O.pgStrictText un)
     returnA -< user

getUserDb :: T.Text -> OT.Transaction (Maybe UserH)
getUserDb un = OT.queryFirst (getUser un)

saveTokenDb :: T.Text -> UserId Int -> OT.Transaction Int64
saveTokenDb token uid = OT.update authTokenTable (setToken token) (filterUser uid)

setToken :: T.Text -> AuthTokenR -> AuthTokenW
setToken token auth =
    auth
    { authTokenToken = O.pgStrictText token
    }

filterUser :: UserId Int -> AuthTokenR -> O.Column O.PGBool
filterUser (UserId userid) auth = (getUserId . authTokenHeedUserId $ auth) O..== O.pgInt4 userid

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
       ((getUserId . userId $ users) O..==
          (getUserId . authTokenHeedUserId $ tokens))
         O..&& (authTokenToken tokens O..== O.pgStrictText token)
     returnA -< users

getUserFeeds :: UserId Int -> O.Query FeedInfoR
getUserFeeds (UserId userid) =
    O.orderBy (O.asc feedInfoName) $
    proc () ->
  do subs <- O.queryTable subscriptionTable -< ()
     feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -< ((getUserId . subscriptionUserId) subs O..== idCol)
     O.restrict -<
       ((getFeedInfoId . feedInfoId) feeds O..==
          (getFeedInfoId . subscriptionFeedId) subs)
     returnA -< feeds
  where
    idCol = O.pgInt4 userid

getUserUnreadItems :: UserId Int -> O.Query (O.Column O.PGInt4, O.Column O.PGInt8)
getUserUnreadItems userid =
    O.aggregate (p2 (O.groupBy, O.count)) $
    proc () ->
  do unread <- O.queryTable unreadItemTable -< ()
     item <- O.queryTable feedItemTable -< ()
     O.restrict -< unreadUserId unread O..=== O.constant userid
     O.restrict -< feedItemId item O..=== unreadFeedItemId unread
     returnA -< ((getFeedInfoId . feedItemFeedId) item, O.pgInt8 1)

getAllUserFeedInfo :: UserId Int -> O.Query FeFeedInfoR
getAllUserFeedInfo uid =
    proc () ->
  do allfeeds <- O.orderBy (O.asc feedInfoName) $ getUserFeeds uid -<
                   ()
     allunread <- getUserUnreadItems uid -< ()
     let fIId = fst allunread
         fIName = feedInfoName allfeeds
         unreadCount = snd allunread
     O.restrict -< fIId O..== (getFeedInfoId . feedInfoId) allfeeds
     returnA -< FeFeedInfo' fIId fIName unreadCount

getUserFeedInfo :: UserId Int -> OT.Transaction [FeFeedInfo]
getUserFeedInfo userid = OT.query $ getAllUserFeedInfo userid

insertUnread :: [FeedItemHR] -> [UserId Int] -> OT.Transaction Int64
insertUnread newItems uids = OT.insertMany unreadItemTable $ O.constant <$> pairings
  where
    pairings = do
        item <- newItems
        user <- uids
        return $ UnreadItem (feedItemId item) user

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
     O.restrict -< uid O..=== unreadUserId allUnread
     O.restrict -< fid O..=== feedItemFeedId allItems
     O.restrict -< unreadFeedItemId allUnread O..=== feedItemId allItems
     let unreadItemId = getFeedItemId . feedItemId $ allItems
         unreadTitle = feedItemTitle allItems
         unreadLink = feedItemUrl allItems
         unreadDate = feedItemDate allItems
         unreadComments = feedItemComments allItems
     returnA -<
       FeItemInfo' unreadItemId unreadTitle unreadLink unreadDate
         unreadComments
         (O.pgBool False)

readFeed :: UserId Int -> FeedItemId Int -> OT.Transaction Int64
readFeed userid itemid =
    OT.delete unreadItemTable $
    \cols ->
         (unreadUserId cols O..=== O.constant userid) O..&&
         (unreadFeedItemId cols O..=== O.constant itemid)

allFeeds :: OT.Transaction [FeedInfoHR]
allFeeds = OT.query $ O.queryTable feedInfoTable

getSubs :: FeedInfoId Int -> OT.Transaction [UserId Int]
getSubs fid =
    OT.query $
    proc () ->
  do allSubs <- O.queryTable subscriptionTable -< ()
     O.restrict -< O.constant fid O..=== subscriptionFeedId allSubs
     returnA -< subscriptionUserId allSubs
