{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Heed.Query where

import Control.Arrow (returnA)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Profunctor.Product (p2)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import Heed.Commands
import Heed.DbTypes
import Heed.Database
import qualified Opaleye as O

runUsersQuery
    :: (MonadIO m)
    => PG.Connection -> O.Query (O.Column O.PGText) -> m [T.Text]
runUsersQuery conn quer = liftIO $ O.runQuery conn quer

getUsers :: O.Query (O.Column O.PGText)
getUsers =
    proc () ->
  do users <- O.queryTable userTable -< ()
     returnA -< userName users

getItemsFrom :: FeedInfoId Int -> UTCTime -> O.Query FeedItemR
getItemsFrom (FeedInfoId feedId) from =
    proc () ->
  do items <- O.queryTable feedItemTable -< ()
     O.restrict -<
       (getFeedInfoId . feedItemFeedId $ items) O..== O.pgInt4 feedId
         O..&& (feedItemDate items O..>= O.pgUTCTime from)
     returnA -< items

getRecentItems
    :: (MonadIO m)
    => PG.Connection -> FeedInfoId Int -> UTCTime -> m [FeedItemHR]
getRecentItems conn feedId time = liftIO $ O.runQuery conn (getItemsFrom feedId time)

insertFeed
    :: (MonadIO m)
    => PG.Connection -> FeedInfoHW -> m [FeedInfoHR]
insertFeed conn newFeed =
    liftIO $ O.runInsertManyReturning conn feedInfoTable [O.constant newFeed] id

insertItems
    :: (MonadIO m)
    => PG.Connection -> [FeedItemHW] -> FeedInfoId Int -> m [FeedItemHR]
insertItems conn newItems feedId =
    liftIO $
    O.runInsertManyReturning conn feedItemTable (O.constant <$> (setId feedId <$> newItems)) id

setId
    :: forall a b c d e f b1.
       b -> FeedItem a b1 c d e f -> FeedItem a b c d e f
setId fid item =
    item
    { feedItemFeedId = fid
    }

setFeedLastUpdated
    :: (MonadIO m)
    => PG.Connection -> FeedInfoId Int -> UTCTime -> m Int64
setFeedLastUpdated conn (FeedInfoId feedId) time =
    liftIO $
    O.runUpdate
        conn
        feedInfoTable
        (setTime time)
        (\row -> (getFeedInfoId . feedInfoId $ row) O..== O.pgInt4 feedId)

thisFeed :: FeedInfoHW -> O.Query FeedInfoR
thisFeed fresh =
    proc () ->
  do feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -<
       feedInfoUrl feeds O..== O.pgStrictText (feedInfoUrl fresh)
     returnA -< feeds

runFeedInfoQuery :: PG.Connection -> O.Query FeedInfoR -> IO [FeedInfoHR]
runFeedInfoQuery = O.runQuery

getUser :: T.Text -> O.Query UserR
getUser un =
    proc () ->
  do user <- O.queryTable userTable -< ()
     O.restrict -< (userName user O..== O.pgStrictText un)
     returnA -< user

--
getUserDb
    :: (MonadIO m)
    => PG.Connection -> T.Text -> m (Maybe UserH)
getUserDb conn un =
    liftIO $
    do ids <- O.runQuery conn (getUser un)
       case length (ids :: [UserH]) of
           0 -> return Nothing
           _ -> return $ Just (head ids)

saveTokenDb
    :: (MonadIO m)
    => PG.Connection -> T.Text -> UserId Int -> m Int64
saveTokenDb conn token uid =
    liftIO $ O.runUpdate conn authTokenTable (setToken token) (filterUser uid)

setToken :: T.Text -> AuthTokenR -> AuthTokenW
setToken token auth =
    auth
    { authTokenToken = O.pgStrictText token
    }

filterUser :: UserId Int -> AuthTokenR -> O.Column O.PGBool
filterUser (UserId userid) auth = (getUserId . authTokenHeedUserId $ auth) O..== O.pgInt4 userid

verifyToken
    :: (MonadIO m)
    => PG.Connection -> T.Text -> m (Maybe UserH)
verifyToken conn token =
    liftIO $
    do user <- O.runQuery conn (tokenToUser token)
       return $ listToMaybe user

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
getUserUnreadItems (UserId userid) =
    O.aggregate (p2 (O.groupBy, O.count)) $
    proc () ->
  do unread <- O.queryTable unreadItemTable -< ()
     item <- O.queryTable feedItemTable -< ()
     O.restrict -< (getUserId . unreadUserId) unread O..== idCol
     O.restrict -<
       ((getFeedItemId . feedItemId) item O..==
          (getFeedItemId . unreadFeedItemId) unread)
     returnA -< ((getFeedInfoId . feedItemFeedId) item, O.pgInt8 1)
  where
    idCol = O.pgInt4 userid

getAllUserFeedInfo :: UserId Int -> O.Query ReactFeedInfoR
getAllUserFeedInfo uid =
    proc () ->
  do allfeeds <- O.orderBy (O.asc feedInfoName) $ getUserFeeds uid -<
                   ()
     allunread <- getUserUnreadItems uid -< ()
     let fIId = fst allunread
         fIName = feedInfoName allfeeds
         unreadCount = snd allunread
     O.restrict -< fIId O..== (getFeedInfoId . feedInfoId) allfeeds
     returnA -< ReactFeedInfo' fIId fIName unreadCount

getUserFeedInfo
    :: (MonadIO m)
    => PG.Connection -> UserId Int -> m [ReactFeedInfo]
getUserFeedInfo conn userid = liftIO $ O.runQuery conn $ getAllUserFeedInfo userid

insertUnread
    :: (MonadIO m)
    => PG.Connection -> [FeedItemHR] -> UserId Int -> m Int64
insertUnread conn newItems uid = liftIO $ O.runInsertMany conn unreadItemTable $ O.constant <$> pairings
  where
    pairings = map (\x -> (\item userid -> UnreadItem (feedItemId item) userid) x uid) newItems

addSubscription
    :: (MonadIO m)
    => PG.Connection -> UserId Int -> FeedInfoId Int -> m Int64
addSubscription conn uid fid =
    liftIO $ O.runInsertMany conn subscriptionTable [O.constant $ Subscription fid uid]
