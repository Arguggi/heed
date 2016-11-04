{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Query where

import Control.Arrow (returnA)
import Control.Monad.IO.Class
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple as PG
import Heed.Database
import qualified Opaleye as O

runUsersQuery :: (MonadIO m) => PG.Connection -> O.Query (O.Column O.PGText) -> m [T.Text]
runUsersQuery conn quer = liftIO $ O.runQuery conn quer

getUsers :: O.Query (O.Column O.PGText)
getUsers =
    proc () ->
  do users <- O.queryTable userTable -< ()
     returnA -< userName users

getItemsFrom :: FeedInfoId Int -> UTCTime -> O.Query FeedItemR
getItemsFrom (FeedInfoId feedId) from = proc () -> do
    items <- O.queryTable feedItemTable -< ()
    O.restrict -<
      (getFeedInfoId . feedItemFeedId $ items) O..== O.pgInt4 feedId
      O..&& (feedItemDate items O..>= O.pgUTCTime from)
    returnA -< items

getRecentItems :: (MonadIO m) => PG.Connection -> FeedInfoId Int -> UTCTime -> m [FeedItemHR]
getRecentItems conn feedId time = liftIO $ O.runQuery conn (getItemsFrom feedId time)

insertFeed
    :: (MonadIO m)
    => PG.Connection -> FeedInfoHW -> m [FeedInfoHR]
insertFeed conn newFeed =
    liftIO $ O.runInsertManyReturning conn feedInfoTable [O.constant newFeed] id

insertItems
    :: (MonadIO m)
    => PG.Connection -> [FeedItemHW] -> FeedInfoId Int -> m Int64
insertItems conn newItems feedId =
    liftIO $ O.runInsertMany conn feedItemTable $ O.constant <$> (setId feedId <$> newItems)
    where setId fid item = item { feedItemFeedId = fid }

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
