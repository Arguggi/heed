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

runUsersQuery :: PG.Connection -> O.Query (O.Column O.PGText) -> IO [T.Text]
runUsersQuery = O.runQuery

getUsers :: O.Query (O.Column O.PGText)
getUsers =
    proc () ->
  do users <- O.queryTable userTable -< ()
     returnA -< userName users

insertFeed
    :: (MonadIO m)
    => PG.Connection -> FeedInfoH -> m [FeedInfoHR]
insertFeed conn newFeed =
    liftIO $ O.runInsertManyReturning conn feedInfoTable [O.constant newFeed] id

insertItems
    :: (MonadIO m)
    => PG.Connection -> [FeedItemH] -> FeedInfoId Int -> m Int64
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

thisFeed :: FeedInfoH -> O.Query FeedInfoR
thisFeed fresh =
    proc () ->
  do feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -<
       feedInfoUrl feeds O..== O.pgStrictText (feedInfoUrl fresh)
     returnA -< feeds

runFeedInfoQuery :: PG.Connection -> O.Query FeedInfoR -> IO [FeedInfoHR]
runFeedInfoQuery = O.runQuery
