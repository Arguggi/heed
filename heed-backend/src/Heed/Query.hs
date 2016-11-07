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
    => PG.Connection -> [FeedItemHW] -> FeedInfoId Int -> m Int64
insertItems conn newItems feedId =
    liftIO $ O.runInsertMany conn feedItemTable $ O.constant <$> (setId feedId <$> newItems)
  where
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
    => PG.Connection -> T.Text -> m [UserH]
verifyToken conn token = liftIO $ O.runQuery conn (tokenToUser token)

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
