{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.Except
import Data.List (deleteFirstsBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (compare)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time
import Data.Time.ISO8601
import Database.PostgreSQL.Simple as PG
import Heed.Database
import Heed.DbTypes
import Heed.Query
import Heed.Types
import Network.HTTP.Client
import qualified Safe
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types
import qualified Text.RSS.Syntax as RSS

addFeed
    :: (MonadThrow m
       ,MonadCatch m
       ,HasHttpManager a
       ,HasDbConnection a
       ,MonadIO m
       ,MonadError HeedError m)
    => a -> Url -> UserId Int -> m ()
addFeed conf url uid = do
    let manager = getHttpManager conf
        dbConn = getDbConnection conf
    request <- catchHttpException InvalidUrl . parseRequest $ "GET " ++ T.unpack url
    feed <- catchHttpException DownloadFailed . liftIO $ responseBody <$> httpLbs request manager
    validFeed <- liftJust InvalidXML . parseFeedSource . decodeUtf8With lenientDecode $ feed
    now <- liftIO getCurrentTime
    (feedInfo, feedItems) <- liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed
    oldFeeds <- liftIO $ runFeedInfoQuery dbConn (thisFeed feedInfo)
    case length oldFeeds of
        0 -> do
            insertedFeed <- insertFeed dbConn feedInfo
            insertedItems <- insertItems dbConn feedItems $ getFeedId insertedFeed
            _ <- insertUnread dbConn insertedItems uid
            _ <- addSubscription dbConn uid (getFeedId insertedFeed)
            return ()
        1 -> do
            let newFeedItems = setFeedId (getFeedId oldFeeds) <$> feedItems
                setFeedId fid hw =
                    hw
                    { feedItemFeedId = Just <$> fid
                    }
            recentItems <-
                getRecentItems
                    dbConn
                    (getFeedId oldFeeds)
                    (feedItemDate (minimumBy after feedItems))
            let newItems =
                    deleteFirstsBy
                        (sameItem (getFeedId oldFeeds))
                        newFeedItems
                        (applyJust <$> recentItems)
            _ <-
                if not (null newItems)
                    then do
                        insertedItems <- insertItems dbConn newFeedItems $ getFeedId oldFeeds
                        insertUnread dbConn insertedItems uid
                    else return 0
            _ <- setFeedLastUpdated dbConn (getFeedId oldFeeds) now
            return ()
        _ -> throwError MultipleFeedsSameUrl
  where
    getFeedId = feedInfoId . head
    after x y = compare (feedItemDate x) (feedItemDate y)
    sameItem feedId fromHttp fromDb =
        (getFeedInfoId . feedItemFeedId $ fromDb) == getFeedInfoId (Just <$> feedId) &&
        (feedItemTitle fromDb == feedItemTitle fromHttp) &&
        (feedItemUrl fromDb == feedItemUrl fromHttp)

applyJust :: FeedItemHR -> FeedItemHW
applyJust hr =
    hr
    { feedItemId = Just <$> feedItemId hr
    , feedItemFeedId = Just <$> feedItemFeedId hr
    }

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfoFromFeed now url (AtomFeed feed) = Just (feedInfo, feedItems)
  where
    feedInfo =
        defFeedInfo
        { feedInfoName = T.pack . Atom.txtToString . Atom.feedTitle $ feed
        , feedInfoUrl = url
        , feedInfoUpdateEvery = 60
        , feedInfoLastUpdated = fromMaybe now (parseISO8601 . Atom.feedUpdated $ feed)
        }
    feedItems = atomEntryToItem now <$> Atom.feedEntries feed
extractInfoFromFeed now url (RSSFeed feed) = Just (feedInfo, feedItems)
  where
    channel = RSS.rssChannel feed
    feedInfo =
        defFeedInfo
        { feedInfoName = T.pack . RSS.rssTitle $ channel
        , feedInfoUrl = url
        , feedInfoUpdateEvery = 60
        , feedInfoLastUpdated =
            fromMaybe now $
            join (parseRfc822 <$> (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel))
        }
    feedItems = rssEntryToItem now <$> RSS.rssItems channel
extractInfoFromFeed _ _ (RSS1Feed _) = error "RSS1 still todo"
extractInfoFromFeed _ _ (XMLFeed _) = Nothing

atomEntryToItem :: UTCTime -> Atom.Entry -> FeedItemHW
atomEntryToItem now entry =
    defFeedItem
    { feedItemTitle = T.pack . Atom.txtToString . Atom.entryTitle $ entry
    , feedItemUrl = T.pack . Safe.headDef "" $ (Atom.linkHref <$> Atom.entryLinks entry)
    , feedItemDate = fromMaybe now $ parseISO8601 (Atom.entryUpdated entry)
    }

rssEntryToItem :: UTCTime -> RSS.RSSItem -> FeedItemHW
rssEntryToItem now entry =
    defFeedItem
    { feedItemTitle = T.pack . fromMaybe "No Title" . RSS.rssItemTitle $ entry
    , feedItemUrl = T.pack . fromMaybe "No Url" . RSS.rssItemLink $ entry
    , feedItemDate = fromMaybe now $ join (parseRfc822 <$> RSS.rssItemPubDate entry)
    , feedItemComments = T.pack <$> RSS.rssItemComments entry
    }

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat

catchHttpException
    :: (MonadCatch m, MonadError HeedError m)
    => (HttpException -> HeedError) -> m a -> m a
catchHttpException excep action =
    action `catch`
    (\httpExcep ->
          let _ = httpExcep :: HttpException
          in throwError $ excep httpExcep)

catchSqlException
    :: (MonadCatch m, MonadError HeedError m)
    => (SqlError -> HeedError) -> m a -> m a
catchSqlException excep action =
    action `catch`
    (\sqlError ->
          let _ = sqlError :: SqlError
          in throwError $ excep sqlError)

liftJust
    :: (MonadError HeedError m)
    => HeedError -> Maybe a -> m a
liftJust e Nothing = throwError e
liftJust _ (Just a) = return a
