{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract where

import Control.Applicative ((<|>))
import Control.Arrow (returnA)
import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601
import Database.PostgreSQL.Simple as PG
import Heed.Database
import Heed.Types
import Network.HTTP.Client
import qualified Opaleye as O
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
    => a -> Url -> m ()
addFeed conf url = do
    let manager = getHttpManager conf
        dbConn = getDbConnection conf
    request <- liftHttpException InvalidUrl . parseRequest $ "GET " ++ T.unpack url
    feed <- liftHttpException DownloadFailed . liftIO $ responseBody <$> httpLbs request manager
    validFeed <- liftJust InvalidXML $ parseFeedSource feed
    now <- liftIO getCurrentTime
    (feedInfo, feedItems) <- liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed
    oldFeeds <- liftIO $ runFeedInfoQuery dbConn (thisFeed feedInfo)
    case length oldFeeds of
        0 -> do
            insertedFeed <- insertFeed dbConn feedInfo
            insertItems dbConn feedItems $ getFeedId insertedFeed
            return ()
        1 -> do
            insertItems dbConn feedItems $ getFeedId oldFeeds
            return ()
        _ -> throwError MultipleFeedsSameUrl
  where
    getFeedId = feedInfoId . head

insertFeed
    :: (MonadIO m)
    => PG.Connection -> FeedInfoH -> m [FeedInfoHR]
insertFeed conn newFeed =
    liftIO $ O.runInsertManyReturning conn feedInfoTable [feedInfoHToW newFeed] id

insertItems
    :: (MonadIO m)
    => PG.Connection -> [FeedItemH] -> FeedInfoId Int -> m ()
insertItems conn newItems feedId = do
    _ <- liftIO $ O.runInsertMany conn feedItemTable (feedItemHToW feedId <$> newItems)
    return ()

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoH, [FeedItemH])
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

atomEntryToItem :: UTCTime -> Atom.Entry -> FeedItemH
atomEntryToItem now entry =
    defFeedItem
    { feedItemTitle = T.pack . Atom.txtToString . Atom.entryTitle $ entry
    , feedItemUrl = T.pack . Safe.headDef "" $ (Atom.linkHref <$> Atom.entryLinks entry)
    , feedItemDate = fromMaybe now $ parseISO8601 (Atom.entryUpdated entry)
    }

rssEntryToItem :: UTCTime -> RSS.RSSItem -> FeedItemH
rssEntryToItem now entry =
    defFeedItem
    { feedItemTitle = T.pack . fromMaybe "No Title" . RSS.rssItemTitle $ entry
    , feedItemUrl = T.pack . fromMaybe "No Url" . RSS.rssItemLink $ entry
    , feedItemDate = fromMaybe now $ join (parseRfc822 <$> RSS.rssItemPubDate entry)
    , feedItemComments = T.pack <$> RSS.rssItemComments entry
    }

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat

thisFeed :: FeedInfoH -> O.Query FeedInfoR
thisFeed fresh =
    proc () ->
  do feeds <- O.queryTable feedInfoTable -< ()
     O.restrict -<
       feedInfoUrl feeds O..== O.pgStrictText (feedInfoUrl fresh)
     returnA -< feeds

liftHttpException
    :: (MonadCatch m, MonadError HeedError m)
    => HeedError -> m a -> m a
liftHttpException e action =
    action `catch`
    (\httpError ->
          let _ = httpError :: HttpException
          in throwError e)

liftJust
    :: (MonadError HeedError m)
    => HeedError -> Maybe a -> m a
liftJust e Nothing = throwError e
liftJust _ (Just a) = return a
