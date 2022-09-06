{-# LANGUAGE OverloadedStrings #-}

module Heed.Feed.RSS
  ( extractInfo,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Clock (UTCTime)
import qualified Heed.Database as DB
import Heed.Feed.HtmlEntities (decodeHtmlEnt)
import qualified Text.RSS.Syntax as RSS

extractInfo ::
  -- | Current time
  UTCTime ->
  -- | 'DB.Url' of feed
  DB.Url ->
  -- | 'RSS.RSS' with information
  RSS.RSS ->
  Maybe (DB.FeedInfoHW, [DB.FeedItemHW])
extractInfo now url feed = Just (feedInfo, feedItems)
  where
    channel = RSS.rssChannel feed
    feedInfo =
      DB.defFeedInfo
        { DB._feedInfoName = T.strip . decodeHtmlEnt . RSS.rssTitle $ channel,
          DB._feedInfoUrl = url,
          DB._feedInfoUpdateEvery = 60,
          DB._feedInfoLastUpdated =
            fromMaybe now $
              (parseRfc822 . T.unpack) =<< (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel)
        }
    feedItems = rssEntryToItem now <$> RSS.rssItems channel

rssEntryToItem :: UTCTime -> RSS.RSSItem -> DB.FeedItemHW
rssEntryToItem now entry =
  DB.defFeedItem
    { DB._feedItemTitle = fromMaybe "No Title" . RSS.rssItemTitle $ entry,
      DB._feedItemUrl = fromMaybe "No Url" . RSS.rssItemLink $ entry,
      DB._feedItemDate = fromMaybe now $ join (parseRfc822 . T.unpack <$> RSS.rssItemPubDate entry),
      DB._feedItemComments = RSS.rssItemComments entry
    }

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat
