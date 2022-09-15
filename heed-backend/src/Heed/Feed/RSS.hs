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
import qualified Network.URI as URI
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
    baseUrl = RSS.rssLink channel
    feedInfo =
      DB.defFeedInfo
        { DB._feedInfoName = T.strip . decodeHtmlEnt . RSS.rssTitle $ channel,
          DB._feedInfoUrl = url,
          DB._feedInfoUpdateEvery = 60,
          DB._feedInfoLastUpdated =
            fromMaybe now $
              (parseRfc822 . T.unpack) =<< (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel)
        }
    feedItems = rssEntryToItem now baseUrl <$> RSS.rssItems channel

rssEntryToItem :: UTCTime -> T.Text -> RSS.RSSItem -> DB.FeedItemHW
rssEntryToItem now baseUrl entry =
  DB.defFeedItem
    { DB._feedItemTitle = T.strip . fromMaybe "No Title" . RSS.rssItemTitle $ entry,
      DB._feedItemUrl = itemUrl,
      DB._feedItemDate = fromMaybe now $ join (parseRfc822 . T.unpack <$> RSS.rssItemPubDate entry),
      DB._feedItemComments = RSS.rssItemComments entry
    }
  where
    itemUrl = fromMaybe "No Url" . fmap (buildUrl baseUrl . T.strip) . RSS.rssItemLink $ entry

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat

buildUrl :: T.Text -> T.Text -> T.Text
buildUrl baseUrl itemUrl =
  if URI.isURI itemUrlStr
    then itemUrl
    else (T.dropWhileEnd (== '/') baseUrl) <> "/" <> (T.dropWhile (== '/') itemUrl)
  where
    itemUrlStr = T.unpack itemUrl
