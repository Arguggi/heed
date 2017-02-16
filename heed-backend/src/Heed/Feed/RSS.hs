module Heed.Feed.RSS
    ( extractInfo
    ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Clock (UTCTime)
import Heed.Database
       (FeedInfoHW, FeedItemHW, Url, defFeedInfo, defFeedItem,
        _feedInfoLastUpdated, _feedInfoName, _feedInfoUpdateEvery,
        _feedInfoUrl, _feedItemComments, _feedItemDate, _feedItemTitle,
        _feedItemUrl)
import Heed.Feed.HtmlEntities (decodeHtmlEnt)
import qualified Text.RSS.Syntax as RSS

extractInfo :: UTCTime -> Url -> RSS.RSS -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfo now url feed = Just (feedInfo, feedItems)
  where
    channel = RSS.rssChannel feed
    feedInfo =
        defFeedInfo
        { _feedInfoName = T.strip . decodeHtmlEnt . T.pack . RSS.rssTitle $ channel
        , _feedInfoUrl = url
        , _feedInfoUpdateEvery = 60
        , _feedInfoLastUpdated =
              fromMaybe now $
              join (parseRfc822 <$> (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel))
        }
    feedItems = rssEntryToItem now <$> RSS.rssItems channel

rssEntryToItem :: UTCTime -> RSS.RSSItem -> FeedItemHW
rssEntryToItem now entry =
    defFeedItem
    { _feedItemTitle = T.pack . fromMaybe "No Title" . RSS.rssItemTitle $ entry
    , _feedItemUrl = T.pack . fromMaybe "No Url" . RSS.rssItemLink $ entry
    , _feedItemDate = fromMaybe now $ join (parseRfc822 <$> RSS.rssItemPubDate entry)
    , _feedItemComments = T.pack <$> RSS.rssItemComments entry
    }

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat
