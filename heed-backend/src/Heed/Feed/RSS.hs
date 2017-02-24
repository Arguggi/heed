module Heed.Feed.RSS
    ( extractInfo
    ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Clock (UTCTime)
import qualified Heed.Database as DB
import Heed.Feed.HtmlEntities (decodeHtmlEnt)
import qualified Text.RSS.Syntax as RSS

extractInfo
    :: UTCTime -- ^ Current time
    -> DB.Url -- ^ 'DB.Url' of feed
    -> RSS.RSS -- ^ 'RSS.RSS' with information
    -> Maybe (DB.FeedInfoHW, [DB.FeedItemHW])
extractInfo now url feed = Just (feedInfo, feedItems)
  where
    channel = RSS.rssChannel feed
    feedInfo =
        DB.defFeedInfo
        { DB._feedInfoName = T.strip . decodeHtmlEnt . T.pack . RSS.rssTitle $ channel
        , DB._feedInfoUrl = url
        , DB._feedInfoUpdateEvery = 60
        , DB._feedInfoLastUpdated =
              fromMaybe now $
              join (parseRfc822 <$> (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel))
        }
    feedItems = rssEntryToItem now <$> RSS.rssItems channel

rssEntryToItem :: UTCTime -> RSS.RSSItem -> DB.FeedItemHW
rssEntryToItem now entry =
    DB.defFeedItem
    { DB._feedItemTitle = T.pack . fromMaybe "No Title" . RSS.rssItemTitle $ entry
    , DB._feedItemUrl = T.pack . fromMaybe "No Url" . RSS.rssItemLink $ entry
    , DB._feedItemDate = fromMaybe now $ join (parseRfc822 <$> RSS.rssItemPubDate entry)
    , DB._feedItemComments = T.pack <$> RSS.rssItemComments entry
    }

parseRfc822 :: String -> Maybe UTCTime
parseRfc822 = parseTimeM True defaultTimeLocale rfc822DateFormat
