module Heed.Feed.Atom
    ( extractInfo
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Heed.Database
       (FeedInfoHW, FeedItemHW, Url, defFeedInfo, defFeedItem,
        _feedInfoLastUpdated, _feedInfoName, _feedInfoUpdateEvery,
        _feedInfoUrl, _feedItemDate, _feedItemTitle, _feedItemUrl)
import Heed.Feed.HtmlEntities (decodeHtmlEnt)
import qualified Safe
import qualified Text.Atom.Feed as Atom

extractInfo :: UTCTime -> Url -> Atom.Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfo now url feed = Just (feedInfo, feedItems)
  where
    feedInfo =
        defFeedInfo
        { _feedInfoName =
              T.strip . decodeHtmlEnt . T.pack . Atom.txtToString . Atom.feedTitle $ feed
        , _feedInfoUrl = url
        , _feedInfoUpdateEvery = 60
        , _feedInfoLastUpdated = fromMaybe now (parseISO8601 . Atom.feedUpdated $ feed)
        }
    feedItems = entryToItem now <$> Atom.feedEntries feed

entryToItem :: UTCTime -> Atom.Entry -> FeedItemHW
entryToItem now entry =
    defFeedItem
    { _feedItemTitle = decodeHtmlEnt . T.pack . Atom.txtToString . Atom.entryTitle $ entry
    , _feedItemUrl = T.pack . Safe.headDef "" $ (Atom.linkHref <$> Atom.entryLinks entry)
    , _feedItemDate = fromMaybe now $ parseISO8601 (Atom.entryUpdated entry)
    }
