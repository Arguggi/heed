module Heed.Feed.Atom
    ( extractInfo
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import qualified Heed.Database as DB
import Heed.Feed.HtmlEntities (decodeHtmlEnt)
import qualified Safe
import qualified Text.Atom.Feed as Atom

extractInfo :: UTCTime -> DB.Url -> Atom.Feed -> Maybe (DB.FeedInfoHW, [DB.FeedItemHW])
extractInfo now url feed = Just (feedInfo, feedItems)
  where
    feedInfo =
        DB.defFeedInfo
        { DB._feedInfoName =
              T.strip . decodeHtmlEnt . T.pack . Atom.txtToString . Atom.feedTitle $ feed
        , DB._feedInfoUrl = url
        , DB._feedInfoUpdateEvery = 60
        , DB._feedInfoLastUpdated = fromMaybe now (parseISO8601 . Atom.feedUpdated $ feed)
        }
    feedItems = entryToItem now <$> Atom.feedEntries feed

entryToItem :: UTCTime -> Atom.Entry -> DB.FeedItemHW
entryToItem now entry =
    DB.defFeedItem
    { DB._feedItemTitle = decodeHtmlEnt . T.pack . Atom.txtToString . Atom.entryTitle $ entry
    , DB._feedItemUrl = T.pack . Safe.headDef "" $ (Atom.linkHref <$> Atom.entryLinks entry)
    , DB._feedItemDate = fromMaybe now $ parseISO8601 (Atom.entryUpdated entry)
    }
