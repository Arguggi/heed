{-# LANGUAGE OverloadedStrings #-}

module Heed.Feed.XML
  ( extractInfo,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import qualified Heed.Database as DB
import Safe (headDef)
import qualified Text.XML
import Text.XML.Cursor (Cursor, attribute, content, fromDocument, laxElement, ($/), (&//), (&|))

-- Tested with youtube xml format for the moment
extractInfo ::
  -- | Current time
  UTCTime ->
  -- | Feed 'DB.URL'
  DB.Url ->
  -- | Feed info
  Text.XML.Element ->
  Maybe (DB.FeedInfoHW, [DB.FeedItemHW])
extractInfo now url feed = if "yt:channel" `T.isInfixOf` feedId then Just (feedInfo, feedItems) else Nothing
  where
    cursor = Text.XML.Cursor.fromDocument (Text.XML.Document (Text.XML.Prologue [] Nothing []) feed [])
    feedId = headDef "" (cursor $/ laxElement "id" &// content)
    feedInfo =
      DB.defFeedInfo
        { DB._feedInfoName = headDef "Unknown Title" (cursor $/ laxElement "title" &// content),
          DB._feedInfoUrl = url,
          DB._feedInfoUpdateEvery = 60,
          DB._feedInfoLastUpdated = now
        }
    -- feedItems = entryToItem now <$> Atom.feedEntries feed
    feedItems = cursor $/ laxElement "entry" &| entryToItem now

entryToItem :: UTCTime -> Cursor -> DB.FeedItemHW
entryToItem now cursor =
  DB.defFeedItem
    { DB._feedItemTitle = headDef "Unknown title" (cursor $/ laxElement "title" &// content),
      DB._feedItemUrl = headDef "Unknown url" (cursor $/ laxElement "link" &// attribute "href"),
      DB._feedItemDate = fromMaybe now . parseISO8601 . T.unpack . headDef "" $ (cursor $/ laxElement "published" &// content)
    }
