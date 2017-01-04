{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.List (deleteFirstsBy, minimumBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Ord (compare)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time
import Data.Time.ISO8601
import Heed.Database
import Heed.Query
import Heed.Types
import qualified Safe
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Match as TS
import qualified Text.RSS.Syntax as RSS

addFeed
    :: (MonadStdOut m, MonadHttp m, MonadParse m, MonadDb m, MonadTime m)
    => Url -> UserId Int -> m ()
addFeed url uid = do
    stdOut $ "Adding: " <> url
    feed <- downloadUrl url
    (feedInfo, feedItems) <- parseFeed feed url
    oldFeeds <- execQuery $ runFeedInfoQuery (thisFeed feedInfo)
    case listToMaybe oldFeeds
         -- We don't have this feed in the common database, insert it
          of
        Nothing -> addNewFeed feedInfo feedItems uid
        -- This feed is already present in the database, add the user to the subscription
        Just oldFeed -> do
            now <- getTime
            updateFeedItems oldFeed feedItems uid now

addNewFeed
    :: MonadDb m
    => FeedInfoHW -> [FeedItemHW] -> UserId Int -> m ()
addNewFeed feedInfo feedItems uid =
    execQuery $
    do insertedFeed <- insertFeed feedInfo
       let newFeedId = feedInfoId . head $ insertedFeed
       insertedItems <- insertItems feedItems newFeedId
       _ <- insertUnread insertedItems uid
       _ <- addSubscription uid newFeedId
       return ()

updateFeedItems
    :: MonadDb m
    => FeedInfoHR -> [FeedItemHW] -> UserId Int -> UTCTime -> m ()
updateFeedItems oldFeed feedItems uid now =
    execQuery $
    do let newFeedItems = setFeedId (feedInfoId oldFeed) <$> feedItems
           oldFeedId = feedInfoId oldFeed
       recentItems <- getRecentItems oldFeedId (feedItemDate (minimumBy after feedItems))
       let newItems = deleteFirstsBy (sameItem oldFeedId) newFeedItems (applyJust <$> recentItems)
       _ <-
           if not (null newItems)
               then do
                   insertedItems <- insertItems newItems oldFeedId
                   insertUnread insertedItems uid
               else return 0
       _ <- setFeedLastUpdated oldFeedId now
       return ()
  where
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

setFeedId
    :: Functor f1
    => f1 a1 -> FeedItem a b c d e f -> FeedItem a (f1 (Maybe a1)) c d e f
setFeedId fid hw =
    hw
    { feedItemFeedId = Just <$> fid
    }

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfoFromFeed now url (AtomFeed feed) = Just (feedInfo, feedItems)
  where
    feedInfo =
        defFeedInfo
        { feedInfoName = T.strip . T.pack . Atom.txtToString . Atom.feedTitle $ feed
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
        { feedInfoName = T.strip . T.pack . RSS.rssTitle $ channel
        , feedInfoUrl = url
        , feedInfoUpdateEvery = 60
        , feedInfoLastUpdated =
            fromMaybe now $
            join (parseRfc822 <$> (RSS.rssPubDate channel <|> RSS.rssLastUpdate channel))
        }
    feedItems = rssEntryToItem now <$> RSS.rssItems channel
-- TODO
extractInfoFromFeed _ _ (RSS1Feed _) = Nothing
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

importOPML
    :: (MonadOpml m, MonadStdOut m, MonadReader BackendConf m, MonadIO m)
    => T.Text -> UserId Int -> m ()
importOPML opml userid = do
    feeds <- parseOpml opml parseTTRssOPML
    conf <- ask
    forM_ feeds $
        \url -> do
            result <- runBe conf $ addFeed url userid
            case result of
                Left e -> do
                    stdOut $ "Failed to add: " <> url
                    stdOut . T.pack . show $ e
                Right _ -> stdOut $ "Added: " <> url
            return ()
    return ()

parseTTRssOPML :: T.Text -> Maybe [T.Text]
parseTTRssOPML opml = maybeFeeds
  where
    parsedTags = TS.parseTags opml
    dataTags = ttRssTags parsedTags
    urls = getFeedUrl <$> dataTags
    maybeFeeds =
        case urls of
            [] -> Nothing
            a -> Just a

ttRssTags :: [TS.Tag T.Text] -> [TS.Tag T.Text]
ttRssTags = filter $ TS.tagOpenLit "outline" matchAllAttr

matchAllAttr :: [TS.Attribute T.Text] -> Bool
matchAllAttr attrs = all (`TS.anyAttrNameLit` attrs) ttRssAttrNames

ttRssAttrNames :: [T.Text]
ttRssAttrNames = ["type", "text", "xmlUrl"]

getFeedUrl :: TS.Tag T.Text -> T.Text
getFeedUrl = TS.fromAttrib "xmlUrl"

class Monad m =>
      MonadParse m  where
    parseFeed :: BSL.ByteString -> Url -> m (FeedInfoHW, [FeedItemHW])

instance MonadParse Backend where
    parseFeed feed url = do
        validFeed <- liftJust InvalidXML . parseFeedSource . decodeUtf8With lenientDecode $ feed
        now <- liftIO getCurrentTime
        liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed

class Monad m =>
      MonadOpml m  where
    parseOpml :: T.Text -> (T.Text -> Maybe [Url]) -> m [Url]

instance MonadOpml Backend where
    parseOpml opml parsingFun = liftJust InvalidOPMLData $ parsingFun opml