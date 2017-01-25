{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract
  ( startUpdateThread
  , addFeed
  , importOPML
  , parseTTRssOPML
  , ttRssTags
  , matchAllAttr
  , ttRssAttrNames
  , getFeedUrl
  , parseOpml
  , extractInfoFromFeed
  , forceUpdate
  , broadcastUpdate
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Lens
import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.List (deleteFirstsBy, minimumBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Ord (compare)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time
       (defaultTimeLocale, getCurrentTime, parseTimeM, rfc822DateFormat)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Heed.Database
import Heed.DbEnums (ItemsDate(..))
import Heed.Query
import Heed.Types
import qualified HTMLEntities.Decoder as EntDec
import qualified Safe
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Match as TS
import qualified Text.RSS.Syntax as RSS

startUpdateThread :: UTCTime -> BackendConf -> FeedInfoHR -> IO ThreadId
startUpdateThread now baConf info =
    forkIO $
    do let nextUpdateAt =
               addUTCTime
                   (fromIntegral $ (info ^. feedInfoUpdateEvery) * 60)
                   (info ^. feedInfoLastUpdated)
       when (nextUpdateAt > now) $
           do logMsgIO (baConf ^. timedLogger) $ info ^. feedInfoName <> ": next update at " <>
                  (T.pack . show $ nextUpdateAt)
              threadDelay $ ceiling (toRational (diffUTCTime nextUpdateAt now) * 1000000)
       forever $
           do res <- runBe baConf $ updateFeed info
              case res of
                  Left e -> do
                      logMsgIO (baConf ^. timedLogger) $ "Failed to update " <> _feedInfoName info
                      logMsgIO (baConf ^. timedLogger) . T.pack . show $ e
                  -- When feeds have new items send the update to the broadcast chan so
                  -- then everyone that is listening will receive the update
                  Right newItems -> broadcastUpdate (info, newItems) (baConf ^. updateChan)
              liftIO . threadDelay $ _feedInfoUpdateEvery info * 1000000 * 60

forceUpdate
    :: (MonadCatch m
       ,MonadDb m
       ,MonadLog m
       ,MonadHttp m
       ,MonadError HeedError m
       ,MonadParse m
       ,MonadTime m)
    => FeedInfoIdH -> m (FeedInfoHR, Int64)
forceUpdate fid = do
    feedInfoList <- execQuery $ allFeedInfo fid
    -- The list should always have a valid FeedInfo from the DB, but someone may send
    -- an invalid id
    feedInfo <- liftJust InvalidFeedQuery $ Safe.headMay feedInfoList
    new <- updateFeed feedInfo
    return (feedInfo, new)

broadcastUpdate :: (FeedInfoHR, Int64) -> BChan.BroadcastChan BChan.In (FeedInfoHR, Int64) -> IO ()
broadcastUpdate update@(_, new) bchan
    | new > 0 = BChan.writeBChan bchan update
    | otherwise = return ()

updateFeed
    :: (MonadCatch m
       ,MonadDb m
       ,MonadLog m
       ,MonadHttp m
       ,MonadError HeedError m
       ,MonadParse m
       ,MonadTime m)
    => FeedInfoHR -> m Int64
updateFeed info = do
    feed <- catchHttp DownloadFailed $ downloadUrl (_feedInfoUrl info)
    (_, feedItems) <- parseFeed feed (_feedInfoUrl info) (_feedInfoUpdateEvery info)
    num <- updateFeedItems info feedItems
    logMsg $ info ^. feedInfoName <> ": " <> (T.pack . show $ num) <> " new items"
    return num

addFeed
    :: (MonadLog m, MonadHttp m, MonadParse m, MonadDb m, MonadTime m)
    => Url -- ^ Feed URL
    -> Int -- ^ Update Every
    -> UserId Int
    -> m (FeedInfoHR, Int64)
addFeed url every uid = do
    logMsg $ "Adding: " <> url
    feed <- downloadUrl url
    (feedInfo, feedItems) <- parseFeed feed url every
    oldFeeds <- execQuery $ runFeedInfoQuery (thisFeed feedInfo)
    case listToMaybe oldFeeds
         -- We don't have this feed in the common database, insert it
          of
        Nothing -> addNewFeed feedInfo feedItems uid
        -- This feed is already present in the database, add the user to the subscription
        Just oldFeed -> do
            newItems <- updateFeedItems oldFeed feedItems
            return (oldFeed, newItems)

addNewFeed
    :: MonadDb m
    => FeedInfoHW -> [FeedItemHW] -> UserId Int -> m (FeedInfoHR, Int64)
addNewFeed feedInfo feedItems uid =
    execQuery $
    do insertedFeed <- insertFeed feedInfo
       let newFeedId = _feedInfoId . head $ insertedFeed
       insertedItems <- insertItems feedItems newFeedId
       num <- insertUnread insertedItems [uid]
       _ <- addSubscription uid newFeedId
       return (head insertedFeed, num)

updateFeedItems
    :: (MonadDb m, MonadTime m)
    => FeedInfoHR -> [FeedItemHW] -> m Int64
updateFeedItems feed feedItems = do
    now <- getTime
    execQuery $
        do let feedId = feed ^. feedInfoId
               newFeedItems :: [FeedItemHW]
               newFeedItems = feedItems & traverse . feedItemFeedId .~ (Just <$> feedId)
           recentItems <- getRecentItems feed (_feedItemDate (minimumBy after feedItems))
           let newItems :: [FeedItemHW]
               newItems = deleteFirstsBy (sameItem feedId) newFeedItems (applyJust <$> recentItems)
           inserted <-
               if not (null newItems)
                   then do
                       insertedItems <- insertItems newItems feedId
                       feedSubs <- getSubs feedId
                       insertUnread insertedItems feedSubs
                   else return 0
           _ <- setFeedLastUpdated feedId now
           return inserted
  where
    after x y = compare (_feedItemDate x) (_feedItemDate y)
    sameItem :: FeedInfoId Int -> FeedItemHW -> FeedItemHW -> Bool
    sameItem feedId fromHttp fromDb =
        (fromDb ^. feedItemFeedId) == (Just <$> feedId) &&
        (_feedItemTitle fromDb == _feedItemTitle fromHttp) &&
        (_feedItemUrl fromDb == _feedItemUrl fromHttp)

applyJust :: FeedItemHR -> FeedItemHW
applyJust hrs = hrs & feedItemId . getFeedItemId %~ Just & feedItemFeedId . getFeedInfoId %~ Just

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfoFromFeed now url (AtomFeed feed) = Just (feedInfo, feedItems)
  where
    feedInfo =
        defFeedInfo
        { _feedInfoName = T.strip . decodeHtmlEnt . T.pack . Atom.txtToString . Atom.feedTitle $ feed
        , _feedInfoUrl = url
        , _feedInfoUpdateEvery = 60
        , _feedInfoLastUpdated = fromMaybe now (parseISO8601 . Atom.feedUpdated $ feed)
        }
    feedItems = atomEntryToItem now <$> Atom.feedEntries feed
extractInfoFromFeed now url (RSSFeed feed) = Just (feedInfo, feedItems)
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
-- TODO
extractInfoFromFeed _ _ (RSS1Feed _) = Nothing
extractInfoFromFeed _ _ (XMLFeed _) = Nothing

atomEntryToItem :: UTCTime -> Atom.Entry -> FeedItemHW
atomEntryToItem now entry =
    defFeedItem
    { _feedItemTitle = decodeHtmlEnt . T.pack . Atom.txtToString . Atom.entryTitle $ entry
    , _feedItemUrl = T.pack . Safe.headDef "" $ (Atom.linkHref <$> Atom.entryLinks entry)
    , _feedItemDate = fromMaybe now $ parseISO8601 (Atom.entryUpdated entry)
    }

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

importOPML
    :: (MonadOpml m, MonadLog m, MonadReader BackendConf m, MonadIO m)
    => T.Text -> UserId Int -> m ()
importOPML opml userid = do
    feeds <- parseOpml opml parseTTRssOPML
    conf <- ask
    forM_ feeds $
        \url -> do
            result <- runBe conf $ addFeed url defUpdateEvery userid
            case result of
                Left e -> do
                    logMsg $ "Failed to add: " <> url
                    logMsg . T.pack . show $ e
                Right _ -> logMsg $ "Added: " <> url
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

updateDateInfo :: UTCTime -> (FeedInfoHW, [FeedItemHW]) -> (FeedInfoHW, [FeedItemHW])
updateDateInfo now (info, items) = (newInfo, items)
  where
    newInfo = info & feedHasItemDate .~ anyItemHasDateNow now items & feedNumberItems .~ length items
    -- On next update download this number of items if dates are 'Missing'
    -- If any of the items don't have a publication date we have to download
    -- a fixed number of the last feeds so we can check which one is new on updates
    anyItemHasDateNow time xs =
        if any (\x -> time == _feedItemDate x) xs
            then Missing
            else Present

decodeHtmlEnt :: T.Text -> T.Text
decodeHtmlEnt = TL.toStrict . toLazyText . EntDec.htmlEncodedText

class Monad m =>
      MonadParse m  where
    parseFeed :: BSL.ByteString -> Url -> Int -> m (FeedInfoHW, [FeedItemHW])

instance MonadParse Backend where
    parseFeed feed url every = do
        validFeed <- liftJust InvalidXML . parseFeedSource . decodeUtf8With lenientDecode $ feed
        now <- liftIO getCurrentTime
        validInfo <- liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed
        return $ validInfo & updateDateInfo now & _1 . feedInfoUpdateEvery .~ every

class Monad m =>
      MonadOpml m  where
    parseOpml :: T.Text -> (T.Text -> Maybe [Url]) -> m [Url]

instance MonadOpml Backend where
    parseOpml opml parsingFun = liftJust InvalidOPMLData $ parsingFun opml
