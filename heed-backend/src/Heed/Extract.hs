{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract
    ( addFeed
    , broadcastUpdate
    , extractInfoFromFeed
    , forceUpdate
    , getFeedUrl
    , importOPML
    , latinTitle
    , matchAllAttr
    , parseFeed
    , parseOpml
    , parseTTRssOPML
    , startUpdateThread
    , ttRssAttrNames
    , ttRssTags
    ) where

import Control.Concurrent (ThreadId, threadDelay)
import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isLatin1)
import Data.Int (Int64)
import Data.List (deleteFirstsBy, minimumBy)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Ord (compare)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Heed.Database
import Heed.DbEnums (ItemsDate(..))
import qualified Heed.Feed.Atom as HAtom
import qualified Heed.Feed.RSS as RSS
import Heed.Query
import Heed.Types
import Heed.Utils (fork)
import qualified Safe
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(AtomFeed, RSS1Feed, RSSFeed, XMLFeed))
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Match as TS

-- | Start a thread for every feed in the db and notify the bchan in the 'BackendConf'
--   when updates arrive
startUpdateThread :: UTCTime -> BackendConf -> FeedInfoHR -> IO ThreadId
startUpdateThread now baConf info =
    fork $ do
        let nextUpdateAt =
                addUTCTime
                    (fromIntegral $ (info ^. feedInfoUpdateEvery) * 60)
                    (info ^. feedInfoLastUpdated)
        when (nextUpdateAt > now) $ do
            logMsgIO (baConf ^. timedLogger) $ info ^. feedInfoName <> ": next update at " <>
                (T.pack . show $ nextUpdateAt)
            threadDelay $ ceiling (toRational (diffUTCTime nextUpdateAt now) * 1000000)
        forever $ do
            res <- runBe baConf $ updateFeed info
            case res of
                Left e -> do
                    logMsgIO (baConf ^. timedLogger) $ "Failed to update " <> _feedInfoName info
                    logMsgIO (baConf ^. timedLogger) . T.pack . show $ e
              -- When feeds have new items send the update to the broadcast chan so
              -- then everyone that is listening will receive the update
                Right newItems -> broadcastUpdate (info, newItems) (baConf ^. updateChan)
            liftIO . threadDelay $ _feedInfoUpdateEvery info * 1000000 * 60

-- | Someone requested a force update. Download feed and update items in db in necessary
forceUpdate
    :: ( MonadCatch m
       , MonadDb m
       , MonadLog m
       , MonadHttp m
       , MonadError HeedError m
       , MonadParse m
       , MonadTime m
       )
    => FeedInfoIdH -> m (FeedInfoHR, Int64)
forceUpdate fid = do
    feedInfoList <- execQuery $ allFeedInfo fid
    -- The list should always have a valid FeedInfo from the DB, but someone may send
    -- an invalid id
    feedInfo <- liftJust InvalidFeedQuery $ Safe.headMay feedInfoList
    new <- updateFeed feedInfo
    return (feedInfo, new)

-- | Broadcast updates to any listeners
broadcastUpdate :: (FeedInfoHR, Int64) -> BChan.BroadcastChan BChan.In (FeedInfoHR, Int64) -> IO ()
broadcastUpdate update@(_, new) bchan
    | new > 0 = BChan.writeBChan bchan update
    | otherwise = return ()

-- | Download, parse and add items to db, also add unread items
updateFeed
    :: ( MonadCatch m
       , MonadDb m
       , MonadLog m
       , MonadHttp m
       , MonadError HeedError m
       , MonadParse m
       , MonadTime m
       )
    => FeedInfoHR -> m Int64
updateFeed info = do
    feed <- catchHttp DownloadFailed $ downloadUrl (_feedInfoUrl info)
    (_, feedItems) <- parseFeed feed (_feedInfoUrl info) (_feedInfoUpdateEvery info)
    num <- updateFeedItems info feedItems
    logMsg $ info ^. feedInfoName <> ": " <> (T.pack . show $ num) <> " new items"
    return num

-- | Check before adding new feed if it already is present in the db. If it's already there
--   set all the old items as unread for the user
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

-- | Add feed to db and set items as unread
addNewFeed
    :: MonadDb m
    => FeedInfoHW -> [FeedItemHW] -> UserId Int -> m (FeedInfoHR, Int64)
addNewFeed feedInfo feedItems uid =
    execQuery $ do
        insertedFeed <- insertFeed feedInfo
        let newFeedId = _feedInfoId . head $ insertedFeed
        insertedItems <- insertItems feedItems newFeedId
        num <- insertUnread insertedItems [uid]
        _ <- addSubscription uid newFeedId
        return (head insertedFeed, num)

-- | Add new items to db and add them as unread for all users that are subscribed
updateFeedItems
    :: (MonadDb m, MonadTime m)
    => FeedInfoHR -> [FeedItemHW] -> m Int64
updateFeedItems feed feedItems = do
    now <- getTime
    execQuery $ do
        let feedId = feed ^. feedInfoId
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

-- | We have to apply 'Just' to the ids since when we read them from the DB they can't
--   be 'Nothing'
applyJust :: FeedItemHR -> FeedItemHW
applyJust hrs = hrs & feedItemId . getFeedItemId %~ Just & feedItemFeedId . getFeedInfoId %~ Just

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfoFromFeed now url (AtomFeed feed) = HAtom.extractInfo now url feed
extractInfoFromFeed now url (RSSFeed feed) = RSS.extractInfo now url feed
-- TODO
extractInfoFromFeed _ _ (RSS1Feed _) = Nothing
extractInfoFromFeed _ _ (XMLFeed _) = Nothing

-- | Import OPML exported by tt-rss
importOPML
    :: (MonadOpml m, MonadLog m, MonadReader BackendConf m, MonadIO m)
    => T.Text -> UserId Int -> m ()
importOPML opml userid = do
    feeds <- parseOpml opml parseTTRssOPML
    conf <- ask
    forM_ feeds $ \url -> do
        result <- runBe conf $ addFeed url defUpdateEvery userid
        case result of
            Left e -> do
                logMsg $ "Failed to add: " <> url
                logMsg . T.pack . show $ e
            Right _ -> logMsg $ "Added: " <> url
        return ()
    return ()

-- | Parse tags of tt-rss exported OPML
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

-- | Update date info of feed
--   1) If any item doesn't have a date we have to set '_feedItemDate' as Missing
--   2) We have to set the total number of feeds
updateDateInfo :: UTCTime -> (FeedInfoHW, [FeedItemHW]) -> (FeedInfoHW, [FeedItemHW])
updateDateInfo now (info, items) = (newInfo, items)
  where
    newInfo =
        info & feedHasItemDate .~ anyItemHasDateNow now items & feedNumberItems .~ length items
    -- On next update download this number of items if dates are 'Missing'
    -- If any of the items don't have a publication date we have to download
    -- a fixed number of the last feeds so we can check which one is new on updates
    anyItemHasDateNow time xs =
        if any (\x -> time == _feedItemDate x) xs
            then Missing
            else Present

latinTitle :: FeedItemHW -> Bool
latinTitle item =
    T.length (item ^. feedItemTitle) < ((length . filter isLatin1 . T.unpack . _feedItemTitle $ item) * 2)

class Monad m =>
      MonadParse m where
    parseFeed :: BSL.ByteString -> Url -> Int -> m (FeedInfoHW, [FeedItemHW])

instance MonadParse Backend where
    parseFeed feed url every = do
        validFeed <- liftJust InvalidXML . parseFeedSource . decodeUtf8With lenientDecode $ feed
        now <- liftIO getCurrentTime
        validInfo <- liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed
        return $ validInfo & updateDateInfo now & _1 . feedInfoUpdateEvery .~ every & _2 %~ filter latinTitle

class Monad m =>
      MonadOpml m where
    parseOpml :: T.Text -> (T.Text -> Maybe [Url]) -> m [Url]

instance MonadOpml Backend where
    parseOpml opml parsingFun = liftJust InvalidOPMLData $ parsingFun opml
