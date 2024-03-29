{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.Extract
  ( addFeed,
    broadcastUpdate,
    filterNew,
    forceUpdate,
    importOPMLTTRSS,
    parseTTRssOPML,
    startUpdateThread,
    extractInfoFromFeed,

    -- * Parse feed MTL
    MonadParse (parseFeed),

    -- * Parse OPTM MTL
    MonadOpml (parseOpml),
  )
where

import qualified BroadcastChan as BChan
import Control.Concurrent (ThreadId, threadDelay)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isLatin1)
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hash
import Data.Int (Int64)
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Heed.Database
  ( FeedInfo
      ( _feedInfoId,
        _feedInfoName,
        _feedInfoUpdateEvery,
        _feedInfoUrl
      ),
    FeedInfoHR,
    FeedInfoHW,
    FeedInfoIdH,
    FeedItem (_feedItemDate, _feedItemTitle, _feedItemUrl),
    FeedItemHR,
    FeedItemHW,
    Url,
    UserId,
    defUpdateEvery,
    feedHasItemDate,
    feedInfoId,
    feedInfoLastUpdated,
    feedInfoName,
    feedInfoUpdateEvery,
    feedItemFeedId,
    feedItemId,
    feedItemTitle,
    feedNumberItems,
    getFeedInfoId,
    getFeedItemId,
  )
import Heed.DbEnums (ItemsDate (..))
import qualified Heed.Feed.Atom as HAtom
import qualified Heed.Feed.RSS as RSS
import qualified Heed.Feed.XML as XML
import Heed.Query
  ( addSubscription,
    allFeedInfo,
    getRecentItems,
    getSubs,
    insertFeed,
    insertItems,
    insertUnread,
    setFeedLastUpdated,
    thisFeed,
  )
import Heed.Types
  ( Backend,
    BackendConf,
    ChanUpdates (SendItems),
    HeedError
      ( DownloadFailed,
        FeedListEmpty,
        InvalidFeedData,
        InvalidFeedQuery,
        InvalidOPMLData,
        InvalidXML
      ),
    MonadDb (..),
    MonadHttp (..),
    MonadLog (..),
    MonadTime (..),
    catchHttp,
    liftJust,
    logMsgIO,
    runBe,
    timedLogger,
    updateChan,
  )
import Heed.Utils (fork)
import Lens.Micro.Platform ((%~), (&), (.~), (^.), _1)
import qualified Safe
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed (AtomFeed, RSS1Feed, RSSFeed, XMLFeed))
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Match as TS
import qualified Text.XML

-- | Start a thread for every feed in the db and notify the bchan in the 'BackendConf'
--   when updates arrive
startUpdateThread ::
  -- | Current time
  UTCTime ->
  BackendConf ->
  -- | Feed to keed updated
  FeedInfoHR ->
  IO ThreadId
startUpdateThread now baConf info =
  fork $ do
    let nextUpdateAt =
          addUTCTime
            (fromIntegral $ (info ^. feedInfoUpdateEvery) * 60)
            (info ^. feedInfoLastUpdated)
    when (nextUpdateAt > now) $ do
      logMsgIO (baConf ^. timedLogger) $
        info ^. feedInfoName <> ": next update at "
          <> (T.pack . show $ nextUpdateAt)
      threadDelay $ ceiling (toRational (diffUTCTime nextUpdateAt now) * 1000000)
    forever $ do
      res <- runBe baConf $ updateFeed info
      case res of
        Left e -> do
          logMsgIO (baConf ^. timedLogger) $ "Failed to update " <> _feedInfoName info
          logMsgIO (baConf ^. timedLogger) . T.pack . show $ e
        -- When feeds have new items send the update to the broadcast chan so
        -- then everyone that is listening will receive the update
        Right newItems -> void (broadcastUpdate (info, newItems) (baConf ^. updateChan))
      liftIO . threadDelay $ _feedInfoUpdateEvery info * 1000000 * 60

-- | 'UserId' requested an immediate update.
--   Download feed and update items in db if necessary.
forceUpdate ::
  ( MonadCatch m,
    MonadDb m,
    MonadLog m,
    MonadHttp m,
    MonadError HeedError m,
    MonadParse m,
    MonadTime m
  ) =>
  -- | Feed to update
  FeedInfoIdH ->
  m (FeedInfoHR, Int64)
forceUpdate fid = do
  feedInfoList <- execSelect $ allFeedInfo fid
  -- The list should always have a valid FeedInfo from the DB, but someone may send
  -- an invalid id
  feedInfo <- liftJust InvalidFeedQuery $ Safe.headMay feedInfoList
  new <- updateFeed feedInfo
  return (feedInfo, new)

-- | Broadcast updates to any listeners
broadcastUpdate ::
  -- | (Feed, number of new items)
  (FeedInfoHR, Int64) ->
  -- | New items will be sent here
  BChan.BroadcastChan BChan.In ChanUpdates ->
  IO Bool
broadcastUpdate (items, new) bchan
  | new > 0 = BChan.writeBChan bchan (SendItems items new)
  | otherwise = return False

-- | Download, parse and add items to db, also add unread items
updateFeed ::
  ( MonadCatch m,
    MonadDb m,
    MonadLog m,
    MonadHttp m,
    MonadError HeedError m,
    MonadParse m,
    MonadTime m
  ) =>
  -- | Feed to update
  FeedInfoHR ->
  -- | Number of new items
  m Int64
updateFeed info = do
  feed <- catchHttp DownloadFailed $ downloadUrl (_feedInfoUrl info)
  (_, feedItems) <- parseFeed feed (_feedInfoUrl info) (_feedInfoUpdateEvery info)
  num <- updateFeedItems info feedItems
  logMsg $ info ^. feedInfoName <> ": " <> (T.pack . show $ num) <> " new items"
  return num

-- | Check before adding new feed if it already is present in the db. If it's already there
--   set all the old items as unread for the user.
--
--   Returns the new Feed and the number of unread items
addFeed ::
  (MonadLog m, MonadHttp m, MonadParse m, MonadDb m, MonadTime m) =>
  -- | Feed URL
  Url ->
  -- | Update Every
  Int ->
  -- | 'UserId'
  UserId Int ->
  m (FeedInfoHR, Int64)
addFeed url every uid = do
  logMsg $ "Adding: " <> url
  feed <- downloadUrl url
  (feedInfo, feedItems) <- parseFeed feed url every
  oldFeeds <- execSelect (thisFeed feedInfo)
  case listToMaybe oldFeeds of
    -- We don't have this feed in the common database, insert it

    Nothing -> addNewFeed feedInfo feedItems uid
    -- This feed is already present in the database, add the user to the subscription
    Just oldFeed -> do
      newItems <- updateFeedItems oldFeed feedItems
      return (oldFeed, newItems)

-- | Add feed to db and set items as unread
addNewFeed ::
  MonadDb m =>
  FeedInfoHW ->
  NonEmpty FeedItemHW ->
  UserId Int ->
  m (FeedInfoHR, Int64)
addNewFeed feedInfo feedItems uid = do
  insertedFeed <- execInsert $ insertFeed feedInfo
  let newFeedId = _feedInfoId . head $ insertedFeed
  insertedItems <- execInsert $ insertItems (NonEmpty.toList feedItems) newFeedId
  num <- execInsert $ insertUnread insertedItems [uid]
  _ <- execInsert $ addSubscription uid newFeedId
  return (head insertedFeed, num)

-- What we consider "new"
newtype FeedItemEq = FeedItemEq
  { runFeedItemEq :: FeedItemHW
  }

instance Eq FeedItemEq where
  (FeedItemEq a) == (FeedItemEq b) =
    _feedItemTitle a == _feedItemTitle b && _feedItemUrl a == _feedItemUrl b

compareDate :: FeedItemEq -> FeedItemEq -> Ordering
compareDate (FeedItemEq a) (FeedItemEq b) = _feedItemDate a `compare` _feedItemDate b

instance Hash.Hashable FeedItemEq where
  hashWithSalt s (FeedItemEq item) =
    s `Hash.hashWithSalt` _feedItemTitle item `Hash.hashWithSalt` _feedItemUrl item

filterNew ::
  -- | New items from feed
  [FeedItemHW] ->
  -- | Items from database
  [FeedItemHW] ->
  [FeedItemHW]
filterNew new db = fmap runFeedItemEq . HashSet.toList $ toHash new `HashSet.difference` toHash db
  where
    toHash = HashSet.fromList . fmap FeedItemEq

-- | Add new items to db and add them as unread for all users that are subscribed
updateFeedItems ::
  (MonadDb m, MonadTime m) =>
  FeedInfoHR ->
  NonEmpty FeedItemHW ->
  m Int64
updateFeedItems feed feedItems = do
  now <- getTime
  -- Get items from db (and transform them to HW)
  recentItems <- fmap (fmap toHW) . execSelect $ getRecentItems feed firstDate
  --recentItemsSet = Set.fromList $ fmap (FeedItemEq . applyJust) recentItems
  let newItems :: [FeedItemHW]
      --newItems = fmap runFeedItemEq . Set.toList $ newItemsSet `Set.difference` recentItemsSet
      newItems = filterNew (NonEmpty.toList feedItems) recentItems
  _ <- execUpdate $ setFeedLastUpdated feedId now
  if not (null newItems)
    then do
      insertedItems <- execInsert $ insertItems newItems feedId
      feedSubs <- execSelect $ getSubs feedId
      execInsert $ insertUnread insertedItems feedSubs
    else return 0
  where
    feedId = feed ^. feedInfoId
    firstDate = _feedItemDate . runFeedItemEq . minimumBy compareDate . fmap FeedItemEq $ feedItems

-- | We have to apply 'Just' to the ids since when we read them from the DB they can't
--   be 'Nothing'
toHW :: FeedItemHR -> FeedItemHW
toHW hrs = hrs & feedItemId . getFeedItemId %~ Just & feedItemFeedId . getFeedInfoId %~ Just

extractInfoFromFeed :: UTCTime -> Url -> Feed -> Maybe (FeedInfoHW, [FeedItemHW])
extractInfoFromFeed now url (AtomFeed feed) = HAtom.extractInfo now url feed
extractInfoFromFeed now url (RSSFeed feed) = RSS.extractInfo now url feed
extractInfoFromFeed now url (XMLFeed feed) = case Text.XML.fromXMLElement feed of
  Left _ -> Nothing
  Right el -> XML.extractInfo now url el
-- TODO
extractInfoFromFeed _ _ (RSS1Feed _) = Nothing

-- | Import OPML exported by tt-rss
importOPMLTTRSS ::
  (MonadOpml m, MonadLog m, MonadReader BackendConf m, MonadIO m) =>
  -- | Raw feed data
  T.Text ->
  -- | 'Userid'
  UserId Int ->
  m ()
importOPMLTTRSS opml userid = do
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
updateDateInfo :: UTCTime -> (FeedInfoHW, NonEmpty FeedItemHW) -> (FeedInfoHW, NonEmpty FeedItemHW)
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
  T.length (item ^. feedItemTitle)
    < ((length . filter isLatin1 . T.unpack . _feedItemTitle $ item) * 2)

class
  Monad m =>
  MonadParse m
  where
  parseFeed ::
    -- | Raw feed data
    BSL.ByteString ->
    -- | Feed 'Url'
    Url ->
    -- | Update every x minutes
    Int ->
    m (FeedInfoHW, NonEmpty FeedItemHW)

instance MonadParse Backend where
  parseFeed feed url every = do
    validFeed <- liftJust InvalidXML . parseFeedSource . decodeUtf8With lenientDecode $ feed
    now <- liftIO getCurrentTime
    (feedInfo, itemsInfoM) <- liftJust InvalidFeedData $ extractInfoFromFeed now url validFeed
    itemsInfo <- liftJust FeedListEmpty $ NonEmpty.nonEmpty (filter latinTitle itemsInfoM)
    let validInfo = (feedInfo, itemsInfo)
    return $ validInfo & updateDateInfo now & _1 . feedInfoUpdateEvery .~ every

class
  Monad m =>
  MonadOpml m
  where
  parseOpml ::
    -- | Raw OPML data
    T.Text ->
    -- | Parsing function
    (T.Text -> Maybe [Url]) ->
    m [Url]

instance MonadOpml Backend where
  parseOpml opml parsingFun = liftJust InvalidOPMLData $ parsingFun opml
