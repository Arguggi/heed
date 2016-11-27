{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.FeedListStore where

import Control.Concurrent (forkIO)
import Control.DeepSeq
import Control.Lens
import Control.Monad (void)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Heed.Commands
import Heed.GlobalWebsocket
import React.Flux
import Safe (headMay, lastMay)

-- | FeedList state
data FeedListStore = FeedListStore
    { _feedList :: [ReactFeedInfo]
    , _selectedFeed :: Maybe ReactFeedInfo
    } deriving (Show, Typeable)

makeLenses ''FeedListStore

-- | Possibile FeedList store actions
data FeedListAction
    = SetFeedList [ReactFeedInfo] -- ^ Set the feed list
    | SelectFeed ReactFeedInfo -- ^ Select a feed
    | FeedRead -- ^ An item of this feed has been read, update unread counter
    | NextFeed -- ^ Select the next feed, wraps around
    | PrevFeed -- ^ Select the previous feed, wraps around
    deriving (Show, Typeable, Generic, NFData)

instance StoreData FeedListStore where
    type StoreAction FeedListStore = FeedListAction
    transform action oldStore =
        case action of
            SetFeedList feeds -> do
                let firstFeed = headMay feeds
                case firstFeed of
                    Nothing -> return ()
                    Just ffeed ->
                        void . forkIO $ mapM_ executeAction (dispatchFeedList . SelectFeed $ ffeed)
                return $ FeedListStore feeds (headMay feeds)
            SelectFeed selFeed -> do
                loadFeed $ Just selFeed
                return $ oldStore & selectedFeed .~ Just selFeed
            NextFeed -> do
                let nextFeedM = do
                        currentFeed <- _selectedFeed oldStore
                        let (_, _:afterFeeds) = break (== currentFeed) (_feedList oldStore)
                        if not (null afterFeeds)
                            then headMay afterFeeds
                            else headMay (_feedList oldStore)
                loadFeed nextFeedM
                return $ oldStore & selectedFeed .~ nextFeedM
            PrevFeed -> do
                let prevFeedM = do
                        currentFeed <- _selectedFeed oldStore
                        let prevList = takeWhile (/= currentFeed) (_feedList oldStore)
                        if not (null prevList)
                            then lastMay prevList
                            else lastMay (_feedList oldStore)
                loadFeed prevFeedM
                return $ oldStore & selectedFeed .~ prevFeedM
            FeedRead -> do
                let currentPos =
                        fromJust $
                        elemIndex (fromJust $ _selectedFeed oldStore) (_feedList oldStore)
                return $ oldStore & feedList . ix currentPos . feedListUnread %~ subtractPositive 1

-- | Dispatcher for 'FeedListStore'
dispatchFeedList :: FeedListAction -> [SomeStoreAction]
dispatchFeedList action = [SomeStoreAction feedListStore action]

-- | Create the store
feedListStore :: ReactStore FeedListStore
feedListStore = mkStore $ FeedListStore [] Nothing

-- | Send command to load the current feeds items
loadFeed :: Maybe ReactFeedInfo -> IO ()
loadFeed Nothing = return ()
loadFeed (Just feedInfo) = sendCommand $ GetFeedItems (_feedListId feedInfo)

-- | Subtract x y = y - x only if the new total is > 0
subtractPositive
    :: (Num a, Ord a)
    => a -- ^ Number to subtract
    -> a -- ^ Subtract from
    -> a
subtractPositive sub total =
    let newTotal = total - sub in
    if newTotal  < 1 then 0 else newTotal
