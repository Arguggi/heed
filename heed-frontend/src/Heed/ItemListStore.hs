{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.ItemListStore where

import Control.Concurrent (forkIO)
import Control.DeepSeq
import Control.Lens
import Control.Monad (void)
import Data.Foldable (forM_)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Heed.Commands
import Heed.FeedListStore
import Heed.GHCJS
import Heed.GlobalWebsocket
import Heed.Utils
import React.Flux
import Safe (headMay, lastMay)

data ItemListStore = ItemListStore
    { _itemList :: [ReactItemStatus]
    , _selectedItem :: Maybe ReactItemStatus
    } deriving (Show, Generic, Typeable, NFData, Eq)

data ReadStatus
    = Unread
    | Read
    deriving (Show, Generic, Typeable, NFData, Eq)

data ReactItemStatus = ReactItemStatus
    { _readStatus :: ReadStatus
    , _itemInfo :: ReactItemInfo
    } deriving (Show, Generic, Typeable, NFData, Eq)

makeLenses ''ItemListStore

makeLenses ''ReactItemStatus

data ItemListAction
    = SetItemList [ReactItemInfo]
    | SelectItem ReactItemStatus
    | NextItem
    | PrevItem
    | OpenItem
    deriving (Show, Typeable, Generic, NFData)

instance StoreData ItemListStore where
    type StoreAction ItemListStore = ItemListAction
    transform action oldStore =
        case action of
            SetItemList items -> do
                let unreadItems = allUnread items
                    firstItem = headMay unreadItems
                forM_ firstItem selectFirstItem
                return $ ItemListStore (allUnread items) firstItem
            SelectItem itemSelected -> return $ oldStore & selectedItem .~ Just itemSelected
            NextItem ->
                if null (_itemList oldStore)
                    then return oldStore
                    else do
                        let currentPos =
                                fromJust $
                                elemIndex (fromJust $ _selectedItem oldStore) (_itemList oldStore)
                        let nextItemM = do
                                currentItem <- _selectedItem oldStore
                                let (_, _:afterItems) = break (== currentItem) (_itemList oldStore)
                                if not (null afterItems)
                                    then headMay afterItems
                                    else headMay (_itemList oldStore)
                        sendItemRead $ _selectedItem oldStore
                        updateUnreadCount
                        return $ oldStore & selectedItem .~ nextItemM & itemList . ix currentPos . readStatus .~
                            Read
            PrevItem -> do
                let prevFeedM = do
                        currentItem <- _selectedItem oldStore
                        let prevList = takeWhile (/= currentItem) (_itemList oldStore)
                        if not (null prevList)
                            then lastMay prevList
                            else lastMay (_itemList oldStore)
                sendItemRead $ _selectedItem oldStore
                return $ oldStore & selectedItem .~ prevFeedM
            OpenItem -> do
                openLink $ _selectedItem oldStore
                sendItemRead $ _selectedItem oldStore
                void . forkIO $ mapM_ executeAction (dispatchItemList NextItem)
                return oldStore

selectFirstItem :: ReactItemStatus -> IO ()
selectFirstItem ffeed = forkIO_ $ mapM_ executeAction (dispatchItemList . SelectItem $ ffeed)

sendItemRead :: Maybe ReactItemStatus -> IO ()
sendItemRead Nothing = return () -- Should never happen?j
sendItemRead (Just info) = sendCommand $ ItemRead (info ^. itemInfo . itemInfoId)

updateUnreadCount :: IO ()
updateUnreadCount = executeAction (SomeStoreAction feedListStore FeedRead)

openLink :: Maybe ReactItemStatus -> IO ()
openLink Nothing = return ()
openLink (Just info) = openInNewTab $ info ^. itemInfo . itemInfoLink

dispatchItemList :: ItemListAction -> [SomeStoreAction]
dispatchItemList action = [SomeStoreAction itemListStore action]

itemListStore :: ReactStore ItemListStore
itemListStore = mkStore $ ItemListStore [] Nothing

allUnread :: [ReactItemInfo] -> [ReactItemStatus]
allUnread = fmap (ReactItemStatus Unread)
