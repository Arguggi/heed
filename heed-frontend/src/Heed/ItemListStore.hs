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

-- | ItemList state
data ItemListStore = ItemListStore
    { _itemList :: [ReactItemStatus] -- ^ List of Items
    , _selectedItem :: Maybe ReactItemStatus -- ^ Currently selected item
    } deriving (Show, Generic, Typeable, NFData, Eq)

-- | Is the item still unread?
data ReadStatus
    = Unread -- ^ Item is unread (default status)
    | Read -- ^ Item was either skipped or opened
    deriving (Show, Generic, Typeable, NFData, Eq)

-- | Item status
data ReactItemStatus = ReactItemStatus
    { _readStatus :: ReadStatus -- ^ Already Read
    , _itemInfo :: ReactItemInfo -- ^ Item info
    } deriving (Show, Generic, Typeable, NFData, Eq)

makeLenses ''ItemListStore

makeLenses ''ReactItemStatus

-- | Possibile Item store actions
data ItemListAction
    = SetItemList [ReactItemInfo] -- ^ Set the item list
    | SelectItem ReactItemStatus -- ^ Select an item
    | NextItem -- ^ Go to next item in list, wraps around
    | PrevItem -- ^ Go to previous item in list, wraps around
    | OpenItem -- ^ Open the items url in a new tab
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

-- | Send a 'SelectItem' action
selectFirstItem
    :: ReactItemStatus -- ^ Item to select
    -> IO ()
selectFirstItem ffeed = forkIO_ $ mapM_ executeAction (dispatchItemList . SelectItem $ ffeed)

-- | Send 'ItemRead' to server
sendItemRead :: Maybe ReactItemStatus -> IO ()
sendItemRead Nothing = return () -- Should never happen?
sendItemRead (Just info) = sendCommand $ ItemRead (info ^. itemInfo . itemInfoId)

-- | Send Update Unread count in this items feed
updateUnreadCount :: IO ()
updateUnreadCount = executeAction (SomeStoreAction feedListStore FeedRead)

-- | Open link in a new tab
openLink :: Maybe ReactItemStatus -> IO ()
openLink Nothing = return ()
openLink (Just info) = openInNewTab $ info ^. itemInfo . itemInfoLink

-- | Dispatcher for 'ItemListStore'
dispatchItemList :: ItemListAction -> [SomeStoreAction]
dispatchItemList action = [SomeStoreAction itemListStore action]

-- | Create the store
itemListStore :: ReactStore ItemListStore
itemListStore = mkStore $ ItemListStore [] Nothing

-- | Set all 'ReactItemInfo' to 'Unread'
allUnread :: [ReactItemInfo] -> [ReactItemStatus]
allUnread = fmap (ReactItemStatus Unread)
