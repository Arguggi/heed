{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.ItemListStore where

import Control.Concurrent (forkIO)
import Control.DeepSeq
import Control.Monad (void)
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Heed.Commands
import React.Flux
import Safe (headMay, lastMay)

data ItemListStore = ItemListStore
    { itemList :: [ReactItemInfo]
    , selectedItem :: Maybe ReactItemInfo
    } deriving (Show, Typeable)

data ItemListAction
    = SetItemList [ReactItemInfo]
    | SelectItem ReactItemInfo
    | NextItem
    | PrevItem
    deriving (Show, Typeable, Generic, NFData)

instance StoreData ItemListStore where
    type StoreAction ItemListStore = ItemListAction
    transform action oldStore =
        case action of
            SetItemList items -> do
                let firstItem = headMay items
                forM_ firstItem selectFirstItem
                return $ ItemListStore items firstItem
            SelectItem itemSelected ->
                return $
                oldStore
                { selectedItem = Just itemSelected
                }
            NextItem -> do
                let nextItemM = do
                        currentItem <- selectedItem oldStore
                        let (_, _:afterItems) = break (== currentItem) (itemList oldStore)
                        if not (null afterItems)
                            then headMay afterItems
                            else headMay (itemList oldStore)
                return $
                    oldStore
                    { selectedItem = nextItemM
                    }
            PrevItem -> do
                let prevFeedM = do
                        currentItem <- selectedItem oldStore
                        let prevList = takeWhile (/= currentItem) (itemList oldStore)
                        if not (null prevList)
                            then lastMay prevList
                            else lastMay (itemList oldStore)
                return
                    oldStore
                    { selectedItem = prevFeedM
                    }

selectFirstItem :: ReactItemInfo -> IO ()
selectFirstItem ffeed = void . forkIO $ mapM_ executeAction (dispatchItemList . SelectItem $ ffeed)

dispatchItemList :: ItemListAction -> [SomeStoreAction]
dispatchItemList action = [SomeStoreAction itemListStore action]

itemListStore :: ReactStore ItemListStore
itemListStore = mkStore $ ItemListStore [] Nothing
