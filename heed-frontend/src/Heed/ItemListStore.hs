{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.ItemListStore where

import Control.DeepSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Heed.Commands
import React.Flux

data ItemListStore = ItemListStore
    { itemList :: [ReactItemInfo]
    , selectedItem :: Maybe ReactItemInfo
    } deriving (Show, Typeable)

data ItemListAction
    = SetItemList [ReactItemInfo]
    | SelectItem ReactItemInfo
    deriving (Show, Typeable, Generic, NFData)

instance StoreData ItemListStore where
    type StoreAction ItemListStore = ItemListAction
    transform action oldStore =
        case action of
            SetItemList items -> return $ ItemListStore items Nothing
            SelectItem itemSelected ->
                return $
                oldStore
                { selectedItem = Just itemSelected
                }

itemListStore :: ReactStore ItemListStore
itemListStore = mkStore $ ItemListStore [] Nothing
