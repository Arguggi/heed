{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.ItemListStore where

import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Heed.Commands

data ItemListStore = ItemListStore
    { itemList :: [ReactItemInfo]
    , selectedItem :: Maybe Int
    } deriving (Show, Typeable)

data ItemListAction
    = SetItemList [ReactItemInfo]
    | SelectItem Int
    deriving (Show, Typeable, Generic, NFData)

instance StoreData ItemListStore where
    type StoreAction ItemListStore = ItemListAction
    transform action oldStore =
        case action of
            SetItemList items -> return $ ItemListStore items Nothing
            SelectItem idSelected ->
                return $
                oldStore
                { selectedItem = Just idSelected
                }

itemListStore :: ReactStore ItemListStore
itemListStore = mkStore $ ItemListStore [] Nothing
