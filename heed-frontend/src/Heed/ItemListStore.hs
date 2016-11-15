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
