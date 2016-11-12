{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.FeedListStore where

import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Heed.Commands

newtype FeedListStore = FeedListStore
    { feedList :: [ReactFeedInfo]
    } deriving (Show, Typeable)

data FeedItem = FeedItem
    { itemName :: T.Text
    , unreadCount :: Int
    } deriving (Show, Typeable)

data FeedListAction =
    SetFeedList [ReactFeedInfo]
    deriving (Show, Typeable, Generic, NFData)

instance StoreData FeedListStore where
    type StoreAction FeedListStore = FeedListAction
    transform action _ =
        case action of
            SetFeedList feeds -> return $ FeedListStore feeds

feedListStore :: ReactStore FeedListStore
feedListStore = mkStore $ FeedListStore [ReactFeedInfo' 2 "asdsad" 23]
