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

newtype HeedFeedList = HeedFeedList
    { feedList :: [FeedList]
    } deriving (Show, Typeable)

data FeedItem = FeedItem
    { itemName :: T.Text
    , unreadCount :: Int
    } deriving (Show, Typeable)

data FeedListAction =
    SetFeedList [FeedList]
    deriving (Show, Typeable, Generic, NFData)

instance StoreData HeedFeedList where
    type StoreAction HeedFeedList = FeedListAction
    transform action _ =
        case action of
            SetFeedList feeds -> return $ HeedFeedList feeds

feedListStore :: ReactStore HeedFeedList
feedListStore = mkStore $ HeedFeedList []
