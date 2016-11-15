{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.FeedListStore where

import Control.Concurrent.MVar (tryReadMVar)
import Control.DeepSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Heed.Commands
import Heed.GlobalWebsocket
import React.Flux

data FeedListStore = FeedListStore
    { feedList :: [ReactFeedInfo]
    , selectedFeed :: Maybe Int
    } deriving (Show, Typeable)

data FeedListAction
    = SetFeedList [ReactFeedInfo]
    | SelectFeed Int
    deriving (Show, Typeable, Generic, NFData)

instance StoreData FeedListStore where
    type StoreAction FeedListStore = FeedListAction
    transform action oldStore =
        case action of
            SetFeedList feeds -> return $ FeedListStore feeds Nothing
            SelectFeed idSelected -> do
                wsM <- tryReadMVar heedWebsocket
                case wsM of
                    Nothing -> return ()
                    Just ws -> sendCommand ws (GetFeedItems idSelected)
                return $
                    oldStore
                    { selectedFeed = Just idSelected
                    }

feedListStore :: ReactStore FeedListStore
feedListStore = mkStore $ FeedListStore [] Nothing
