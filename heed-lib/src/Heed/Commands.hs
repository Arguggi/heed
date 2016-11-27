{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heed.Commands where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Int
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

-- | Convenience
type HeedUserName = T.Text

-- | List of feeds sent to the client
data ReactFeedInfo' a b c = ReactFeedInfo'
    { _feedListId :: a -- ^ Postgresql feed id
    , _feedListName :: b -- ^ Feed name
    , _feedListUnread :: c -- ^ Number of unread items
    } deriving (Generic, Show)

-- | Will be sent to client
type ReactFeedInfo = ReactFeedInfo' Int T.Text Int64

makeLenses ''ReactFeedInfo'

instance Eq ReactFeedInfo where
    a == b = _feedListId a == _feedListId b

instance FromJSON ReactFeedInfo

instance ToJSON ReactFeedInfo

instance NFData ReactFeedInfo

-- | List of items, one for each feed
data ReactItemInfo' a b c d e = ReactItemInfo'
    { _itemInfoId :: a -- ^ Postgresql item id
    , _itemInfoTitle :: b -- ^ Item Title
    , _itemInfoLink :: c -- ^ Item Link
    , _itemInfoDate :: d -- ^ Item published date
    , _itemInfoComments :: e -- ^ Item comment link if available
    } deriving (Generic, Show)

-- | Will be sent to client
type ReactItemInfo = ReactItemInfo' Int T.Text T.Text UTCTime (Maybe T.Text)

makeLenses ''ReactItemInfo'

instance Eq ReactItemInfo where
    a == b = _itemInfoId a == _itemInfoId b

instance FromJSON ReactItemInfo

instance ToJSON ReactItemInfo

instance NFData ReactItemInfo

-- | Decide if something is selected
class IsSelected a  where
    isSelected :: a -> Maybe a -> Bool

-- | Decide if 'ReactFeedInfo' is selected
instance IsSelected ReactFeedInfo where
    isSelected _ Nothing = False
    isSelected selId (Just feedInfo) = _feedListId selId == _feedListId feedInfo

-- | Decide if 'ReactItemInfo' is selected
instance IsSelected ReactItemInfo where
    isSelected _ Nothing = False
    isSelected selId (Just itemInfo) = _itemInfoId selId == _itemInfoId itemInfo

-- | Commands sent from client to server via websocket
--
--   The client never has to send his id since the websocket server
--   knows who is who by looking at the authentication token
--   This hopefully prevents authentication security problems
data Up
    = Initialized -- ^ Sent when the client is ready to receive data
    | GetFeedItems Int -- ^ Get a feeds items
    | ItemRead Int -- ^ Sent when an item is considered read
    | InvalidReceived -- ^ The server can't parse the message (should never
                      --   happen since client and server share the exact same code)
    deriving (Generic, Show)

instance FromJSON Up

instance ToJSON Up

-- | Commands sent from server to client via websocket
data Down
    = Feeds [ReactFeedInfo] -- ^ List of feeds
    | FeedItems [ReactItemInfo] -- ^ Feed items
    | Status HeedUserName -- ^ Username of who is logged in obtained via auth token
    | InvalidSent -- ^ The client can't parse the message
    deriving (Generic, Show)

instance FromJSON Down

instance ToJSON Down
