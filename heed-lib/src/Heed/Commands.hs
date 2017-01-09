{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Heed.Commands where

import Control.Lens
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int
import Data.Monoid ((<>))
import Data.Store (Store, decode, encode)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Servant.API.ContentTypes
       (MimeRender(..), MimeUnrender(..), OctetStream)
import Web.FormUrlEncoded (FromForm(..))

-- | Convenience
type HeedUserName = T.Text

data Seen
    = Seen
    | Unseen
    deriving (Show, Generic, Eq)

toBool :: Seen -> Bool
toBool Seen = True
toBool Unseen = False

fromBool :: Bool -> Seen
fromBool True = Seen
fromBool False = Unseen

instance Monoid Seen where
    mempty = Seen
    Seen `mappend` _ = Seen
    Unseen `mappend` a = a

instance Store Seen

-- | List of feeds sent to the client
data FeFeedInfo' a b c = FeFeedInfo'
    { _feedListId :: a -- ^ Postgresql feed id
    , _feedListName :: b -- ^ Feed name
    , _feedListUnread :: c -- ^ Number of unread items
    } deriving (Generic, Show)

-- | Will be sent to client
type FeFeedInfo = FeFeedInfo' Int T.Text Int64

makeLenses ''FeFeedInfo'

instance Eq FeFeedInfo where
    a == b = _feedListId a == _feedListId b

instance Store FeFeedInfo

-- | List of items, one for each feed
data FeItemInfo' a b c d e f = FeItemInfo'
    { _itemInfoId :: a -- ^ Postgresql item id
    , _itemInfoTitle :: b -- ^ Item Title
    , _itemInfoLink :: c -- ^ Item Link
    , _itemInfoDate :: d -- ^ Item published date
    , _itemInfoComments :: e -- ^ Item comment link if available
    , _itemInfoRead :: f -- ^ Item is read
    } deriving (Generic, Show)

-- | Will be sent to client
type FeItemInfo = FeItemInfo' Int T.Text T.Text UTCTime (Maybe T.Text) Seen

makeLenses ''FeItemInfo'

instance Eq FeItemInfo where
    a == b = _itemInfoId a == _itemInfoId b

instance Store FeItemInfo

-- | Decide if something is selected
class IsSelected a  where
    isSelected :: a -> Maybe a -> Bool

-- | Decide if 'FeFeedInfo' is selected
instance IsSelected FeFeedInfo where
    isSelected _ Nothing = False
    isSelected selId (Just feedInfo) = _feedListId selId == _feedListId feedInfo

-- | Decide if 'FeItemInfo' is selected
instance IsSelected FeItemInfo where
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
    | FeedRead Int -- ^ Sent when all items of a fead are read
    | InvalidReceived -- ^ The server can't parse the message (should never
      --   happen since client and server share the exact same code)
    deriving (Generic, Show)

instance Store Up

-- | Commands sent from server to client via websocket
data Down
    = Feeds [FeFeedInfo] -- ^ List of feeds
    | FeedItems [FeItemInfo] -- ^ Feed items
    | Status HeedUserName -- ^ Username of who is logged in obtained via auth token
    | InvalidSent -- ^ The client can't parse the message
    deriving (Generic, Show)

instance Store Down

data AuthData = AuthData
    { username :: T.Text
    , password :: T.Text
    } deriving (Generic)

instance FromForm AuthData

instance Store AuthData

--instance MimeRender OctetStream AuthData where
--mimeRender _ = fromStrict . encode
instance MimeUnrender OctetStream AuthData where
    mimeUnrender _ a =
        case decode . toStrict $ a of
            Left e -> Left ("Invalid OctetStream: " <> show e)
            Right ad -> Right ad

-- | A Token we generate if username and password are correct
newtype Token = Token
    { unToken :: T.Text
    } deriving (Generic, Show)

instance Store Token

instance MimeRender OctetStream Token where
    mimeRender _ = fromStrict . encode
