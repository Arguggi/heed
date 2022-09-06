{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Everything going from backend \<-\> frontend should be this module
module Heed.Commands
  ( AuthData (..),
    Token (..),

    -- * Data sent from backend to client
    FeEditFeed,
    FeEditFeed' (..),
    FeFeedInfo,
    FeFeedInfo' (..),
    FeFeedInfoR,
    FeItemInfo,
    FeItemInfo' (..),
    FeItemInfoR,

    -- * Websocket messages

    -- ** From client to backend
    Up (..),

    -- ** From backend to client
    Down (..),

    -- ** Seen <-> Bool isomorphism
    Seen (..),
    fromBool,
    toBool,

    -- * Feed Info Lenses
    feedListId,
    feedListName,
    feedListUnread,

    -- * Feed Item Lenses
    itemInfoId,
    itemInfoTitle,
    itemInfoLink,
    itemInfoDate,
    itemInfoComments,
    itemInfoRead,
    toFeEditFeed,

    -- * EditableFeedInfo Lenses
    feEditId,
    feEditName,
    feEditUpdateEvery,
  )
where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Serialize (Serialize, decode, encode)
import Data.Serialize.Text ()
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Heed.Database as DB
import Heed.Orphans ()
import Lens.Micro.Platform (makeLenses, (^.))
import qualified Opaleye as O
import Servant.API.ContentTypes
  ( MimeRender (..),
    MimeUnrender (..),
    OctetStream,
  )
import Test.QuickCheck (Arbitrary (..))
import Web.FormUrlEncoded (FromForm (..))

-- | Convenience
type HeedUserName = T.Text

-- | Feed item status
data Seen
  = -- | Already read
    Seen
  | -- | Still unread
    Unseen
  deriving (Show, Generic, Eq)

toBool :: Seen -> Bool
toBool Seen = True
toBool Unseen = False

fromBool :: Bool -> Seen
fromBool True = Seen
fromBool False = Unseen

instance Semigroup Seen where
  Seen <> _ = Seen
  Unseen <> a = a

instance Monoid Seen where
  mempty = Seen

instance Serialize Seen

instance Arbitrary Seen where
  arbitrary = fromBool <$> arbitrary

-- | List of feeds sent to the client
data FeFeedInfo' a b c = FeFeedInfo'
  { -- | Postgresql feed id
    _feedListId :: a,
    -- | Feed name
    _feedListName :: b,
    -- | Number of unread items
    _feedListUnread :: c
  }
  deriving (Generic, Show)

-- | Will be sent to client
type FeFeedInfo = FeFeedInfo' Int T.Text Int64

makeLenses ''FeFeedInfo'

instance
  (Eq a) =>
  Eq (FeFeedInfo' a b c)
  where
  a == b = (a ^. feedListId) == (b ^. feedListId)

instance
  (Eq a, Ord b) =>
  Ord (FeFeedInfo' a b c)
  where
  a `compare` b = (a ^. feedListName) `compare` (b ^. feedListName)

instance
  (Serialize a, Serialize b, Serialize c) =>
  Serialize (FeFeedInfo' a b c)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (FeFeedInfo' a b c)
  where
  arbitrary = FeFeedInfo' <$> arbitrary <*> arbitrary <*> arbitrary

-- | List of items, one for each feed
data FeItemInfo' a b c d e f = FeItemInfo'
  { -- | Postgresql item id
    _itemInfoId :: a,
    -- | Item Title
    _itemInfoTitle :: b,
    -- | Item Link
    _itemInfoLink :: c,
    -- | Item published date
    _itemInfoDate :: d,
    -- | Item comment link if available
    _itemInfoComments :: e,
    -- | Item is read
    _itemInfoRead :: f
  }
  deriving (Generic, Show)

-- | Will be sent to client
type FeItemInfo = FeItemInfo' Int T.Text T.Text UTCTime (Maybe T.Text) Seen

makeLenses ''FeItemInfo'

instance
  (Eq a) =>
  Eq (FeItemInfo' a b c d e f)
  where
  a == b = _itemInfoId a == _itemInfoId b

data FeEditFeed' a b c = FeEditFeed'
  { _feEditId :: a,
    _feEditName :: b,
    _feEditUpdateEvery :: c
  }
  deriving (Show, Generic)

makeLenses ''FeEditFeed'

type FeEditFeed = FeEditFeed' Int T.Text Int

-- | Convert from 'DB.FeedInfoHR' to 'FeEditFeed' since the client can only change
-- some properties of the feed and sending all of them is pointless
toFeEditFeed :: DB.FeedInfoHR -> FeEditFeed
toFeEditFeed feed =
  FeEditFeed'
    (feed ^. DB.feedInfoId . DB.getFeedInfoId)
    (feed ^. DB.feedInfoName)
    (feed ^. DB.feedInfoUpdateEvery)

instance
  (Serialize a, Serialize b, Serialize c) =>
  Serialize (FeEditFeed' a b c)

instance
  (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f) =>
  Serialize (FeItemInfo' a b c d e f)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) =>
  Arbitrary (FeItemInfo' a b c d e f)
  where
  arbitrary =
    FeItemInfo' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary

$(makeAdaptorAndInstance "pFeFeedInfo" ''FeFeedInfo')

-- | 'FeFeedInfo' opaleye type
type FeFeedInfoR = FeFeedInfo' (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGInt8)

$(makeAdaptorAndInstance "pFeItemInfo" ''FeItemInfo')

-- | 'FeItemInfo' opaleye type
type FeItemInfoR = FeItemInfo' (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText)) (O.Column O.PGBool)

instance O.DefaultFromField O.PGBool Seen where
  defaultFromField = fromBool <$> O.fieldQueryRunnerColumn

-- | Commands sent from client to server via websocket
--
--   The client never has to send his id since the websocket server
--   knows who is who by looking at the authentication token
--   This hopefully prevents authentication security problems
data Up
  = -- | Sent when the client is ready to receive data
    Initialized
  | -- | Get a feeds items
    GetFeedItems Int
  | -- | Sent when an item is considered read
    ItemRead Int
  | -- | Sent when all items of a fead are read
    FeedRead Int
  | -- | The server can't parse the message (should never
    --   happen since client and server share the exact same code)
    InvalidReceived
  | -- | Sent when adding a new feed (url + update every x minutes)
    NewFeed
      T.Text
      Int
  | -- | Force the server to download an rss feed
    ForceRefresh Int
  | -- | Get a single feeds information
    GetSingleFeedInfo Int
  | -- | Get a single feeds information
    UpdatedFeedInfo FeEditFeed
  deriving (Generic, Show)

instance Serialize Up

-- | Commands sent from server to client via websocket
data Down
  = -- | List of feeds
    Feeds [FeFeedInfo]
  | -- | Feed items
    FeedItems [FeItemInfo]
  | -- | Username of who is logged in obtained via auth token
    Status HeedUserName
  | -- | The client can't parse the message
    InvalidSent
  | -- | Confirm the feed was added
    FeedAdded T.Text
  | -- | Send a generic error string
    BackendError T.Text
  | -- | Send Feed update
    NewItems FeFeedInfo
  | -- | Send all Info for a feed
    EditableFeedInfo FeEditFeed
  | -- | Send new name
    FeedInfoUpdated (Int, T.Text)
  deriving (Generic, Show)

instance Serialize Down

-- | Authentication pair sent from the client
data AuthData = AuthData
  { username :: T.Text,
    password :: T.Text
  }
  deriving (Generic)

instance FromForm AuthData

instance Serialize AuthData

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
  }
  deriving (Generic, Show)

instance Serialize Token

instance MimeRender OctetStream Token where
  mimeRender _ = fromStrict . encode
