{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Commands
    ( AuthData(..)
    , Down(..)
    , FeEditFeed
    , FeEditFeed'(..)
    , FeFeedInfo
    , FeFeedInfo'(..)
    , FeFeedInfoR
    , FeItemInfo
    , FeItemInfo'(..)
    , FeItemInfoR
    , Seen(..)
    , Token(..)
    , Up(..)
    , fromBool
    , toBool
    -- Feed Info Lenses
    , feedListId
    , feedListName
    , feedListUnread
    -- * Feed Item Lenses
    , itemInfoId
    , itemInfoTitle
    , itemInfoLink
    , itemInfoDate
    , itemInfoComments
    , itemInfoRead
    , toFeEditFeed
    -- * EditableFeedInfo Lenses
    , feEditId
    , feEditName
    , feEditUpdateEvery
    ) where

import Control.Lens
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Serialize (Serialize, decode, encode)
import Data.Serialize.Text ()
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Heed.Database as DB
import Heed.Orphans ()
import qualified Opaleye as O
import Servant.API.ContentTypes
       (MimeRender(..), MimeUnrender(..), OctetStream)
import Test.QuickCheck (Arbitrary(..))
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

instance Serialize Seen

instance Arbitrary Seen where
    arbitrary = fromBool <$> arbitrary

-- | List of feeds sent to the client
data FeFeedInfo' a b c = FeFeedInfo'
    { _feedListId :: a -- ^ Postgresql feed id
    , _feedListName :: b -- ^ Feed name
    , _feedListUnread :: c -- ^ Number of unread items
    } deriving (Generic, Show)

-- | Will be sent to client
type FeFeedInfo = FeFeedInfo' Int T.Text Int64

makeLenses ''FeFeedInfo'

instance (Eq a) =>
         Eq (FeFeedInfo' a b c) where
    a == b = (a ^. feedListId) == (b ^. feedListId)

instance (Eq a, Ord b) =>
         Ord (FeFeedInfo' a b c) where
    a `compare` b = (a ^. feedListName) `compare` (b ^. feedListName)

instance (Serialize a, Serialize b, Serialize c) =>
         Serialize (FeFeedInfo' a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (FeFeedInfo' a b c) where
    arbitrary = FeFeedInfo' <$> arbitrary <*> arbitrary <*> arbitrary

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

instance (Eq a) =>
         Eq (FeItemInfo' a b c d e f) where
    a == b = _itemInfoId a == _itemInfoId b

data FeEditFeed' a b c = FeEditFeed'
    { _feEditId :: a
    , _feEditName :: b
    , _feEditUpdateEvery :: c
    } deriving (Show, Generic)

makeLenses ''FeEditFeed'

type FeEditFeed = FeEditFeed' Int T.Text Int

toFeEditFeed :: DB.FeedInfoHR -> FeEditFeed
toFeEditFeed feed =
    FeEditFeed'
        (feed ^. DB.feedInfoId . DB.getFeedInfoId)
        (feed ^. DB.feedInfoName)
        (feed ^. DB.feedInfoUpdateEvery)

instance (Serialize a, Serialize b, Serialize c) =>
         Serialize (FeEditFeed' a b c)

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f) =>
         Serialize (FeItemInfo' a b c d e f)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) =>
         Arbitrary (FeItemInfo' a b c d e f) where
    arbitrary =
        FeItemInfo' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary

$(makeAdaptorAndInstance "pFeFeedInfo" ''FeFeedInfo')

type FeFeedInfoR = FeFeedInfo' (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGInt8)

$(makeAdaptorAndInstance "pFeItemInfo" ''FeItemInfo')

type FeItemInfoR = FeItemInfo' (O.Column O.PGInt4) (O.Column O.PGText) (O.Column O.PGText) (O.Column O.PGTimestamptz) (O.Column (O.Nullable O.PGText)) (O.Column O.PGBool)

instance O.QueryRunnerColumnDefault O.PGBool Seen where
    queryRunnerColumnDefault = fromBool <$> O.fieldQueryRunnerColumn

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
    | NewFeed T.Text
              Int -- ^ Sent when adding a new feed (url + update every x minutes)
    | ForceRefresh Int -- ^ Force the server to download an rss feed
    | GetSingleFeedInfo Int -- ^ Get a single feeds information
    | UpdatedFeedInfo FeEditFeed -- ^ Get a single feeds information
    deriving (Generic, Show)

instance Serialize Up

-- | Commands sent from server to client via websocket
data Down
    = Feeds [FeFeedInfo] -- ^ List of feeds
    | FeedItems [FeItemInfo] -- ^ Feed items
    | Status HeedUserName -- ^ Username of who is logged in obtained via auth token
    | InvalidSent -- ^ The client can't parse the message
    | FeedAdded T.Text -- ^ Confirm the feed was added
    | BackendError T.Text -- ^ Send a generic error string
    | NewItems FeFeedInfo -- ^ Send Feed update
    | EditableFeedInfo FeEditFeed -- ^ Send all Info for a feed
    | FeedInfoUpdated (Int, T.Text) -- ^ Send new name
    deriving (Generic, Show)

instance Serialize Down

data AuthData = AuthData
    { username :: T.Text
    , password :: T.Text
    } deriving (Generic)

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
    } deriving (Generic, Show)

instance Serialize Token

instance MimeRender OctetStream Token where
    mimeRender _ = fromStrict . encode
