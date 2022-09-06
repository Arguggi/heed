{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Vty.WidgetStates
  ( AddName (..),
    AddState (..),
    AppState (..),
    DataValid (..),
    EditName (..),
    EditState (..),
    MyWebsocketException (..),
    Name (..),
    defState,
    isValidUpdateInterval,

    -- * 'AppState' lenses
    feeds,
    items,
    userName,
    wsConn,
    status,

    -- * 'AddState' Lenses
    addFocusRing,
    urlEdit,
    addUpdateEdit,
    urlMessage,
    addUpdateMessage,
    addAdding,
    addWsConn,

    -- * 'EditState' Lenses
    editFeedId,
    editFocusRing,
    nameEdit,
    editUpdateEdit,
    nameMessage,
    editUpdateMessage,
    editAdding,
    editWsConn,
  )
where

import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as BL
import Control.Exception (Exception)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as Vec
import Heed.Commands (FeFeedInfo, FeItemInfo)
import Lens.Micro.Platform (makeLenses)
import qualified Network.WebSockets as WS
import Text.Read (readEither)

data Name
  = StatusBar
  | FeedList
  | ItemList
  deriving (Ord, Show, Eq)

data AppState = AppState
  { _feeds :: BL.List Name FeFeedInfo,
    _items :: BL.List Name FeItemInfo,
    _userName :: T.Text,
    _wsConn :: WS.Connection,
    _status :: T.Text
  }

makeLenses ''AppState

defState :: T.Text -> WS.Connection -> T.Text -> AppState
defState = AppState (BL.list FeedList Vec.empty 2) (BL.list ItemList Vec.empty 2)

data AddName
  = UrlEdit
  | UpdateEdit
  deriving (Ord, Show, Eq)

data AddState = AddState
  { _addFocusRing :: F.FocusRing AddName,
    _urlEdit :: E.Editor T.Text AddName,
    _addUpdateEdit :: E.Editor T.Text AddName,
    _urlMessage :: T.Text,
    _addUpdateMessage :: T.Text,
    _addAdding :: Bool,
    _addWsConn :: WS.Connection
  }

makeLenses ''AddState

data EditName
  = NameEdit
  | UpdateEveryEdit
  deriving (Ord, Show, Eq)

data EditState = EditState
  { _editFeedId :: Int,
    _editFocusRing :: F.FocusRing EditName,
    _nameEdit :: E.Editor T.Text EditName,
    _editUpdateEdit :: E.Editor T.Text EditName,
    _nameMessage :: T.Text,
    _editUpdateMessage :: T.Text,
    _editAdding :: Bool,
    _editWsConn :: WS.Connection
  }

makeLenses ''EditState

data MyWebsocketException
  = DeadConnection
  | InvalidDataOnWs
  deriving (Show, Typeable)

isValidUpdateInterval :: T.Text -> Maybe T.Text
isValidUpdateInterval update =
  case readEither (T.unpack update) of
    Left _ -> Just "Not a valid number"
    Right int ->
      if int < (0 :: Int)
        then Just "Must be positive"
        else Nothing

instance Exception MyWebsocketException

data DataValid
  = Valid
  | InvalidLeft T.Text
  | InvalidRight T.Text
  | BothInvalid
      T.Text
      T.Text
