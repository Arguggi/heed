{-# LANGUAGE TemplateHaskell #-}

module Heed.Vty.AppState where

import qualified Brick.Widgets.List as BL
import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Heed.Commands (FeFeedInfo, FeItemInfo)
import qualified Network.WebSockets as WS

data Name
    = StatusBar
    | FeedList
    | ItemList
    deriving (Ord, Show, Eq)

data AppState = AppState
    { _feeds :: BL.List Name FeFeedInfo
    , _items :: BL.List Name FeItemInfo
    , _userName :: T.Text
    , _wsConn :: WS.Connection
    , _status :: T.Text
    }

makeLenses ''AppState

defState :: T.Text -> WS.Connection -> T.Text -> AppState
defState = AppState (BL.list FeedList Vec.empty 2) (BL.list ItemList Vec.empty 2)
