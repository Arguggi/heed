module Heed.Commands
  ( Up(..)
  , Down(..)
  ) where

import Heed.Types
import Text.Feed.Types

data Up
    = ItemRead
    | AddFeed [Feed]

data Down
    = NewItems [Item]
    | Feeds [FeedInfo]
