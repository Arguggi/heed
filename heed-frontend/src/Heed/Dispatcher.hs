module Heed.Dispatcher where

import Heed.Commands
import Heed.FeedListStore
import Heed.ItemListStore
import React.Flux

-- | Wrap 'FeedListAction' and 'ItemListAction'
data Action
    = Feed FeedListAction -- ^ Action for 'FeedListStore'
    | Item ItemListAction -- ^ Action for 'ItemListStore'
    | Ignore -- ^ Do nothing

-- | Wrap 'SelectFeed' into an 'Action'
selectFeed :: ReactFeedInfo -> Action
selectFeed = Feed . SelectFeed

-- | Wrap 'SelectItem' into an 'Action'
selectItem :: ReactItemStatus -> Action
selectItem = Item . SelectItem

-- | Wrap actions
nextFeed, nextItem, prevFeed, prevItem, openItem :: Action
nextFeed = Feed NextFeed

nextItem = Item NextItem

prevFeed = Feed PrevFeed

prevItem = Item PrevItem

openItem = Item OpenItem

-- | Decide where to send actions
dispatchHeed :: Action -> [SomeStoreAction]
dispatchHeed (Feed fla) = [SomeStoreAction feedListStore fla]
dispatchHeed (Item ita) = [SomeStoreAction itemListStore ita]
dispatchHeed Ignore = []
