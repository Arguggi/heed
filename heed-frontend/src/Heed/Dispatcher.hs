module Heed.Dispatcher where

import Heed.Commands
import Heed.FeedListStore
import Heed.ItemListStore
import React.Flux

data Action
    = Feed FeedListAction
    | Item ItemListAction
    | Ignore

selectFeed :: ReactFeedInfo -> Action
selectFeed = Feed . SelectFeed

selectItem :: ReactItemStatus -> Action
selectItem = Item . SelectItem

nextFeed, nextItem, prevFeed, prevItem, openItem :: Action
nextFeed = Feed NextFeed

nextItem = Item NextItem

prevFeed = Feed PrevFeed

prevItem = Item PrevItem

openItem = Item OpenItem

dispatchHeed :: Action -> [SomeStoreAction]
dispatchHeed (Feed fla) = [SomeStoreAction feedListStore fla]
dispatchHeed (Item ita) = [SomeStoreAction itemListStore ita]
dispatchHeed Ignore = []
