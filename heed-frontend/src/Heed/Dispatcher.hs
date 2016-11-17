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

selectItem :: ReactItemInfo -> Action
selectItem = Item . SelectItem

nextFeed, nextItem, prevFeed, prevItem :: Action
nextFeed = Feed NextFeed

nextItem = Item NextItem

prevFeed = Feed PrevFeed

prevItem = Item PrevItem

dispatchHeed :: Action -> [SomeStoreAction]
dispatchHeed (Feed fla) = [SomeStoreAction feedListStore fla]
dispatchHeed (Item ita) = [SomeStoreAction itemListStore ita]
dispatchHeed Ignore = []
