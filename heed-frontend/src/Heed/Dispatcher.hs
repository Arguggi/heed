module Heed.Dispatcher where

import Heed.Commands
import Heed.FeedListStore
import Heed.ItemListStore
import React.Flux

data Action
    = Feed FeedListAction
    | Item ItemListAction

selectFeed :: ReactFeedInfo -> Action
selectFeed = Feed . SelectFeed

selectItem :: ReactItemInfo -> Action
selectItem = Item . SelectItem

dispatchHeed :: Action -> [SomeStoreAction]
dispatchHeed (Feed fla) = [SomeStoreAction feedListStore fla]
dispatchHeed (Item ita) = [SomeStoreAction itemListStore ita]
