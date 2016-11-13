module Heed.Dispatcher where

import React.Flux
import Heed.FeedListStore
import Heed.ItemListStore

data Action
    = Feed FeedListAction
    | Item ItemListAction

selectFeed :: Int -> Action
selectFeed = Feed . SelectFeed

selectItem :: Int -> Action
selectItem = Item . SelectItem

dispatchHeed :: Action -> [SomeStoreAction]
dispatchHeed (Feed fla) = [SomeStoreAction feedListStore fla]
dispatchHeed (Item ita) = [SomeStoreAction itemListStore ita]
