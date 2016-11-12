module Heed.Dispatcher
  ( dispatchHeed
  ) where

import React.Flux
import Heed.FeedListStore

dispatchHeed :: FeedListAction -> [SomeStoreAction]
dispatchHeed a = [SomeStoreAction feedListStore a]
