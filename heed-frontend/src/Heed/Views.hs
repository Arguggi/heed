{-# LANGUAGE OverloadedStrings #-}

module Heed.Views where

import React.Flux
import Data.Monoid
import qualified Data.Text as T

import Heed.FeedListStore
import Heed.Commands

-- | The controller view and also the top level of the Heed app.
heedApp :: ReactView ()
heedApp = defineControllerView "Heed app" feedListStore $ \feedLStore () ->
    div_ $ do
        heedHeader_
        div_ [ "id" $= "container"] $ feedList_ feedLStore

heedHeader_ :: ReactElementM eventHandler ()
heedHeader_ = view heedHeader () mempty

heedHeader :: ReactView ()
heedHeader = defineView "header" $ \() ->
    header_ [ "id" $= "header"] $ h1_ "Heed"

feedList_ :: FeedListStore -> ReactElementM ViewEventHandler ()
feedList_ flst = div_ [ "id" $= "feeds" ] $
    ul_ [ "id" $= "feedList" ] $ mapM_ feed_ $ feedList flst

feed :: ReactView ReactFeedInfo
feed = defineView "feed info" $ \feedinfo ->
    li_ $ elemText (showFeedInfo feedinfo)

feed_ :: ReactFeedInfo -> ReactElementM eventHandler ()
feed_ feedInfo = viewWithIKey feed (feedListId feedInfo) feedInfo mempty

showFeedInfo :: ReactFeedInfo -> T.Text
showFeedInfo feedInfo = showId <> showName <> showUnread
    where showId = T.pack . show . feedListId $ feedInfo
          showName = feedListName feedInfo
          showUnread = T.pack . show . feedListUnread $ feedInfo
