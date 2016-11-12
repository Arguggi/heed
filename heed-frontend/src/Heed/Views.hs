{-# LANGUAGE OverloadedStrings #-}

module Heed.Views where

import React.Flux
import qualified Data.Text as T
import Heed.FeedListStore
import Heed.Commands
import Heed.Dispatcher

-- | The controller view and also the top level of the Heed app.
heedApp :: ReactView ()
heedApp =
    defineControllerView "Heed app" feedListStore $
    \feedLStore () ->
         div_ $
         do heedHeader_
            div_ $ feedList_ feedLStore

heedHeader_ :: ReactElementM eventHandler ()
heedHeader_ = view heedHeader () mempty

heedHeader :: ReactView ()
heedHeader = defineView "header" $ \() -> header_ $ h1_ "Heed"

feedList_ :: FeedListStore -> ReactElementM ViewEventHandler ()
feedList_ flst = div_ ["className" $= "feedList"] $ mapM_ (feed_ $ selected flst) $ feedList flst

feed :: Maybe Int -> ReactView ReactFeedInfo
feed selectedId =
    defineView "feed info" $
    \feedInfo ->
         div_
             [ onClick (\_ _ -> dispatchHeed $ SetSelected (feedListId feedInfo))
             , classNames
                   [ ("selected", isSelected (feedListId feedInfo) selectedId)
                   , ("feedInfoItem", True)
                   ]
             ] $
         do span_ ["className" $= "feedName"] $ elemText $ feedListName feedInfo
            span_ ["className" $= "feedUnread"] $ elemText $ unreadText feedInfo
  where
    isSelected feedId selId = Just feedId == selId

feed_ :: Maybe Int -> ReactFeedInfo -> ReactElementM eventHandler ()
feed_ selId feedInfo = viewWithIKey (feed selId) (feedListId feedInfo) feedInfo mempty

unreadText :: ReactFeedInfo -> T.Text
unreadText = T.pack . show . feedListUnread
