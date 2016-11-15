{-# LANGUAGE OverloadedStrings #-}

module Heed.Views where

import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Heed.Commands
import Heed.Dispatcher
import Heed.FeedListStore
import Heed.ItemListStore
import React.Flux

-- | The controller view and also the top level of the Heed app.
heedApp :: ReactView ()
heedApp =
    defineView "Heed app" $
    \() -> do
        heedHeader_
        container_

--heedFeeds_
--feedItems_
container_ :: ReactElementM eventHandler ()
container_ = view container () mempty

container :: ReactView ()
container =
    defineView "container" $
    \() ->
         div_ ["className" $= "container"] $
         do heedFeeds_
            feedItems_

---- Header
heedHeader_ :: ReactElementM eventHandler ()
heedHeader_ = view heedHeader () mempty

heedHeader :: ReactView ()
heedHeader = defineView "header" $ \() -> header_ $ h1_ "Heed"

---- Feed List
heedFeeds_ :: ReactElementM eventHandler ()
heedFeeds_ = view heedFeeds () mempty

heedFeeds :: ReactView ()
heedFeeds = defineControllerView "feed list" feedListStore $ \feedLStore _ -> feedList_ feedLStore

feedList_ :: FeedListStore -> ReactElementM ViewEventHandler ()
feedList_ flst = div_ ["className" $= "feedList"] $ mapM_ (feed_ $ selectedFeed flst) $ feedList flst

---- Feed List item
--
feed :: Maybe Int -> ReactView ReactFeedInfo
feed selectedId =
    defineView "feed info" $
    \feedInfo ->
         div_
             [ onClick (\_ _ -> dispatchHeed $ selectFeed (feedListId feedInfo))
             , classNames
                   [ ("selected", isSelected (feedListId feedInfo) selectedId)
                   , ("feedInfoItem", True)
                   ]
             ] $
         do span_ ["className" $= "feedName"] $ elemText $ feedListName feedInfo
            span_ ["className" $= "feedUnread"] $ elemText $ unreadText feedInfo

feed_ :: Maybe Int -> ReactFeedInfo -> ReactElementM eventHandler ()
feed_ selId feedInfo = viewWithIKey (feed selId) (feedListId feedInfo) feedInfo mempty

unreadText :: ReactFeedInfo -> T.Text
unreadText = T.pack . show . feedListUnread

feedItems_ :: ReactElementM eventHandler ()
feedItems_ = view feedItems () mempty

feedItems :: ReactView ()
feedItems = defineControllerView "item list" itemListStore $ \itemLStore _ -> rItemList_ itemLStore

rItemList_ :: ItemListStore -> ReactElementM ViewEventHandler ()
rItemList_ itst = div_ ["className" $= "itemList"] $ mapM_ (item_ $ selectedItem itst) $ itemList itst

item_ :: Maybe Int -> ReactItemInfo -> ReactElementM eventHandler ()
item_ selId itemInfo = viewWithIKey (item selId) (itemInfoId itemInfo) itemInfo mempty

item :: Maybe Int -> ReactView ReactItemInfo
item selectedId =
    defineView "item info" $
    \itemInfo ->
         div_
             [ onClick (\_ _ -> dispatchHeed $ selectItem (itemInfoId itemInfo))
             , classNames
                   [ ("selected", isSelected (itemInfoId itemInfo) selectedId)
                   , ("itemInfoItem", True)
                   ]
             ] $
         do span_ ["className" $= "itemName"] $ elemText $ itemInfoTitle itemInfo
            span_ ["className" $= "itemDate"] $ elemString $ showUtc (itemInfoDate itemInfo)

isSelected :: Int -> Maybe Int -> Bool
isSelected elementId selId = Just elementId == selId

showUtc :: UTCTime -> String
showUtc = formatTime defaultTimeLocale "%T %D"
