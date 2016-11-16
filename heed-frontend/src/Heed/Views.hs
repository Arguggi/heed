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
feed :: Maybe ReactFeedInfo -> ReactView ReactFeedInfo
feed selFeed =
    defineView "feed info" $
    \feedInfo ->
         div_
             [ onClick (\_ _ -> dispatchHeed $ selectFeed feedInfo)
             , classNames [("selected", isSelected feedInfo selFeed), ("feedInfoItem", True)]
             ] $
         do span_ ["className" $= "feedName"] $ elemText $ feedListName feedInfo
            span_ ["className" $= "feedUnread"] $ elemText $ unreadText feedInfo

feed_ :: Maybe ReactFeedInfo -> ReactFeedInfo -> ReactElementM eventHandler ()
feed_ selId feedInfo = viewWithIKey (feed selId) (feedListId feedInfo) feedInfo mempty

unreadText :: ReactFeedInfo -> T.Text
unreadText = T.pack . show . feedListUnread

feedItems_ :: ReactElementM eventHandler ()
feedItems_ = view feedItems () mempty

feedItems :: ReactView ()
feedItems =
    defineControllerView "item list" itemListStore $
    \itemLStore _ ->
         div_ ["className" $= "itemsContainer"] $
         do rItemList_ itemLStore
            rItemDetail_ itemLStore

rItemList_ :: ItemListStore -> ReactElementM ViewEventHandler ()
rItemList_ itst = div_ ["className" $= "itemList"] $ mapM_ (item_ $ selectedItem itst) $ itemList itst

rItemDetail_ :: ItemListStore -> ReactElementM ViewEventHandler ()
rItemDetail_ itst = view itemDetail (selectedItem itst) mempty

itemDetail :: ReactView (Maybe ReactItemInfo)
itemDetail =
    defineView "detail" $
    \itemInfo ->
         div_ ["className" $= "itemDetail"] $
         case itemInfo of
             Nothing -> mempty
             Just info -> do
                 a_ ["className" $= "detailName", "href" &= itemInfoLink info] $
                     elemText $ itemInfoTitle info
                 a_ ["className" $= "detailUrl", "href" &= itemInfoLink info] $
                     elemText $ itemInfoLink info

item_ :: Maybe ReactItemInfo -> ReactItemInfo -> ReactElementM eventHandler ()
item_ selItem itemInfo = viewWithIKey (item selItem) (itemInfoId itemInfo) itemInfo mempty

item :: Maybe ReactItemInfo -> ReactView ReactItemInfo
item selItem =
    defineView "item info" $
    \itemInfo ->
         div_
             [ onClick (\_ _ -> dispatchHeed $ selectItem itemInfo)
             , classNames [("selected", isSelected itemInfo selItem), ("itemInfoItem", True)]
             ] $
         do span_ ["className" $= "itemName"] $ elemText $ itemInfoTitle itemInfo
            span_ ["className" $= "itemDate"] $ elemString $ showUtc (itemInfoDate itemInfo)

showUtc :: UTCTime -> String
showUtc = formatTime defaultTimeLocale "%T %D"
