{-# LANGUAGE OverloadedStrings #-}

module Heed.Views where

import Control.Lens hiding (view)
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
feedList_ flst = div_ ["className" $= "feedList"] $ mapM_ (feed_ $ _selectedFeed flst) $ _feedList flst

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
         do span_ ["className" $= "feedName"] $ elemText $ _feedListName feedInfo
            span_ ["className" $= "feedUnread"] $ elemText $ unreadText feedInfo

feed_ :: Maybe ReactFeedInfo -> ReactFeedInfo -> ReactElementM eventHandler ()
feed_ selId feedInfo = viewWithIKey (feed selId) (_feedListId feedInfo) feedInfo mempty

unreadText :: ReactFeedInfo -> T.Text
unreadText = T.pack . show . _feedListUnread

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
rItemList_ itst =
    div_ ["className" $= "itemList"] $ mapM_ (item_ $ _selectedItem itst) $ _itemList itst

rItemDetail_ :: ItemListStore -> ReactElementM ViewEventHandler ()
rItemDetail_ itst = view itemDetail (_selectedItem itst) mempty

itemDetail :: ReactView (Maybe ReactItemStatus)
itemDetail =
    defineView "detail" $
    \iinfo ->
         div_ ["className" $= "itemDetail"] $
         case iinfo of
             Nothing -> mempty
             Just status -> do
                 let info = _itemInfo status
                 a_ ["className" $= "detailName", "href" &= _itemInfoLink info] $ elemText $
                     _itemInfoTitle info
                 a_ ["className" $= "detailUrl", "href" &= _itemInfoLink info] $ elemText $
                     _itemInfoLink info

item_ :: Maybe ReactItemStatus -> ReactItemStatus -> ReactElementM eventHandler ()
item_ selItem info = viewWithIKey (item selItem) (info ^. itemInfo . itemInfoId) info mempty

item :: Maybe ReactItemStatus -> ReactView ReactItemStatus
item selItem =
    defineView "item info" $
    \status -> do
        let iinfo = _itemInfo status
        div_
            [ onClick (\_ _ -> dispatchHeed $ selectItem status)
            , classNames
                  [ ("selected", isSelected iinfo (_itemInfo <$> selItem))
                  , ("itemInfoItem", True)
                  , ("read", isRead $ _readStatus status)
                  ]
            ] $
            do span_ ["className" $= "itemName"] $ elemText $ _itemInfoTitle iinfo
               span_ ["className" $= "itemDate"] $ elemString $ showUtc (_itemInfoDate iinfo)

showUtc :: UTCTime -> String
showUtc = formatTime defaultTimeLocale "%T %D"

isRead :: ReadStatus -> Bool
isRead Unread = False
isRead Read = True
