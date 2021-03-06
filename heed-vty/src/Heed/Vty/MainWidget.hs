{-# LANGUAGE OverloadedStrings #-}

module Heed.Vty.MainWidget
    ( app
    , insertInOrder
    , MyEvent(WsReceive)
    ) where

import qualified Brick.AttrMap as BA
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
       (hBox, hLimit, padLeft, str, txt, vBox, vLimit, withAttr, (<+>),
        (<=>), txtWrap)
import qualified Brick.Widgets.List as BL
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Serialize (encode)
import qualified Data.Text as T
import qualified Data.Time.Format as Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Heed.Commands
import Heed.Utils (fork_, silentProc)
import Heed.Vty.AddFeedWidget (addVty)
import Heed.Vty.EditFeedWidget (editVty)
import Heed.Vty.WidgetStates
import qualified Network.WebSockets as WS
import qualified System.Process as Process
import Text.URI (parseURI, uriRegName)

newtype MyEvent =
    WsReceive Down

data Browser = Firefox | Chromium

drawUi :: AppState -> [Widget Name]
drawUi s = [ui]
  where
    ui = C.center $ vBox [statusBar, B.hBorder, mainInfo]
    statusBar = txt ("Connected as " <> (s ^. userName)) <+> (padLeft BT.Max . txt $ s ^. status)
    mainInfo = hBox [feedListVty, B.vBorder, itemInfoVty]
    feedListVty = hLimit feedListWidth $ BL.renderList feedDrawElement True (s ^. feeds)
    itemInfoVty = vBox [itemListVty, B.hBorder, itemDetailVty]
    itemListVty = BL.renderList itemDrawElement True (s ^. items)
    itemDetailVty = vLimit 4 $ itemDrawDetail (BL.listSelectedElement $ s ^. items)

feedDrawElement :: Bool -> FeFeedInfo -> Widget Name
feedDrawElement sel a =
    selectedStyle $ txtWrap (a ^. feedListName) <+>
    (padLeft BT.Max . str . show $ a ^. feedListUnread)
  where
    selectedStyle =
        if sel
            then withAttr "selected"
            else id

itemDrawElement :: Bool -> FeItemInfo -> Widget Name
itemDrawElement sel a =
    selectedStyle $ txtWrap (a ^. itemInfoTitle) <+> (padLeft BT.Max . str . showTime $ a)
  where
    selectedStyle
        | sel = withAttr "selected"
        | a ^. itemInfoRead == Seen = withAttr "read"
        | otherwise = id
    showTime s = Time.formatTime euTimeLocale "%T %x" $ s ^. itemInfoDate

itemDrawDetail :: Maybe (Int, FeItemInfo) -> Widget Name
itemDrawDetail Nothing = txt "No item selected"
itemDrawDetail (Just (_, info)) =
    txtWrap (info ^. itemInfoTitle) <=> txtWrap (info ^. itemInfoLink) <=>
    txtWrap (fromMaybe "no comments" $ info ^. itemInfoComments)

appEvent :: AppState -> BT.BrickEvent Name MyEvent -> BT.EventM Name (BT.Next AppState)
-- Close app
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt s
appEvent s (BT.VtyEvent (V.EvKey V.KEsc [])) = M.halt s
-- Go down in feed list
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'J') [])) = do
    let s' = s & feeds %~ BL.listMoveDown
    getSelFeedItems s'
    M.continue s'
-- Go up in feed list
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'K') [])) = do
    let s' = s & feeds %~ BL.listMoveUp
    getSelFeedItems s'
    M.continue s'
-- Go down in item list and send read
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'j') [])) = do
    s' <- updateUnreadCount $ setItemAsRead s
    M.continue $ s' & items %~ BL.listMoveDown
-- Go up in item list and send read
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'k') [])) = do
    s' <- updateUnreadCount $ setItemAsRead s
    M.continue $ s' & items %~ BL.listMoveUp
-- Open link in chromium
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'o') [])) = openInBrowser s Chromium
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'O') [])) = openInBrowser s Firefox
-- Set all items as read
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'a') [])) = do
    s' <-
        liftIO $
        case BL.listSelectedElement (s ^. feeds) of
            Nothing -> return s
            Just (i, e) -> do
                sendAllRead s e
                return $ s & feeds . BL.listElementsL . ix i . feedListUnread .~ 0
    let s'' = s' & feeds %~ BL.listMoveDown
    getSelFeedItems s''
    M.continue s''
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'n') [])) = M.suspendAndResume $ addVty s
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'e') [])) = do
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. feeds)
    case sel of
        Nothing -> return ()
        Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (GetSingleFeedInfo (e ^. feedListId)))
    M.continue $ s & status .~ "Getting feed info to edit"
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'r') [])) = do
    let selectedFeedId = s ^. feeds . to BL.listSelectedElement ^? _Just . _2 . feedListId
        conn = s ^. wsConn
    s' <-
        case selectedFeedId of
            Nothing -> return s
            Just fid -> do
                fork_ $ WS.sendBinaryData conn (encode (ForceRefresh fid))
                return $ s & status .~ "Refreshing selected feed"
    M.continue s'
appEvent s (BT.AppEvent (WsReceive e)) = handleMess s e
appEvent s _ = M.continue s

openInBrowser :: AppState -> Browser -> BT.EventM Name (BT.Next AppState)
openInBrowser s browser = do
    liftIO $
        case BL.listSelectedElement (s ^. items) of
            Nothing -> return ()
            Just (_, e) -> openTab browser e
    s' <- updateUnreadCount $ setItemAsRead s
    M.continue $ s' & items %~ BL.listMoveDown

sendRead
    :: (MonadIO m)
    => AppState -> m ()
sendRead s =
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. items)
    in case sel of
           Nothing -> return ()
           Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (ItemRead (e ^. itemInfoId)))

sendAllRead
    :: (MonadIO m)
    => AppState -> FeFeedInfo -> m ()
sendAllRead s f = fork_ $ WS.sendBinaryData conn (encode (FeedRead (f ^. feedListId)))
  where
    conn = s ^. wsConn

updateUnreadCount
    :: MonadIO m
    => (Seen, AppState) -> m AppState
updateUnreadCount (Seen, s) = return s
updateUnreadCount (Unseen, s) = do
    sendRead s
    return $
        case s ^. feeds . BL.listSelectedL of
            Nothing -> s
            Just i -> s & feeds . BL.listElementsL . ix i . feedListUnread -~ 1

openTab :: Browser -> FeItemInfo -> IO ()
openTab browser e = do
    forM_ (sameDomain link (e ^. itemInfoComments)) (callBrowser selectedBrowser)
    callBrowser selectedBrowser link
  where
    link = e ^. itemInfoLink
    selectedBrowser = case browser of
        Chromium -> "chromium"
        Firefox -> "firefox"
    callBrowser brow url = fork_ $ Process.createProcess (silentProc brow [T.unpack url])
    -- If the comments are on the same domain we shouldn't bother opening them
    sameDomain l commentsM = do
        comments <- commentsM
        linkUri <- parseURI (T.unpack l)
        commentUri <- parseURI (T.unpack comments)
        linkDomain <- uriRegName linkUri
        commentDomain <- uriRegName commentUri
        if linkDomain == commentDomain
            then Nothing
            else Just comments

setItemAsRead :: AppState -> (Seen, AppState)
setItemAsRead s =
    let ind = s ^. items . BL.listSelectedL
        s' =
            case ind of
                Nothing -> (Seen, s)
                Just i -> s & items . BL.listElementsL . ix i . itemInfoRead <<.~ Seen
    in s'

handleMess :: AppState -> Down -> BT.EventM Name (BT.Next AppState)
handleMess s (Feeds fi) = do
    let s' = s & feeds .~ BL.list FeedList (Vec.fromList fi) 1
    getSelFeedItems s'
    M.continue s'
handleMess s (FeedItems fi) =
    M.continue $ s & items .~ BL.list ItemList (Vec.fromList fi) 1 & feeds .~ newCount
  where
    newCount = BL.listModify (feedListUnread .~ (fromIntegral . length) fi) (s ^. feeds)
handleMess s (Status name) = M.continue $ s & userName .~ name
handleMess s (FeedAdded url) = M.continue $ s & status .~ ("Added " <> url)
handleMess s (BackendError text) = M.continue $ s & status .~ ("Error: " <> text)
handleMess s (NewItems feed) = do
    when (needUpdate && sameAsSelected) (getSelFeedItems s)
    M.continue newState
  where
    (newState, needUpdate) =
        case Vec.elemIndex feed (s ^. feeds . BL.listElementsL) of
            Nothing -> (s & feeds %~ insertInOrder feed, False)
            Just i ->
                ( s & feeds . BL.listElementsL . ix i . feedListUnread +~ (feed ^. feedListUnread)
                , True)
    sameAsSelected = Just feed == (s ^. feeds . to BL.listSelectedElement ^? _Just . _2)
handleMess s (EditableFeedInfo feed) = M.suspendAndResume $ editVty s feed
handleMess s (FeedInfoUpdated (fid, name)) = M.continue $ updateName s name fid
handleMess s InvalidSent = M.continue s

updateName :: AppState -> T.Text -> Int -> AppState
updateName s name fid =
    case Vec.findIndex (\x -> x ^. feedListId == fid) (s ^. feeds . BL.listElementsL) of
        Nothing -> s
        Just ind -> s & feeds . BL.listElementsL . ix ind . feedListName .~ name

insertInOrder
    :: Ord e
    => e -> BL.List n e -> BL.List n e
insertInOrder newFeed feedList = BL.listInsert pos newFeed feedList
  where
    pos = length $ Vec.takeWhile (< newFeed) (feedList ^. BL.listElementsL)

getSelFeedItems
    :: (MonadIO m)
    => AppState -> m ()
getSelFeedItems s = do
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. feeds)
    case sel of
        Nothing -> return ()
        Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (GetFeedItems (e ^. feedListId)))
    return ()

getInfo :: AppState -> BT.EventM Name AppState
getInfo s = do
    let conn = _wsConn s
    fork_ $ WS.sendBinaryData conn (encode Initialized)
    return $ s & status .~ "Connected to server"

app :: M.App AppState MyEvent Name
app =
    M.App
    { M.appDraw = drawUi
    , M.appStartEvent = getInfo
    , M.appHandleEvent = appEvent
    , M.appAttrMap = const $ BA.attrMap V.defAttr myAttrs
    , M.appChooseCursor = M.neverShowCursor
    }

myAttrs :: [(BA.AttrName, V.Attr)]
myAttrs = [("selected", BU.fg V.white), ("read", BU.fg V.red)]

euTimeLocale :: Time.TimeLocale
euTimeLocale = Time.defaultTimeLocale {Time.dateFmt = "%d/%m/%y"}

feedListWidth :: Int
feedListWidth = 50
