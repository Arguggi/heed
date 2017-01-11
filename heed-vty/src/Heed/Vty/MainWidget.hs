{-# LANGUAGE OverloadedStrings #-}

module Heed.Vty.MainWidget where

import qualified Brick.AttrMap as BA
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
       (hBox, hLimit, padLeft, str, txt, vBox, vLimit, withAttr, (<+>),
        (<=>))
import qualified Brick.Widgets.List as BL
import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Store (encode)
import qualified Data.Text as T
import qualified Data.Time.Format as Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Heed.Commands
import Heed.Vty.AddFeedWidget (addVty)
import Heed.Vty.WidgetStates
import qualified Network.WebSockets as WS
import qualified System.Process as Process

newtype MyEvent =
    WsReceive Down

drawUi :: AppState -> [Widget Name]
drawUi s = [ui]
  where
    ui = C.center $ vBox [statusBar, B.hBorder, mainInfo]
    statusBar = txt ("Connected as " <> (s ^. userName)) <+> (padLeft BT.Max . txt $ s ^. status)
    mainInfo = hBox [feedListVty, B.vBorder, itemInfoVty]
    feedListVty = hLimit 50 $ BL.renderList feedDrawElement True (s ^. feeds)
    itemInfoVty = vBox [itemListVty, B.hBorder, itemDetailVty]
    itemListVty = BL.renderList itemDrawElement True (s ^. items)
    itemDetailVty = vLimit 4 $ itemDrawDetail (BL.listSelectedElement $ s ^. items)

feedDrawElement :: Bool -> FeFeedInfo -> Widget Name
feedDrawElement sel a =
    selectedStyle $ txt (a ^. feedListName) <+> (padLeft BT.Max . str . show $ a ^. feedListUnread)
  where
    selectedStyle =
        if sel
            then withAttr "selected"
            else id

itemDrawElement :: Bool -> FeItemInfo -> Widget Name
itemDrawElement sel a =
    selectedStyle $ txt (a ^. itemInfoTitle) <+> (padLeft BT.Max . str . showTime $ a)
  where
    selectedStyle
        | sel = withAttr "selected"
        | a ^. itemInfoRead == Seen = withAttr "read"
        | otherwise = id
    showTime s = Time.formatTime euTimeLocale "%T %x" $ s ^. itemInfoDate

itemDrawDetail :: Maybe (Int, FeItemInfo) -> Widget Name
itemDrawDetail Nothing = txt "No item selected"
itemDrawDetail (Just (_, info)) =
    txt (info ^. itemInfoTitle) <=> txt (info ^. itemInfoLink) <=>
    txt (fromMaybe "no comments" $ info ^. itemInfoComments)

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
appEvent s (BT.VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    liftIO $
        case BL.listSelectedElement (s ^. items) of
            Nothing -> return ()
            Just (_, e) -> openTab e
    s' <- updateUnreadCount $ setItemAsRead s
    M.continue $ s' & items %~ BL.listMoveDown
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
appEvent s (BT.AppEvent (WsReceive e)) = handleMess s e
appEvent s _ = M.continue s

sendRead
    :: (MonadIO m)
    => AppState -> m ()
sendRead s =
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. items)
    in case sel of
           Nothing -> return ()
           Just (_, e) ->
               void . liftIO . forkIO $ WS.sendBinaryData conn (encode (ItemRead (e ^. itemInfoId)))

sendAllRead
    :: (MonadIO m)
    => AppState -> FeFeedInfo -> m ()
sendAllRead s f =
    void . liftIO . forkIO $ WS.sendBinaryData conn (encode (FeedRead (f ^. feedListId)))
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

openTab :: FeItemInfo -> IO ()
openTab e = do
    forM_ (e ^. itemInfoComments) callBrowser
    callBrowser $ e ^. itemInfoLink
  where
    callBrowser link =
        void . forkIO . void $
        Process.createProcess
            (browserProc link)
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.NoStream
            , Process.std_err = Process.NoStream
            }
    browserProc link = Process.proc "chromium" [T.unpack link]

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
handleMess s (FeedItems fi) = M.continue $ s & items .~ BL.list ItemList (Vec.fromList fi) 1
handleMess s (Status name) = M.continue $ s & userName .~ name
handleMess s (FeedAdded url) = M.continue $ s & status .~ (url <> " added")
handleMess s (BackendError text) = M.continue $ s & status .~ ("Error: " <> text)
handleMess s InvalidSent = M.continue s

getSelFeedItems
    :: (MonadIO m)
    => AppState -> m ()
getSelFeedItems s = do
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. feeds)
    case sel of
        Nothing -> return ()
        Just (_, e) ->
            void . liftIO . forkIO $
            WS.sendBinaryData conn (encode (GetFeedItems (e ^. feedListId)))
    return ()

getInfo :: AppState -> BT.EventM Name AppState
getInfo s = do
    let conn = _wsConn s
    _ <- liftIO . forkIO $ WS.sendBinaryData conn (encode Initialized)
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
euTimeLocale =
    Time.defaultTimeLocale
    { Time.dateFmt = "%d/%m/%y"
    }