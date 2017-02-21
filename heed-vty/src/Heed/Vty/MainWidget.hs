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
        (<=>))
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
import Heed.Utils (fork_)
import Heed.Vty.AddFeedWidget (addVty)
import Heed.Vty.WidgetStates
import qualified Network.WebSockets as WS
import qualified System.Process as Process
import Text.URI (parseURI, uriRegName)

newtype MyEvent =
    WsReceive Down

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
    selectedStyle $ hLimit infoNameMaxWidth (txt (a ^. feedListName)) <+>
    (padLeft BT.Max . str . show $ a ^. feedListUnread)
  where
    selectedStyle =
        if sel
            then withAttr "selected"
            else id
    -- Start from max width, subtract length of unread items (e.g. 88) and then a space
    infoNameMaxWidth = feedListWidth - (length . show $ a ^. feedListUnread) - 1

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

openTab :: FeItemInfo -> IO ()
openTab e = do
    forM_ (sameDomain link (e ^. itemInfoComments)) callBrowser
    callBrowser link
  where
    link = e ^. itemInfoLink
    callBrowser l =
        fork_ $
        Process.createProcess
            (browserProc l)
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.NoStream
            , Process.std_err = Process.NoStream
            }
    browserProc l = Process.proc "chromium" [T.unpack l]
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
handleMess s InvalidSent = M.continue s

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
