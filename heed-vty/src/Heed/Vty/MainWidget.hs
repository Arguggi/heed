{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Vty.MainWidget
    ( app
    , insertInOrder
    , MyEvent(WsReceive)
    ) where

import qualified Brick.AttrMap as BA
import qualified Brick.Main as M
import Brick (attrName)
import Brick.Types (Widget)
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
       (hBox, hLimit, padLeft, padRight, str, txt, vBox, vLimit, withAttr, (<+>),
        (<=>), txtWrap, Padding(..))
import qualified Brick.Widgets.List as BL
import Lens.Micro.Platform
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
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

data Browser = Firefox

selectedAttr :: BA.AttrName
selectedAttr = attrName "selected"

readAttr :: BA.AttrName
readAttr = attrName "read"

drawUi :: AppState -> [Widget Name]
drawUi s = [ui]
  where
    ui = C.center $ vBox [statusBar, B.hBorder, mainInfo]
    statusBar = txt ("Connected as " <> (s ^. userName)) <+> (padLeft Max . txt $ s ^. status)
    mainInfo = hBox [feedListVty, B.vBorder, itemInfoVty]
    feedListVty = hLimit feedListWidth $ BL.renderList feedDrawElement True (s ^. feeds)
    itemInfoVty = vBox [itemListVty, B.hBorder, itemDetailVty]
    itemListVty = BL.renderList itemDrawElement True (s ^. items)
    itemDetailVty = vLimit 4 $ itemDrawDetail (BL.listSelectedElement $ s ^. items)

feedDrawElement :: Bool -> FeFeedInfo -> Widget Name
feedDrawElement sel a = selectedStyle $
    padRight Max (txtWrap (a ^. feedListName)) <+>
    (padLeft Max . str . show $ a ^. feedListUnread)
  where
    selectedStyle =
        if sel
            then withAttr selectedAttr
            else id

itemDrawElement :: Bool -> FeItemInfo -> Widget Name
itemDrawElement sel a =
    selectedStyle $ txtWrap (a ^. itemInfoTitle) <+> (padLeft Max . str . showTime $ a)
  where
    selectedStyle
        | sel = withAttr selectedAttr
        | a ^. itemInfoRead == Seen = withAttr readAttr
        | otherwise = id
    showTime s = Time.formatTime euTimeLocale "%T %x" $ s ^. itemInfoDate

itemDrawDetail :: Maybe (Int, FeItemInfo) -> Widget Name
itemDrawDetail Nothing = txt "No item selected"
itemDrawDetail (Just (_, info)) =
    txtWrap (info ^. itemInfoTitle) <=> txtWrap (info ^. itemInfoLink) <=>
    txtWrap (fromMaybe "no comments" $ info ^. itemInfoComments)

appEvent :: BT.BrickEvent Name MyEvent -> BT.EventM Name AppState ()
-- Close app
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (BT.VtyEvent (V.EvKey V.KEsc [])) = M.halt
-- Go down in feed list
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'J') [])) = do
    feeds %= BL.listMoveDown
    getSelFeedItems
-- Go up in feed list
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'K') [])) = do
    feeds %= BL.listMoveUp
    getSelFeedItems
-- Go down in item list and send read
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'j') [])) = do
    setItemAsRead >>= updateUnreadCount
    items %= BL.listMoveDown
-- Go up in item list and send read
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'k') [])) = do
    setItemAsRead >>= updateUnreadCount
    items %= BL.listMoveUp
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'o') [])) = openInBrowser Firefox
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'O') [])) = openInBrowser Firefox
-- Set all items as read
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'a') [])) = do
    s <- BT.get
    _ <- case BL.listSelectedElement (s ^. feeds) of
        Nothing -> return ()
        Just (i, e) -> do
            sendAllRead e
            (feeds . BL.listElementsL . ix i . feedListUnread) .= 0
    feeds %= BL.listMoveDown
    getSelFeedItems
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    s <- BT.get
    M.suspendAndResume (addVty s)
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'e') [])) = do
    s <- BT.get
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. feeds)
    case sel of
        Nothing -> return ()
        Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (GetSingleFeedInfo (e ^. feedListId)))
    status .= "Getting feed info to edit"
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'r') [])) = do
    s :: AppState <- BT.get
    let selectedFeedId = s ^. feeds . to BL.listSelectedElement ^? _Just . _2 . feedListId
        conn = s ^. wsConn
    case selectedFeedId of
        Nothing -> return ()
        Just fid -> do
            fork_ $ WS.sendBinaryData conn (encode (ForceRefresh fid))
            status .= "Refreshing selected feed"
appEvent (BT.VtyEvent (V.EvKey (V.KChar 'c') [])) = do
    feeds . BL.listElementsL %= Vec.filter ((/= 0) . _feedListUnread)
    feeds %= BL.listMoveTo 0
    getSelFeedItems
appEvent (BT.AppEvent (WsReceive e)) = handleMess e
appEvent _ = return ()

openInBrowser :: Browser -> BT.EventM Name AppState ()
openInBrowser browser = do
    s <- BT.get
    liftIO $
        case BL.listSelectedElement (s ^. items) of
            Nothing -> return ()
            Just (_, e) -> openTab browser e
    setItemAsRead >>= updateUnreadCount
    items %= BL.listMoveDown

sendRead :: BT.EventM Name AppState ()
sendRead = do
    s <- BT.get
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. items)
    case sel of
           Nothing -> return ()
           Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (ItemRead (e ^. itemInfoId)))

sendAllRead :: FeFeedInfo -> BT.EventM Name AppState ()
sendAllRead f = do
    s <- BT.get
    fork_ $ WS.sendBinaryData (s ^. wsConn) (encode (FeedRead (f ^. feedListId)))

updateUnreadCount :: Seen -> BT.EventM Name AppState ()
updateUnreadCount Seen = return ()
updateUnreadCount Unseen = do
    sendRead
    s <- BT.get
    case s ^. feeds . BL.listSelectedL of
        Nothing -> return ()
        Just i -> feeds . BL.listElementsL . ix i . feedListUnread -= 1

openTab :: Browser -> FeItemInfo -> IO ()
openTab browser e = do
    forM_ (sameDomain link (e ^. itemInfoComments)) (callBrowser selectedBrowser)
    callBrowser selectedBrowser link
  where
    link = e ^. itemInfoLink
    selectedBrowser = case browser of
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

setItemAsRead :: BT.EventM Name AppState Seen
setItemAsRead = do
    s <- BT.get
    let ind = s ^. items . BL.listSelectedL
        (seen, s')  =
            case ind of
                Nothing -> (Seen, s)
                Just i -> s & items . BL.listElementsL . ix i . itemInfoRead <<.~ Seen
    BT.put s'
    return seen

handleMess :: Down -> BT.EventM Name AppState ()
handleMess (Feeds fi) = do
    feeds .= BL.list FeedList (Vec.fromList fi) 1
    getSelFeedItems
handleMess (FeedItems fi) = do
    s <- BT.get
    let newCount = BL.listModify (feedListUnread .~ (fromIntegral . length) fi) (s ^. feeds)
    items .= BL.list ItemList (Vec.fromList fi) 1
    feeds .= newCount
handleMess (Status name) = userName .= name
handleMess (FeedAdded url) = status .= ("Added " <> url)
handleMess (BackendError text) = status .= ("Error: " <> text)
handleMess (NewItems feed) = do
    s <- BT.get
    let
        (_, needUpdate) =
            case Vec.elemIndex feed (s ^. feeds . BL.listElementsL) of
                Nothing -> (s & feeds %~ insertInOrder feed, False)
                Just i ->
                    ( s & feeds . BL.listElementsL . ix i . feedListUnread +~ (feed ^. feedListUnread)
                    , True)
        sameAsSelected = Just feed == (s ^. feeds . to BL.listSelectedElement ^? _Just . _2)
    when (needUpdate && sameAsSelected) getSelFeedItems
handleMess (EditableFeedInfo feed) = do
    s <- BT.get
    M.suspendAndResume $ editVty s feed
handleMess (FeedInfoUpdated (fid, name)) = BT.modify $ \s -> updateName s name fid
handleMess InvalidSent = return ()

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

getSelFeedItems :: BT.EventM Name AppState ()
getSelFeedItems = do
    s <- BT.get
    let conn = s ^. wsConn
        sel = BL.listSelectedElement (s ^. feeds)
    case sel of
        Nothing -> return ()
        Just (_, e) -> fork_ $ WS.sendBinaryData conn (encode (GetFeedItems (e ^. feedListId)))
    return ()

getInfo :: BT.EventM Name AppState ()
getInfo = do
    s <- BT.get
    fork_ $ WS.sendBinaryData (s ^. wsConn) (encode Initialized)
    status .= "Connected to server"

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
myAttrs = [(selectedAttr, BU.fg V.white), (readAttr, BU.fg V.red)]

euTimeLocale :: Time.TimeLocale
euTimeLocale = Time.defaultTimeLocale {Time.dateFmt = "%d/%m/%y"}

feedListWidth :: Int
feedListWidth = 50
