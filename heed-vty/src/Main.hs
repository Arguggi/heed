{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Brick.AttrMap as BA
import qualified Brick.BChan as BChan
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
       (hBox, hLimit, padLeft, str, txt, vBox, withAttr, (<+>), (<=>), vLimit)
import qualified Brick.Widgets.List as BL
import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad (forever, void, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decodeStrict', encode)
import qualified Data.ByteString as BS
import Data.Default
import Data.Function ((&))
import Data.Ini (lookupValue, readIniFile)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Format as Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Heed.Commands
import Heed.Utils (progName)
import Network (PortNumber)
import Network.HTTP.Simple
       (Request, defaultRequest, getResponseBody, httpJSONEither,
        setRequestBodyJSON, setRequestHost, setRequestMethod,
        setRequestPath, setRequestPort, setRequestSecure)
import qualified Network.WebSockets as WS
import System.Directory
       (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import System.Exit (die)
import qualified System.Process as Process
import Text.Read (readEither)
import Wuss (runSecureClientWith)

data Name
    = StatusBar
    | FeedList
    | ItemList
    deriving (Ord, Show, Eq)

data AppState = AppState
    { _feeds :: BL.List Name FeFeedInfo
    , _items :: BL.List Name FeItemInfo
    , _userName :: Text
    , _wsConn :: WS.Connection
    , _status :: Text
    }

makeLenses ''AppState

defState :: Text -> WS.Connection -> Text -> AppState
defState = AppState (BL.list FeedList Vec.empty 2) (BL.list ItemList Vec.empty 2)

data MyEvent =
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
itemDrawDetail (Just (_,info)) =
    txt (info ^. itemInfoTitle)
    <=> txt (info ^. itemInfoLink)
    <=> txt (fromMaybe "no comments" $ info ^. itemInfoComments)

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
    void . forkIO . void $ Process.createProcess
        browserProc
        { Process.std_in = Process.NoStream
        , Process.std_out = Process.NoStream
        , Process.std_err = Process.NoStream
        }
    case e ^. itemInfoComments of
        Nothing -> return ()
        Just linkComment -> void . forkIO . void $ Process.createProcess
            (commentProc linkComment)
            { Process.std_in = Process.NoStream
            , Process.std_out = Process.NoStream
            , Process.std_err = Process.NoStream
            }
  where browserProc = Process.proc "chromium" [T.unpack $ e ^. itemInfoLink]
        commentProc x = Process.proc "chromium" [T.unpack x]

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

main :: IO ()
main = do
    configFolder <- getXdgDirectory XdgConfig progName
    createDirectoryIfMissing True configFolder
    creds <- authRequest configFolder
    case creds of
        Left e -> putStrLn $ "Invalid config file: " <> e
        Right (req, host, port) -> do
            putStrLn "Authenticating"
            authCheck <- httpJSONEither req
            case getResponseBody authCheck of
                Left e -> print e
                Right (Token t) -> do
                    putStrLn "Starting heed"
                    runSecureClientWith
                        (T.unpack host)
                        ((read . show $ port) :: PortNumber)
                        "/"
                        WS.defaultConnectionOptions
                        [("auth-token", encodeUtf8 t)]
                        startApp
                    putStrLn "Closing"

startApp :: WS.Connection -> IO ()
startApp wsconn = do
    eventChan <- BChan.newBChan 200
    _ <-
        forkIO . forever $
        do wsdata <- WS.receiveData wsconn :: IO BS.ByteString
           case decodeStrict' wsdata of
               Nothing -> return ()
               Just mess -> BChan.writeBChan eventChan (WsReceive mess)
    _ <-
        M.customMain
            (V.mkVty Data.Default.def)
            (Just eventChan)
            app
            (defState "" wsconn "Connecting")
    return ()

euTimeLocale :: Time.TimeLocale
euTimeLocale =
    Time.defaultTimeLocale
    { Time.dateFmt = "%d/%m/%y"
    }

type Host = T.Text

type Port = Int

authRequest
    :: (MonadIO m)
    => FilePath -> m (Either String (Request, Host, Port))
authRequest configFolder = do
    iniFile <- liftIO . readIniFile $ configFolder <> "/" <> progName <> ".ini"
    case iniFile of
        Left e -> liftIO . die $ "Invalid config file: " <> e
        Right ini ->
            return $
            do host <- lookupValue "server" "host" ini
               portT <- lookupValue "server" "port" ini
               -- Assume tls by default
               secure <- return . either (const True) id $ isSecure <$> lookupValue "server" "tls" ini
               port <- return . either (const (443 :: Int)) id $ readEither (T.unpack portT)
               user <- lookupValue "auth" "username" ini
               pass <- lookupValue "auth" "password" ini
               return
                   ( defaultRequest & setRequestBodyJSON (AuthData user pass) &
                     setRequestHost (encodeUtf8 host) &
                     setRequestPort port &
                     setRequestPath "auth" &
                     setRequestMethod "POST" &
                     setRequestSecure secure
                   , host
                   , port)
  where
    isSecure x = T.toLower x == "on"
