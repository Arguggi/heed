{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Heed.Vty.AddFeedWidget
    ( addVty
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, txt, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Edit as E
import Lens.Micro.Platform
import Control.Monad.IO.Class (MonadIO)
import Data.Serialize (encode)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Heed.Commands (Up(NewFeed))
import Heed.Utils (fork_)
import Heed.Vty.WidgetStates as S
import qualified Network.WebSockets as WS
import Safe (headDef)
import Text.URI (parseURI, uriScheme)

drawUI :: S.AddState -> [T.Widget S.AddName]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. S.addFocusRing) (E.renderEditor (txt . Text.unlines)) (st ^. S.urlEdit)
    e2 = F.withFocusRing (st ^. S.addFocusRing) (E.renderEditor (txt . Text.unlines)) (st ^. S.addUpdateEdit)
    ui =
        C.center $
        (txt "Feed Url " <+> errorMsg (st ^. urlMessage)) <=> hLimit 100 e1 <=>
        (txt "Update every (minutes) " <+> errorMsg (st ^. addUpdateMessage)) <=>
        hLimit 100 e2 <=>
        txt " " <=>
        txt "Tab to switch line" <=>
        txt "Esc to exit" <=>
        txt "Enter to confirm information"

errorMsg :: Text -> T.Widget S.AddName
errorMsg = withAttr (A.attrName "error") . txt

appEvent :: T.BrickEvent S.AddName () -> T.EventM S.AddName S.AddState ()
appEvent event@(T.VtyEvent ev) = do
    st <- T.get
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey (V.KChar '\t') [] -> S.addFocusRing %= F.focusNext
        V.EvKey V.KEnter [] -> do
            let url = headDef "" . E.getEditContents $ st ^. urlEdit
                update = headDef "" . E.getEditContents $ st ^. S.addUpdateEdit
                validate = valid (isValidUrl url) (S.isValidUpdateInterval update)
            case validate of
                S.Valid -> do
                    sendNewFeedData (st ^. S.addWsConn) url (read . unpack $ update)
                    S.addAdding .= True
                    M.halt
                S.InvalidLeft e -> urlMessage .= e
                S.InvalidRight e -> S.addUpdateMessage .= e
                S.BothInvalid u up -> do
                    urlMessage .= u 
                    S.addUpdateMessage .= up
        V.EvKey V.KBackTab [] -> S.addFocusRing %= F.focusPrev
        _ ->
            case F.focusGetCurrent (st ^. S.addFocusRing) of
                Just S.UrlEdit -> zoom S.urlEdit $ E.handleEditorEvent event
                Just S.UpdateEdit -> zoom S.addUpdateEdit $ E.handleEditorEvent event
                Nothing -> return ()
appEvent _ = return ()

sendNewFeedData
    :: (MonadIO m)
    => WS.Connection -> Text -> Int -> m ()
sendNewFeedData conn url every = fork_ $ WS.sendBinaryData conn (encode (NewFeed url every))

isValidUrl :: Text -> Maybe Text
isValidUrl url =
    case parseURI (unpack url) of
        Nothing -> Just "Invalid url"
        Just uri ->
            if validUriScheme (uriScheme uri)
                then Nothing
                else Just "Invalid scheme"

validUriScheme :: Maybe String -> Bool
validUriScheme Nothing = False
validUriScheme (Just scheme)
    | scheme == "http" = True
    | scheme == "https" = True
    | otherwise = False

valid :: Maybe Text -> Maybe Text -> DataValid
valid Nothing Nothing = S.Valid
valid (Just urlError) Nothing = S.InvalidLeft urlError
valid Nothing (Just updateError) = S.InvalidRight updateError
valid (Just url) (Just up) = S.BothInvalid url up

initialState :: WS.Connection -> S.AddState
initialState =
    S.AddState
        (F.focusRing [S.UrlEdit, S.UpdateEdit])
        (E.editorText S.UrlEdit (Just 1) "")
        (E.editorText S.UpdateEdit (Just 1) "60")
        ""
        ""
        False

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (E.editAttr, V.white `on` V.blue)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        , (A.attrName "error", fg V.red)
        ]

appCursor :: S.AddState -> [T.CursorLocation S.AddName] -> Maybe (T.CursorLocation S.AddName)
appCursor = F.focusRingCursor (^. S.addFocusRing)

theApp :: M.App S.AddState () S.AddName
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = appCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

addVty :: S.AppState -> IO S.AppState
addVty state = do
    st <- M.defaultMain theApp $ initialState (state ^. S.wsConn)
    let finalStatus =
            if st ^. S.addAdding
                then "Adding " <> (headDef "" . E.getEditContents) (st ^. urlEdit)
                else "Not adding new feed"
    return $ state & status .~ finalStatus
