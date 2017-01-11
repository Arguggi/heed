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
import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Store (encode)
import Data.Text as T
import qualified Graphics.Vty as V
import Heed.Commands (Up(NewFeed))
import Heed.Vty.WidgetStates as S
import qualified Network.WebSockets as WS
import Safe (headDef)
import Text.Read (readEither)
import Text.URI (parseURI, uriScheme)

data DataValid
    = Valid
    | InvalidUrl T.Text
    | InvalidUpdate T.Text
    | BothInvalid T.Text
                  T.Text

drawUI :: S.AddState -> [T.Widget S.AddName]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) E.renderEditor (st ^. S.urlEdit)
    e2 = F.withFocusRing (st ^. focusRing) E.renderEditor (st ^. S.updateEdit)
    ui =
        C.center $
        (txt "Feed Url " <+> errorMsg (st ^. urlMessage)) <=> hLimit 100 e1 <=>
        (txt "Update every (minutes) " <+> errorMsg (st ^. updateMessage)) <=>
        hLimit 100 e2 <=>
        txt " " <=>
        txt "Tab to switch line" <=>
        txt "Esc to exit" <=>
        txt "Enter to confirm information"

errorMsg :: T.Text -> T.Widget S.AddName
errorMsg = withAttr "error" . txt

appEvent :: S.AddState -> T.BrickEvent S.AddName e -> T.EventM S.AddName (T.Next S.AddState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KEnter [] -> do
            let url = headDef "" . E.getEditContents $ st ^. urlEdit
                update = headDef "" . E.getEditContents $ st ^. updateEdit
                validate = valid (isValidUrl url) (isValidUpdate update)
            case validate of
                Valid -> do
                    sendNewFeedData (st ^. wsConnA) url (read . T.unpack $ update)
                    M.halt $ st & adding .~ True
                InvalidUrl e -> M.continue $ st & urlMessage .~ e
                InvalidUpdate e -> M.continue $ st & updateMessage .~ e
                BothInvalid u up -> M.continue $ st & urlMessage .~ u & updateMessage .~ up
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        _ ->
            M.continue =<<
            case F.focusGetCurrent (st ^. focusRing) of
                Just S.UrlEdit -> T.handleEventLensed st S.urlEdit E.handleEditorEvent ev
                Just S.UpdateEdit -> T.handleEventLensed st S.updateEdit E.handleEditorEvent ev
                Nothing -> return st
appEvent st _ = M.continue st

-- TODO
sendNewFeedData
    :: (MonadIO m)
    => WS.Connection -> T.Text -> Int -> m ()
sendNewFeedData conn url every =
    void . liftIO . forkIO $ WS.sendBinaryData conn (encode (NewFeed url every))

isValidUrl :: T.Text -> Maybe T.Text
isValidUrl url =
    case parseURI (T.unpack url) of
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

isValidUpdate :: T.Text -> Maybe T.Text
isValidUpdate update =
    case readEither (T.unpack update) of
        Left _ -> Just "Not a valid number"
        Right int ->
            if int < (0 :: Int)
                then Just "Must be positive"
                else Nothing

valid :: Maybe T.Text -> Maybe T.Text -> DataValid
valid Nothing Nothing = Valid
valid (Just urlError) Nothing = InvalidUrl urlError
valid Nothing (Just updateError) = InvalidUpdate updateError
valid (Just url) (Just up) = BothInvalid url up

initialState :: WS.Connection -> S.AddState
initialState =
    S.AddState
        (F.focusRing [S.UrlEdit, S.UpdateEdit])
        (E.editorText S.UrlEdit (txt . T.unlines) (Just 1) "")
        (E.editorText S.UpdateEdit (txt . T.unlines) (Just 1) "60")
        ""
        ""
        False

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (E.editAttr, V.white `on` V.blue)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        , ("error", fg V.red)
        ]

appCursor :: S.AddState -> [T.CursorLocation S.AddName] -> Maybe (T.CursorLocation S.AddName)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: M.App S.AddState e S.AddName
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = appCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }

addVty :: S.AppState -> IO S.AppState
addVty state = do
    st <- M.defaultMain theApp $ initialState (state ^. S.wsConn)
    let finalStatus =
            if st ^. adding
                then "Adding " <> (headDef "" . E.getEditContents) (st ^. urlEdit)
                else "Not adding new feed"
    return $ state & status .~ finalStatus
