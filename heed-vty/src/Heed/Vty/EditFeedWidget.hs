{-# LANGUAGE OverloadedStrings #-}

--{-# LANGUAGE RankNTypes #-}
module Heed.Vty.EditFeedWidget
    ( editVty
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, txt, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Edit as E
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Serialize (encode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Heed.Commands
       (FeEditFeed, FeEditFeed'(..), Up(UpdatedFeedInfo))
import Heed.Utils (fork_)
import Heed.Vty.WidgetStates as S
import qualified Network.WebSockets as WS
import Safe (headDef)

drawUI :: S.EditState -> [T.Widget S.EditName]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. S.editFocusRing) E.renderEditor (st ^. S.nameEdit)
    e2 = F.withFocusRing (st ^. S.editFocusRing) E.renderEditor (st ^. S.editUpdateEdit)
    ui =
        C.center $
        (txt "Feed Name " <+> errorMsg (st ^. S.nameMessage)) <=> hLimit 100 e1 <=>
        (txt "Update every (minutes) " <+> errorMsg (st ^. S.editUpdateMessage)) <=>
        hLimit 100 e2 <=>
        txt " " <=>
        txt "Tab to switch line" <=>
        txt "Esc to exit" <=>
        txt "Enter to confirm information"

errorMsg :: Text -> T.Widget S.EditName
errorMsg = withAttr "error" . txt

--
appEvent :: S.EditState -> T.BrickEvent S.EditName e -> T.EventM S.EditName (T.Next S.EditState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & editFocusRing %~ F.focusNext
        V.EvKey V.KEnter [] -> do
            let name = headDef "" . E.getEditContents $ st ^. S.nameEdit
                update = headDef "" . E.getEditContents $ st ^. S.editUpdateEdit
                validate = valid (isValidName name) (S.isValidUpdateInterval update)
            case validate of
                S.Valid -> do
                    sendEditData
                        (st ^. editWsConn)
                        (st ^. editFeedId)
                        name
                        (read . Text.unpack $ update)
                    M.halt $ st & S.editAdding .~ True
                S.InvalidLeft e -> M.continue $ st & nameMessage .~ e
                S.InvalidRight e -> M.continue $ st & editUpdateMessage .~ e
                S.BothInvalid u up -> M.continue $ st & nameMessage .~ u & editUpdateMessage .~ up
        V.EvKey V.KBackTab [] -> M.continue $ st & S.editFocusRing %~ F.focusPrev
        _ ->
            M.continue =<<
            case F.focusGetCurrent (st ^. S.editFocusRing) of
                Just S.NameEdit -> T.handleEventLensed st S.nameEdit E.handleEditorEvent ev
                Just S.UpdateEveryEdit ->
                    T.handleEventLensed st S.editUpdateEdit E.handleEditorEvent ev
                Nothing -> return st
appEvent st _ = M.continue st

isValidName :: Text -> Maybe Text
isValidName text
    | Text.null text = Just "Name cannot be empty"
    | otherwise = Nothing

valid :: Maybe Text -> Maybe Text -> DataValid
valid Nothing Nothing = S.Valid
valid (Just urlError) Nothing = S.InvalidLeft urlError
valid Nothing (Just updateError) = S.InvalidRight updateError
valid (Just url) (Just up) = S.BothInvalid url up

sendEditData
    :: (MonadIO m)
    => WS.Connection -> Int -> Text -> Int -> m ()
sendEditData conn feedId name every =
    fork_ $ WS.sendBinaryData conn (encode (UpdatedFeedInfo $ FeEditFeed' feedId name every))

initialState :: FeEditFeed -> WS.Connection -> S.EditState
initialState edit =
    S.EditState
        (_feEditId edit)
        (F.focusRing [S.NameEdit, S.UpdateEveryEdit])
        (E.editorText S.NameEdit (txt . Text.unlines) (Just 1) (_feEditName edit))
        (E.editorText
             S.UpdateEveryEdit
             (txt . Text.unlines)
             (Just 1)
             (Text.pack . show . _feEditUpdateEvery $ edit))
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

appCursor :: S.EditState -> [T.CursorLocation S.EditName] -> Maybe (T.CursorLocation S.EditName)
appCursor = F.focusRingCursor (^. editFocusRing)

theApp :: M.App S.EditState e S.EditName
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = appCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }

--
editVty :: S.AppState -> FeEditFeed -> IO S.AppState
editVty state feed = do
    st <- M.defaultMain theApp $ initialState feed (state ^. wsConn)
    let finalStatus =
            if st ^. S.editAdding
                then "Updating " <> (headDef "" . E.getEditContents) (st ^. nameEdit)
                else "Not updating feed info "
    return $ state & status .~ finalStatus
