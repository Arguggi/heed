{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Heed.Vty.AddFeedWidget
  ( addVty
  ) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, (<=>))
import qualified Brick.Widgets.Edit as E
import Control.Lens
import qualified Graphics.Vty as V
import Heed.Vty.WidgetStates as S
import qualified Network.WebSockets as WS

drawUI :: S.AddState -> [T.Widget S.AddName]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) E.renderEditor (st ^. S.urlEdit)
    e2 = F.withFocusRing (st ^. focusRing) E.renderEditor (st ^. S.updateEdit)
    ui =
        C.center $
        str "Feed Url" <=> hLimit 100 e1 <=> str "Update every (minutes): " <=> hLimit 100 e2 <=>
        str " " <=>
        str "Tab to switch between lines" <=>
        str "Esc to confirm"

appEvent :: S.AddState -> T.BrickEvent S.AddName e -> T.EventM S.AddName (T.Next S.AddState)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        _ ->
            M.continue =<<
            case F.focusGetCurrent (st ^. focusRing) of
                Just S.UrlEdit -> T.handleEventLensed st S.urlEdit E.handleEditorEvent ev
                Just S.UpdateEdit -> T.handleEventLensed st S.updateEdit E.handleEditorEvent ev
                Nothing -> return st
appEvent st _ = M.continue st

initialState :: WS.Connection -> S.AddState
initialState =
    S.AddState
        (F.focusRing [S.UrlEdit, S.UpdateEdit])
        (E.editor S.UrlEdit (str . unlines) (Just 1) "")
        (E.editor S.UpdateEdit (str . unlines) (Just 1) "60")

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [(E.editAttr, V.white `on` V.blue), (E.editFocusedAttr, V.black `on` V.yellow)]

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
    _ <- M.defaultMain theApp $ initialState (state ^. S.wsConn)
    return state
