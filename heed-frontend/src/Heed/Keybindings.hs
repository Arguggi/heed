{-# LANGUAGE OverloadedStrings #-}

module Heed.Keybindings where

import Control.Lens.Operators ((^.))
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Heed.Dispatcher
import Heed.GHCJS
import JSDOM
import qualified JSDOM.EventM as E
import JSDOM.Generated.Window
import JSDOM.Types
import Language.Javascript.JSaddle (js)
import React.Flux hiding (KeyboardEvent)

initKeybindings :: IO ()
initKeybindings =
    runJ $
    do windowM <- currentWindow
       _ <-
           case windowM of
               Nothing -> Prelude.error "Unable to get window"
               Just window -> E.on window keyPress handleKeypress
       return ()

keybindings :: Map T.Text [SomeStoreAction]
keybindings =
    Map.fromList
        [ ("J", dispatchHeed nextFeed)
        , ("j", dispatchHeed nextItem)
        , ("K", dispatchHeed prevFeed)
        , ("k", dispatchHeed prevItem)
        ]

handleKeypress :: ReaderT KeyboardEvent DOM ()
handleKeypress =
    ReaderT $
    \keyboardEvent -> do
        key <- getKey keyboardEvent
        liftIO $ Prelude.print key
        case Map.lookup key keybindings of
            Nothing -> return ()
            Just action -> liftIO $ mapM_ executeAction action

getKey
    :: (MonadDOM m, FromJSString result)
    => KeyboardEvent -> m result
getKey self = liftDOM ((self ^. js keyS) >>= fromJSValUnchecked)

keyS :: String
keyS = "key"
