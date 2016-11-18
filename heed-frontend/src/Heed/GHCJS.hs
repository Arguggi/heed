module Heed.GHCJS where

import qualified Data.Text as T
import JSDOM
import JSDOM.Generated.Window
import JSDOM.Types

runJ :: JSM a -> IO a
runJ = flip runJSM undefined

openInNewTab :: T.Text -> IO ()
openInNewTab link =
    runJ $
    do windowM <- currentWindow
       case windowM of
           Nothing -> Prelude.error "Unable to get window"
           Just window -> open_ window link "_blank" ""
