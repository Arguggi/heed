module Heed.GHCJS where

import qualified Data.Text as T
import JSDOM
import JSDOM.Generated.Window
import JSDOM.Types

-- | Open link in new tab with 'open_', unfortunatly I still haven't figured out
--   how to open in a new tab AND in the background via js
openInNewTab :: T.Text -> IO ()
openInNewTab link =
    runJ $
    do windowM <- currentWindow
       case windowM of
           Nothing -> Prelude.error "Unable to get window"
           Just window -> open_ window link "_blank" ""

-- | Convenience. This only works with ghcjs since 'runJSM' is called with an undefined.
--   When using ghcjs this isn't a problem, but ghc needs the bridge to call js code,
--   since I don't need it I didn't bother looking if it's possibile and eventually
--   how to do it.
runJ :: JSM a -> IO a
runJ = flip runJSM undefined
