module Heed.GHCJS where

import JSDOM.Types

runJ :: JSM a -> IO a
runJ = flip runJSM undefined
