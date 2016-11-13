module Heed.GlobalWebsocket where

import System.IO.Unsafe
import Data.Aeson (encode)
import Data.Text.Lazy (toStrict)
import Control.Concurrent.MVar
import Heed.Commands
import Data.Text.Lazy.Encoding (decodeUtf8)
import JSDOM.Generated.WebSocket
import JSDOM.Types

heedWebsocket :: MVar WebSocket
heedWebsocket = unsafePerformIO newEmptyMVar
{-# NOINLINE heedWebsocket #-}

sendCommand :: WebSocket -> Up -> IO ()
sendCommand ws command =
    runJ $ sendString ws (toStrict . decodeUtf8 . encode $ command)

runJ :: JSM a -> IO a
runJ = flip runJSM undefined
