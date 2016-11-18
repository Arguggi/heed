module Heed.GlobalWebsocket where

import Control.Concurrent.MVar
import Data.Aeson (encode)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Heed.Commands
import JSDOM.Generated.WebSocket
import JSDOM.Types
import System.IO.Unsafe

heedWebsocket :: MVar WebSocket
heedWebsocket = unsafePerformIO newEmptyMVar

{-# NOINLINE heedWebsocket #-}

sendCommand :: Up -> IO ()
sendCommand command = do
    wsM <- tryReadMVar heedWebsocket
    case wsM of
        Nothing -> return ()
        Just ws -> runJ $ sendString ws (toStrict . decodeUtf8 . encode $ command)

runJ :: JSM a -> IO a
runJ = flip runJSM undefined
