module Heed.GlobalWebsocket where

import Control.Concurrent.MVar
import Data.Aeson (encode)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Heed.Commands
import Heed.GHCJS
import JSDOM.Generated.WebSocket
import System.IO.Unsafe

-- | Global 'MVar' that will be filled with a 'WebSocket' once the connection is
--   opened
heedWebsocket :: MVar WebSocket
heedWebsocket = unsafePerformIO newEmptyMVar

{-# NOINLINE heedWebsocket #-}

-- | Send a command to the server if 'heedWebsocket' contains a 'WebSocket'
sendCommand :: Up -> IO ()
sendCommand command = do
    wsM <- tryReadMVar heedWebsocket
    case wsM of
        Nothing -> return ()
        Just ws -> runJ $ sendString ws (toStrict . decodeUtf8 . encode $ command)
