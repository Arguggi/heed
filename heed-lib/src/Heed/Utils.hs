module Heed.Utils where

import Control.Concurrent (forkIO)
import Control.Monad (void)

forkIO_ :: IO () -> IO ()
forkIO_ = void . forkIO
