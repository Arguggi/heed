module Heed.Utils where

import Control.Concurrent (forkIO)
import Control.Monad (void)

-- | 'forkIO' that ignores 'ThreadId'
forkIO_
    :: IO () -- ^ Action
    -> IO ()
forkIO_ = void . forkIO
