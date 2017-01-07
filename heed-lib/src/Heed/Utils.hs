module Heed.Utils
  ( forkIO_
  , progName
  , Port
  , defPort
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)

-- | 'forkIO' that ignores 'ThreadId'
forkIO_
    :: IO () -- ^ Action
    -> IO ()
forkIO_ = void . forkIO

progName :: String
progName = "heed"

type Port = Int

defPort :: Port
defPort = 443
