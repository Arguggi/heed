module Heed.Utils
    ( fork
    , fork_
    , progName
    , Port
    , defPort
    ) where

import Control.Concurrent (ThreadId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified SlaveThread as ST

fork
    :: (MonadIO m)
    => IO a -> m ThreadId
fork = liftIO . ST.fork

-- | 'ST.fork' that ignores 'ThreadId'
fork_
    :: (MonadIO m)
    => IO a -- ^ Action
    -> m ()
fork_ = void . fork

progName :: String
progName = "heed"

type Port = Int

defPort :: Port
defPort = 443
