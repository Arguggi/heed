module Heed.Utils
    ( fork
    , fork_
    , progName
    , Port
    , defPort
    , silentProc
    ) where

import Control.Concurrent (ThreadId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified SlaveThread as ST
import qualified System.Process as Process

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

silentProc :: FilePath -> [String] -> Process.CreateProcess
silentProc fp command =
    (Process.proc fp command)
    { Process.std_in = Process.NoStream
    , Process.std_out = Process.NoStream
    , Process.std_err = Process.NoStream
    }
