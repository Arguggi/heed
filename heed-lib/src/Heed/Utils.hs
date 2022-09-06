module Heed.Utils
  ( fork,
    fork_,
    progName,
    Port,
    defPort,
    silentProc,
  )
where

import Control.Concurrent (ThreadId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified SlaveThread as ST
import qualified System.Process as Process

-- | Lift 'ST.fork' to 'MonadIO'
fork ::
  (MonadIO m) =>
  IO a ->
  m ThreadId
fork = liftIO . ST.fork

-- | 'ST.fork' that ignores 'ThreadId'
fork_ ::
  (MonadIO m) =>
  -- | Action
  IO a ->
  m ()
fork_ = void . fork

-- | Program name
progName :: String
progName = "heed"

type Port = Int

-- | Default port for server (assumes TLS so 443)
defPort :: Port
defPort = 8090

-- | 'Process.CreateProcess' with all handles (in,out,err) closed
--   we need this since vty doens't handle stdout well
silentProc :: FilePath -> [String] -> Process.CreateProcess
silentProc fp command =
  (Process.proc fp command)
    { Process.std_in = Process.NoStream,
      Process.std_out = Process.NoStream,
      Process.std_err = Process.NoStream
    }
