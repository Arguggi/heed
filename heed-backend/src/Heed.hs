{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed
  ( main,
  )
where

import qualified BroadcastChan as BChan
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Database.PostgreSQL.Simple as PG (connectPostgreSQL)
import Development.GitRev (gitHash)
import Heed.Database (feedInfoId)
import Heed.Extract (startUpdateThread)
import Heed.Query (allFeeds)
import Heed.Server (genAuthMain)
import Heed.Types (BackendConf (..), execSelect, runBe, threadMap)
import Heed.Utils (Port, defPort)
import Lens.Micro.Platform ((&), (.~), (^..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    help,
    info,
    infoOption,
    long,
  )
import Paths_heed_backend (version)
import System.Exit (die)
import qualified System.Log.FastLogger as Log
import qualified System.Log.FastLogger.Date as LogDate

-- | List of Environment variables to setup for PostgreSQL
-- pgEnvVar :: [String]
-- pgEnvVar = ["PGUSER", "PGDATABASE"]

-- | Connect to PostgreSQL and start rest backend
main :: IO ()
main = do
  _ <- execParser optsParser
  timeCache <- LogDate.newTimeCache LogDate.simpleTimeFormat
  Log.withTimedFastLogger timeCache (Log.LogStdout Log.defaultBufSize) $ \logger -> do
    putStrLn "Starting heed-backend"
    port <- setupEnvGetPort
    baConf <- setupBackendConf logger
    feedsE <- runBe baConf $ execSelect allFeeds
    now <- getCurrentTime
    threads <-
      case feedsE of
        Left _ -> die "Can't get feed list from db"
        Right feeds -> do
          threadIds <- forM feeds (startUpdateThread now baConf)
          let nameThreadList = zip (feeds ^.. traverse . feedInfoId) threadIds
          return $ Map.fromList nameThreadList
    tvarThreads <- newTVarIO threads
    genAuthMain (baConf & (threadMap .~ tvarThreads)) port

-- | Read ini file and setup 'pgEnvVar' variables
setupEnvGetPort :: IO Port
setupEnvGetPort = return defPort

-- iniFile <- Ini.readIniFile "backend.ini"
-- case iniFile of
--     Left e -> do
--         putStrLn $ "Ignoring ini file: " ++ e
--         return defPort
--     Right ini -> do
--         forM_ pgEnvVar $ \var ->
--             let value = T.unpack . fromRight "" $ Ini.lookupValue "postgresql" (T.pack var) ini in
--             setEnv var value
--         return $ getPort ini

-- | Get port fron ini or 'defPort'
-- getPort :: Ini.Ini -> Port
-- getPort ini =
--   fromRight defPort $ do
--     port <- Ini.lookupValue "websocket" "port" ini
--     readEither . T.unpack $ port

-- | Create 'BackendConf' for the server
setupBackendConf :: Log.TimedFastLogger -> IO BackendConf
setupBackendConf logger =
  BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings
    <*> BChan.newBroadcastChan
    <*> pure logger
    <*> newTVarIO Map.empty

-- | Only parse --version and --help
optsParser :: ParserInfo String
optsParser = info (versionOption <*> pure "") mempty

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (concat [showVersion version, " ", $(gitHash)])
    (long "version" <> help "Show version")
