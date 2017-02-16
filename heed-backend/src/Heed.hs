{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed where

import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Monad (forM_)
import qualified Data.Ini as Ini
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Database.PostgreSQL.Simple as PG
import Development.GitRev (gitHash)
import Heed.Extract (startUpdateThread)
import Heed.Query (allFeeds)
import Heed.Server (genAuthMain)
import Heed.Types
import Heed.Utils (Port, defPort)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
       (Parser, ParserInfo, execParser, help, info, infoOption, long)
import Paths_heed_backend (version)
import System.Environment (setEnv)
import System.Exit (die)
import qualified System.Log.FastLogger as Log
import qualified System.Log.FastLogger.Date as LogDate
import qualified System.Remote.Monitoring as EKG
import Text.Read (readEither)

-- | List of Environment variables to setup for PostgreSQL
pgEnvVar :: [String]
pgEnvVar = ["PGUSER", "PGDATABASE"]

-- | Connect to PostgreSQL and start rest backend
main :: IO ()
main = do
    _ <- execParser optsParser
    _ <- EKG.forkServer "localhost" 9579
    timeCache <- LogDate.newTimeCache LogDate.simpleTimeFormat
    Log.withTimedFastLogger timeCache (Log.LogStdout Log.defaultBufSize) $ \logger -> do
        putStrLn "Starting heed-backend"
        port <- setupEnvGetPort
        baConf <- setupBackendConf logger
        feedsE <- runBe baConf $ execQuery allFeeds
        now <- getCurrentTime
        case feedsE of
            Left _ -> die "Can't get feed list from db"
            Right feeds -> forM_ feeds (startUpdateThread now baConf)
        genAuthMain baConf port

-- | Read ini file and setup 'pgEnvVar' variables
setupEnvGetPort :: IO Port
setupEnvGetPort = do
    iniFile <- Ini.readIniFile "/etc/heed/backend.ini"
    case iniFile of
        Left e -> die $ "Invalid ini file: " ++ e
        Right ini -> do
            forM_ pgEnvVar $ \var ->
                setEnv var . T.unpack $
                either (const "") id (Ini.lookupValue "postgresql" (T.pack var) ini)
            return $ getPort ini

getPort :: Ini.Ini -> Port
getPort ini =
    either (const defPort) id $ do
        port <- Ini.lookupValue "websocket" "port" ini
        readEither . T.unpack $ port

-- | Create 'BackendConf' for the server
setupBackendConf :: Log.TimedFastLogger -> IO BackendConf
setupBackendConf logger =
    BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings <*>
    BChan.newBroadcastChan <*>
    pure logger

optsParser :: ParserInfo String
optsParser = info (versionOption <*> pure "") mempty

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        (concat [showVersion version, " ", $(gitHash)])
        (long "version" <> help "Show version")
