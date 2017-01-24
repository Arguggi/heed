{-# LANGUAGE OverloadedStrings #-}

module Heed where

import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Monad (forM_)
import qualified Data.Ini as Ini
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Heed.Extract (startUpdateThread)
import Heed.Query (allFeeds)
import Heed.Server (genAuthMain)
import Heed.Types
import Heed.Utils (Port, defPort)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (setEnv)
import System.Exit (die)
import Text.Read (readEither)

-- | List of Environment variables to setup for PostgreSQL
pgEnvVar :: [String]
pgEnvVar = ["PGUSER", "PGDATABASE"]

-- | Connect to PostgreSQL and start rest backend
main :: IO ()
main = do
    putStrLn "Starting heed-backend"
    port <- setupEnvGetPort
    baConf <- setupBackendConf
    feedsE <- runBe baConf $ execQuery allFeeds
    case feedsE of
        Left _ -> die "Can't get feed list from db"
        Right feeds -> forM_ feeds (startUpdateThread baConf)
    genAuthMain baConf port

-- | Read ini file and setup 'pgEnvVar' variables
setupEnvGetPort :: IO Port
setupEnvGetPort = do
    iniFile <- Ini.readIniFile "/etc/heed/backend.ini"
    case iniFile of
        Left e -> die $ "Invalid ini file: " ++ e
        Right ini -> do
            forM_ pgEnvVar $
                \var ->
                     setEnv var . T.unpack $
                     either (const "") id (Ini.lookupValue "postgresql" (T.pack var) ini)
            return $ getPort ini

getPort :: Ini.Ini -> Port
getPort ini =
    either (const defPort) id $
    do port <- Ini.lookupValue "websocket" "port" ini
       readEither . T.unpack $ port

-- | Create 'BackendConf' for the server
setupBackendConf :: IO BackendConf
setupBackendConf =
    BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings <*> BChan.newBroadcastChan
