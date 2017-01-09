{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever, void)
import qualified Data.Ini as Ini
import qualified Data.Text as T

import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple as PG

import Heed.Database (UserId(..))
import Heed.Extract (importOPML, startUpdateThread)
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
    --Opmlfile <- TIO.readFile "/etc/heed/ttrss.opml"
    --_ <- runBe baConf $ importOPML opmlfile (UserId 1) -- Hardcoded as my user
    feedsE <- runBe baConf $ execQuery allFeeds
    case feedsE of
        Left _ -> die "Can't get feed list from db"
        Right feeds ->
            void $ forM_ feeds (forkIO . forever . void . runBe baConf . startUpdateThread)
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
setupBackendConf = BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings
