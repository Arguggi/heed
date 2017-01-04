{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Ini as Ini
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple as PG
import Heed.Database
import Heed.Extract (importOPML)
import Heed.Server (genAuthMain)
import Heed.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (setEnv)
import System.Exit (die)

-- | List of Environment variables to setup for PostgreSQL
pgEnvVar :: [String]
pgEnvVar = ["PGUSER", "PGDATABASE"]

-- | Connect to PostgreSQL and start rest backend
main :: IO ()
main = do
    putStrLn "Starting heed-backend"
    setupPostgresEnv
    baConf <- setupBackendConf
    --opmlfile <- TIO.readFile "ttrss.opml"
    --_ <- runBe baConf $ importOPML opmlfile (UserId 1) -- Hardcoded as my user
    genAuthMain baConf

-- | Read ini file and setup 'pgEnvVar' variables
setupPostgresEnv :: IO ()
setupPostgresEnv = do
    iniFile <- Ini.readIniFile "./heed-backend/config/devel.ini"
    case iniFile of
        Left e -> die $ "Invalid ini file: " ++ e
        Right ini ->
            forM_ pgEnvVar $
            \var ->
                 setEnv var . T.unpack $
                 either (const "") id (Ini.lookupValue "PostgreSQL" (T.pack var) ini)

-- | Create 'BackendConf' for the server
setupBackendConf :: IO BackendConf
setupBackendConf = BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings
