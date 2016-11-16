{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Trans.Except
import qualified Data.Ini as Ini
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple as PG
import Heed.Database
import Heed.Extract (importOPML)
import Heed.Server
import Heed.Types (BackendConf(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (setEnv)
import System.Exit (die)

pgEnvVar :: [String]
pgEnvVar = ["PGUSER", "PGDATABASE"]

main :: IO ()
main = do
    setupPostgresEnv
    baConf <- setupBackendConf
    genAuthMain baConf

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

setupBackendConf :: IO BackendConf
setupBackendConf = BackendConf <$> PG.connectPostgreSQL "" <*> newManager tlsManagerSettings
