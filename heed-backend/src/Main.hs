{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Trans.Except
import qualified Data.Ini as Ini
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Heed.Extract (addFeed)
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
    result <- runExceptT $ addFeed baConf "https://news.ycombinator.com/rss"
    print result

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
