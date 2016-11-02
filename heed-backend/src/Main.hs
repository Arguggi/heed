{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (returnA)
import Control.Monad (forM_)
import qualified Data.Ini as Ini
import qualified Data.Text as T
import Database.PostgreSQL.Simple as PG
import Heed.Database (User(..), userTable)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Opaleye as O
import System.Environment (setEnv)
import System.Exit (die)

pgEnvVar :: [String]
pgEnvVar = ["PGUSER", "PGDATABASE"]

main :: IO ()
main = do
    setPostgresEnv
    dbConnection <- PG.connectPostgreSQL ""
    manager <- newManager tlsManagerSettings
    users <- runUsersQuery dbConnection getUsers
    print users

setPostgresEnv :: IO ()
setPostgresEnv = do
    iniFile <- Ini.readIniFile "./heed-backend/config/devel.ini"
    case iniFile of
        Left e -> die $ "Invalid ini file: " ++ e
        Right ini ->
            forM_ pgEnvVar $
            \var ->
                 setEnv var . T.unpack $
                 either (const "") id (Ini.lookupValue "PostgreSQL" (T.pack var) ini)

runUsersQuery :: PG.Connection -> O.Query (O.Column O.PGText) -> IO [T.Text]
runUsersQuery = O.runQuery

getUsers :: O.Query (O.Column O.PGText)
getUsers =
    proc () ->
  do (User _ name _ _) <- O.queryTable userTable -< ()
     returnA -< name
