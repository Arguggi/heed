{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Tmp (withTmpDB, DBInfo(..))
import qualified Heed.QueryTest as QT
import Heed.Types (execQuery, runTest)
import qualified Opaleye.Trans as OT
import Test.Hspec (describe, hspec, it, shouldSatisfy)
import System.Process (callProcess)

testingDB :: ByteString
testingDB = "dbname='heed' user='heed'"

tables :: [(OT.Transaction [Int64], String)]
tables =
    [ (QT.checkAuthTokenTable, "Auth token Table")
    , (QT.checkFeedInfoTable, "Feed Info Table")
    , (QT.checkFeedItemTable, "Feed Items Table")
    , (QT.checkSubscriptionTable, "Subscriptions Table")
    , (QT.checkUserTable, "User Tables")
    ]

connect :: ByteString -> (PG.Connection -> IO ()) -> IO ()
connect connString = bracket (PG.connectPostgreSQL connString) PG.close

main :: IO ()
main = do
    connect testingDB (runSQLTests "SQL tables should match haskell")
    withTmpDB $ \(DBInfo tempName tempRole) -> do
        let tempDB = encodeUtf8 $ "dbname='" <> tempName <> "' user='" <> tempRole <> "'"
        callProcess "/usr/bin/psql" ["-d", T.unpack tempName, "-U", T.unpack tempRole, "-f", "/home/arguggi/projects/heed/confs/db/tables.sql"]
        connect tempDB $ runSQLTests "SQL file should match haskell"

runSQLTests :: String -> PG.Connection -> IO ()
runSQLTests desc conn = hspec $
    describe desc $
        forM_ tables $ \(trans, message) ->
            it message $ runTest conn (execQuery trans) >>= (`shouldSatisfy` (\x -> head x >= 0))

