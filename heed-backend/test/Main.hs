{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Database.PostgreSQL.Simple as PSQL
import qualified Heed.QueryTest as QT
import Heed.Types (execQuery, runTest)
import qualified Opaleye.Trans as OT
import Test.Hspec (describe, hspec, it, shouldSatisfy)

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

main :: IO ()
main =
    bracket (PSQL.connectPostgreSQL testingDB) PSQL.close $ \conn ->
        hspec $
        describe "SQL tables should match haskell" $
        forM_ tables $ \(trans, message) ->
            it message $ runTest conn (execQuery trans) >>= (`shouldSatisfy` (\x -> head x >= 0))
