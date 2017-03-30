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
import Database.PostgreSQL.Tmp (DBInfo(..), withTmpDB)
import Heed.Database (afterDefTime, defFeedItem, FeedItemHW, _feedItemDate, _feedItemTitle, _feedItemUrl)
import Heed.Extract (filterNew)
import qualified Heed.QueryTest as QT
import Heed.Types (execQuery, runTest)
import Heed.Utils (silentProc)
import qualified Opaleye.Trans as OT
import System.Process (createProcess, waitForProcess)
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

testingDB :: ByteString
testingDB = "dbname='heed' user='heed'"

tables :: [(OT.Transaction [Int64], String)]
tables =
    [ (QT.checkAuthTokenTable, "Auth token Table")
    , (QT.checkFeedInfoTable, "Feed Info Table")
    , (QT.checkFeedItemTable, "Feed Items Table")
    , (QT.checkPrefTable, "User Pref Tables")
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
        (_, _, _, p) <-
            createProcess $
            silentProc
                "/usr/bin/psql"
                [ "-d"
                , T.unpack tempName
                , "-U"
                , T.unpack tempRole
                , "-f"
                , "/home/arguggi/projects/heed/confs/db/tables.sql"
                ]
        _ <- waitForProcess p
        connect tempDB $ runSQLTests "SQL file should match haskell"
    runTests

runSQLTests :: String -> PG.Connection -> IO ()
runSQLTests desc conn =
    hspec $
    describe desc $
    forM_ tables $ \(trans, message) ->
        it message $ runTest conn (execQuery trans) >>= (`shouldSatisfy` (\x -> head x >= 0))

runTests :: IO ()
runTests = hspec $
    describe "Filter new items" $ do
        it "Removes duplicates" $
            length (filterNew (replicate 10 defFeedItem) []) `shouldBe` 1
        it "Removes same items" $
            length (filterNew differentList [differentTitle]) `shouldBe` 2
        it "Removes the same duplicate items" $
            length (filterRep defFeedItem defFeedItem) `shouldBe` 0
        it "Removes items with only different dates" $
            length (filterRep defFeedItem differentTime) `shouldBe` 0
        it "Keeps items with different titles" $
            length (filterRep defFeedItem differentTitle) `shouldBe` 1
        it "Keeps items with different urls" $
            length (filterRep defFeedItem differentUrl) `shouldBe` 1

filterRep :: FeedItemHW -> FeedItemHW -> [FeedItemHW]
filterRep new db = filterNew (replicate 10 new) (replicate 10 db)

differentTime :: FeedItemHW
differentTime = defFeedItem { _feedItemDate = afterDefTime }

differentTitle :: FeedItemHW
differentTitle = defFeedItem { _feedItemTitle = "Testing title" }

differentUrl :: FeedItemHW
differentUrl = defFeedItem { _feedItemUrl = "Testing url" }

differentList :: [FeedItemHW]
differentList = [differentTime, differentUrl, differentTitle]
