{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

--import Control.Exception (bracket)
import Control.Monad ((>=>))
--import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
-- import Data.Int (Int64)
-- import Data.Monoid ((<>))
-- import qualified Data.Text as T
-- import Data.Text.Encoding (encodeUtf8)
-- import qualified Database.PostgreSQL.Simple as PG
-- import Database.PostgreSQL.Tmp (DBInfo(..), withTmpDB)

-- import qualified Heed.QueryTest as QT
-- import Heed.Types (execQuery, runTest)
-- import Heed.Utils (silentProc)
-- import System.Process (createProcess, waitForProcess)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Heed.Database (FeedItemHW, afterDefTime, defFeedInfo, defFeedItem, _feedItemDate, _feedItemTitle, _feedItemUrl)
import Heed.Extract (filterNew)
import Heed.Feed.XML (extractInfo)
import qualified System.Directory
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Text.RawString.QQ
import Text.XML (Node, documentRoot, elementName, parseLBS_)
import qualified Text.XML
import Text.XML.Cursor (attribute, child, content, descendant, element, fromDocument, hasAttribute, laxElement, node, ($/), ($//), (&/), (&//), (&|))
import qualified Text.XML.Cursor.Generic
import qualified Text.XML.Stream.Parse

-- testingDB :: ByteString
-- testingDB = "dbname='heed' user='heed'"
--
-- tables :: [(OT.Transaction [Int64], String)]
-- tables =
--     [ (QT.checkAuthTokenTable, "Auth token Table")
--     , (QT.checkFeedInfoTable, "Feed Info Table")
--     , (QT.checkFeedItemTable, "Feed Items Table")
--     , (QT.checkPrefTable, "User Pref Tables")
--     , (QT.checkSubscriptionTable, "Subscriptions Table")
--     , (QT.checkUserTable, "User Tables")
--     ]

-- connect :: ByteString -> (PG.Connection -> IO ()) -> IO ()
-- connect connString = bracket (PG.connectPostgreSQL connString) PG.close

main :: IO ()
main = do
  --connect testingDB (runSQLTests "SQL tables should match haskell")
  --withTmpDB $ \(DBInfo tempName tempRole) -> do
  --    let tempDB = encodeUtf8 $ "dbname='" <> tempName <> "' user='" <> tempRole <> "'"
  --    (_, _, _, p) <-
  --        createProcess $
  --        silentProc
  --            "/usr/bin/psql"
  --            [ "-d"
  --            , T.unpack tempName
  --            , "-U"
  --            , T.unpack tempRole
  --            , "-f"
  --            , "/home/arguggi/projects/heed/confs/db/tables.sql"
  --            ]
  --    _ <- waitForProcess p
  --    connect tempDB $ runSQLTests "SQL file should match haskell"
  let root = Text.XML.documentRoot $ Text.XML.parseLBS_ Text.XML.Stream.Parse.def veritasiumxml
  now <- getCurrentTime
  -- let cursor = Text.XML.Cursor.fromDocument (Text.XML.Document (Text.XML.Prologue [] Nothing []) root [])
  runTests now root

-- runSQLTests :: String -> PG.Connection -> IO ()
-- runSQLTests desc conn =
--     hspec $
--     describe desc $
--     forM_ tables $ \(trans, message) ->
--         it message $ runTest conn (execQuery trans) >>= (`shouldSatisfy` (\x -> head x >= 0))

-- runTests :: Text.XML.Cursor.Generic.Cursor Text.XML.Node -> IO ()
runTests :: UTCTime -> Text.XML.Element -> IO ()
runTests now el = hspec $ do
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
  describe "Parses XML feeds" $ do
    it "Gets the current node" $ do
      -- show (Text.XML.Cursor.child >=> Text.XML.Cursor.element "link" $ cursor) `shouldBe` "1"
      --show (elementName $ child >=> hasAttribute "rel" $ cursor) `shouldBe` "1"
      let parsed = Heed.Feed.XML.extractInfo now "Url" el
      case parsed of
        Nothing -> return ()
        Just (info, items) -> do
          length items `shouldBe` 1
          info `shouldBe` defFeedInfo
          return ()

--length (extractInfohow (cursor $/ element "{http://www.w3.org/2005/Atom}title" &// content) `shouldBe` 1
--show cursor `shouldBe` "1"

filterRep :: FeedItemHW -> FeedItemHW -> [FeedItemHW]
filterRep new db = filterNew (replicate 10 new) (replicate 10 db)

differentTime :: FeedItemHW
differentTime = defFeedItem {_feedItemDate = afterDefTime}

differentTitle :: FeedItemHW
differentTitle = defFeedItem {_feedItemTitle = "Testing title"}

differentUrl :: FeedItemHW
differentUrl = defFeedItem {_feedItemUrl = "Testing url"}

differentList :: [FeedItemHW]
differentList = [differentTime, differentUrl, differentTitle]

veritasiumxml :: Data.ByteString.Lazy.ByteString
veritasiumxml =
  [Text.RawString.QQ.r|<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns:yt="http://www.youtube.com/xml/schemas/2015" xmlns:media="http://search.yahoo.com/mrss/" xmlns="http://www.w3.org/2005/Atom">
 <link rel="self" href="http://www.youtube.com/feeds/videos.xml?channel_id=UCHnyfMqiRRG1u-2MsSQLbXA"/>
 <id>yt:channel:UCHnyfMqiRRG1u-2MsSQLbXA</id>
 <yt:channelId>UCHnyfMqiRRG1u-2MsSQLbXA</yt:channelId>
 <title>Veritasium</title>
 <link rel="alternate" href="https://www.youtube.com/channel/UCHnyfMqiRRG1u-2MsSQLbXA"/>
 <author>
  <name>Veritasium</name>
  <uri>https://www.youtube.com/channel/UCHnyfMqiRRG1u-2MsSQLbXA</uri>
 </author>
 <published>2010-07-21T07:18:02+00:00</published>
 <entry>
  <id>yt:video:Grv1RJkdyqI</id>
  <yt:videoId>Grv1RJkdyqI</yt:videoId>
  <yt:channelId>UCHnyfMqiRRG1u-2MsSQLbXA</yt:channelId>
  <title>I Asked Bill Gates What's The Next Crisis?</title>
  <link rel="alternate" href="https://www.youtube.com/watch?v=Grv1RJkdyqI"/>
  <author>
   <name>Veritasium</name>
   <uri>https://www.youtube.com/channel/UCHnyfMqiRRG1u-2MsSQLbXA</uri>
  </author>
  <published>2021-02-04T14:00:03+00:00</published>
  <updated>2021-02-05T22:05:28+00:00</updated>
  <media:group>
   <media:title>I Asked Bill Gates What's The Next Crisis?</media:title>
   <media:content url="https://www.youtube.com/v/Grv1RJkdyqI?version=3" type="application/x-shockwave-flash" width="640" height="390"/>
   <media:thumbnail url="https://i4.ytimg.com/vi/Grv1RJkdyqI/hqdefault.jpg" width="480" height="360"/>
   <media:description>I got the chance to interview Bill Gates so I asked him: Will Covid-19 be the last pandemic? How does he deal with misinformation and conspiracy theories? And what is the next disaster? The Foundation Letter is here: https://ve42.co/BG21

Special thanks to Patreon supporters:
Ludovic Robillard, jim buckmaster, Robert, fanime96, Marc Forand, Juan Benet, Robert Blum, Grace O'Maille KRON x Arc iOS, Richard Sundvall, Lee Redden, Vincent, Lyvann Ferrusca, Alfred Wallace, Arjun Chakroborty, Joar Wandborg, Clayton Greenwell, Pindex , Michael Krugman, Cy 'kkm' K'Nelson, Sam Lutfi, Ron Neal

Thanks to Petr Lebedev for early edits and Jonny Hyman for feedback</media:description>
   <media:community>
    <media:starRating count="91842" average="4.62" min="1" max="5"/>
    <media:statistics views="1349150"/>
   </media:community>
  </media:group>
 </entry>
</feed>
|]
