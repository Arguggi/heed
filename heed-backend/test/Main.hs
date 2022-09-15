{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Lazy
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (parseISO8601)
import Heed.Database (FeedInfo (..), FeedItem (_feedItemComments), FeedItemHW, afterDefTime, defFeedInfo, defFeedItem, _feedItemDate, _feedItemTitle, _feedItemUrl)
import Heed.Extract (filterNew)
import qualified Heed.Extract
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Feed.Import (parseFeedSource)

main :: IO ()
main = do
  veritasiumparsed <- fromJust . parseFeedSource <$> Data.ByteString.Lazy.readFile "test/feeds/veritasium.xml"
  laurenceparsed <- fromJust . parseFeedSource <$> Data.ByteString.Lazy.readFile "test/feeds/laurencejones.xml"
  joachimparsed <- fromJust . parseFeedSource <$> Data.ByteString.Lazy.readFile "test/feeds/joachim.xml"
  duplodeparsed <- fromJust . parseFeedSource <$> Data.ByteString.Lazy.readFile "test/feeds/duplode.xml"
  now <- getCurrentTime
  hspec $ do
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
    describe "Parses RSS feeds" $ do
      it "Builds the correct url for items with relative urls" $ do
        let parsed = Heed.Extract.extractInfoFromFeed now "Url" laurenceparsed
        case parsed of
          Nothing -> return ()
          Just (info, items) -> do
            info `shouldBe` defFeedInfo {_feedInfoName = "Lawrence Jones"}
            items `shouldBe` [laurenceEntry]
      it "Builds the correct url for items with absolute urls" $ do
        let parsed = Heed.Extract.extractInfoFromFeed now "Url" joachimparsed
        case parsed of
          Nothing -> return ()
          Just (info, items) -> do
            info `shouldBe` defFeedInfo {_feedInfoName = "nomeata’s mind shares"}
            items `shouldBe` [joachimentry]
      it "Parses dates correctly" $ do
        let parsed = Heed.Extract.extractInfoFromFeed now "Url" duplodeparsed
        case parsed of
          Nothing -> return ()
          Just (info, items) -> do
            info `shouldBe` defFeedInfo {_feedInfoName = "The Life Monadic"}
            items `shouldBe` [duplodeentry1, duplodeentry2]
    describe "Parses XML feeds" $ do
      it "Youtube xml" $ do
        let parsed = Heed.Extract.extractInfoFromFeed now "Url" veritasiumparsed
        case parsed of
          Nothing -> return ()
          Just (info, items) -> do
            info `shouldBe` defFeedInfo {_feedInfoName = "Veritasium"}
            items `shouldBe` [veritasiumEntry]

veritasiumEntry :: FeedItemHW
veritasiumEntry =
  defFeedItem
    { _feedItemTitle = "I Asked Bill Gates What's The Next Crisis?",
      _feedItemUrl = "https://www.youtube.com/watch?v=Grv1RJkdyqI",
      _feedItemDate = fromJust $ parseISO8601 "2021-02-04T14:00:03+00:00"
    }

laurenceEntry :: FeedItemHW
laurenceEntry =
  defFeedItem
    { _feedItemTitle = "Building workflows: technical deep-dive and evaluation",
      _feedItemUrl = "https://blog.lawrencejones.dev/workflows/",
      _feedItemDate = fromJust $ parseISO8601 "2022-09-14T12:00:00+00:00"
    }

joachimentry :: FeedItemHW
joachimentry =
  defFeedItem
    { _feedItemTitle = "rec-def: Dominators case study",
      _feedItemUrl = "http://www.joachim-breitner.de/blog/795-rec-def__Dominators_case_study",
      _feedItemDate = fromJust $ parseISO8601 "2022-09-15T10:27:19+02:00",
      _feedItemComments = Just "http://www.joachim-breitner.de/blog/795-rec-def__Dominators_case_study#comments"
    }

duplodeentry1 :: FeedItemHW
duplodeentry1 =
  defFeedItem
    { _feedItemTitle = "Every Distributive is Representable",
      _feedItemUrl = "https://duplode.github.io/posts/every-distributive-is-representable.html",
      _feedItemDate = fromJust $ parseISO8601 "2022-02-07T19:55:00+00:00"
    }

duplodeentry2 :: FeedItemHW
duplodeentry2 =
  defFeedItem
    { _feedItemTitle = "Traversable: A Remix",
      _feedItemUrl = "https://duplode.github.io/posts/traversable-a-remix.html",
      _feedItemDate = fromJust $ parseISO8601 "2017-05-19T07:30:00+00:00"
    }

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
