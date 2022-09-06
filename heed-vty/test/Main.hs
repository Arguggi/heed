{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( main
    ) where

import qualified Brick.Widgets.List as BL
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import Heed.Commands (FeFeedInfo)
import Heed.Vty.MainWidget (insertInOrder)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

main :: IO ()
main =
    hspec $
    describe "insertInOrder" $
    it "inserts in order and correctly updates index" $
    property (\(x :: FeFeedInfo) -> prop_insertInOrder x)

isSorted
    :: (Ord a)
    => [a] -> Bool
isSorted xs = all (uncurry (<=)) $ zip xs (tail xs)

prop_insertInOrder
    :: (Show e, Arbitrary e, Ord e)
    => e -> BL.List ListName e -> Bool
prop_insertInOrder e list =
    case list ^. BL.listSelectedL of
        Nothing -> new ^. BL.listSelectedL == Just 0
        (Just _) ->
            (snd <$> BL.listSelectedElement new) == (snd <$> BL.listSelectedElement list) &&
            isSorted (Vec.toList (new ^. BL.listElementsL))
  where
    new = insertInOrder e list

data ListName =
    ListName
    deriving (Generic, Show)

instance Arbitrary ListName where
    arbitrary = return ListName

instance (Arbitrary n, Arbitrary e, Ord e) =>
         Arbitrary (BL.List n e) where
    arbitrary = do
        name <- arbitrary
        l <- orderedList
        return $ BL.list name (Vec.fromList l) 1
