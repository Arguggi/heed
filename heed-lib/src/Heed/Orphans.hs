{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Only exports orphan instances
-}
module Heed.Orphans
      -- * Orphans
    (
    ) where

import Data.Serialize (Serialize(get, put))
import Data.Serialize.Text ()
import Data.Time.Calendar
       (Day(ModifiedJulianDay), toModifiedJulianDay)
import Data.Time.Clock

instance Serialize UTCTime where
    get = UTCTime <$> get <*> get
    put (UTCTime day time) = put day >> put time

instance Serialize Day where
    get = fmap ModifiedJulianDay get
    put = put . toModifiedJulianDay

instance Serialize DiffTime where
    get = fmap picosecondsToDiffTime get
    put = put . diffTimeToPicoseconds
