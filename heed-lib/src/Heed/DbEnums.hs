{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Heed.DbEnums
  ( ItemsDate (Missing, Present),
    SqlItemsDate,
  )
where

import qualified Data.Profunctor.Product.Default as ProDef
import Data.Serialize (Serialize)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Opaleye as O
import qualified Opaleye.Experimental.Enum as Enum

-- | Iso to Bool, only to avoid boolean blindness
data ItemsDate
  = -- | Feed has no items on dates and the current time will be used
    Missing
  | -- | Feed has dates on items
    Present
  deriving (Typeable, Show, Generic, Eq)

instance Serialize ItemsDate

-- | Postgres type
data SqlItemsDate

fromSqlItemDatesString :: String -> Maybe ItemsDate
fromSqlItemDatesString "missing" = Just Missing
fromSqlItemDatesString "present" = Just Present
fromSqlItemDatesString _ = Nothing

toSqlItemDatesString :: ItemsDate -> String
toSqlItemDatesString Missing = "missing"
toSqlItemDatesString Present = "present"

sqlItemsDateMapper :: Enum.EnumMapper SqlItemsDate ItemsDate
sqlItemsDateMapper = Enum.enumMapper "itemdates" fromSqlItemDatesString toSqlItemDatesString

instance O.DefaultFromField SqlItemsDate ItemsDate where
  defaultFromField = Enum.enumFromField sqlItemsDateMapper

instance ProDef.Default O.ToFields ItemsDate (O.Field SqlItemsDate) where
  def = Enum.enumToFields sqlItemsDateMapper
