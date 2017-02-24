{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Heed.DbEnums
    ( ItemsDate(Missing, Present)
    , PGItemsDate
    ) where

import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))
import qualified Data.Profunctor as Pro
import qualified Data.Profunctor.Product.Default as ProDef
import Data.Serialize (Serialize)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Database.PostgreSQL.Simple.FromField as PG
import GHC.Generics
import qualified Opaleye as O

-- | Iso to Bool, only to avoid boolean blindness
data ItemsDate
    = Missing -- ^ Feed has no items on dates and the current time will be used
    | Present -- ^ Feed has dates on items
    deriving (Typeable, Show, Generic)

instance Serialize ItemsDate

-- | Postgres type
data PGItemsDate

missing, present, unexpected :: Text.Text
missing = "missing"

present = "present"

unexpected = "Unexpected itemsdate:"

instance PG.FromField ItemsDate where
    fromField f itemsDate = pgTextFromFieldNoTypeCheck f itemsDate >>= parseItemsDate
      where
        parseItemsDate itemsDateString
            | itemsDateString == missing = return Missing
            | itemsDateString == present = return Present
            | otherwise =
                PG.returnError PG.ConversionFailed f (Text.unpack $ unexpected <> itemsDateString)

instance O.QueryRunnerColumnDefault PGItemsDate ItemsDate where
    queryRunnerColumnDefault = O.fieldQueryRunnerColumn

constantColumnUsing
    :: O.Constant haskell (O.Column pgType)
    -> (haskell' -> haskell)
    -> O.Constant haskell' (O.Column pgType')
constantColumnUsing oldConstant f = Pro.dimap f O.unsafeCoerceColumn oldConstant

instance ProDef.Default O.Constant ItemsDate (O.Column PGItemsDate) where
    def =
        constantColumnUsing (ProDef.def :: O.Constant String (O.Column O.PGText)) itemsDateToString
      where
        itemsDateToString :: ItemsDate -> String
        itemsDateToString Missing = "missing"
        itemsDateToString Present = "present"

pgGuardNotNull :: PG.FieldParser B8.ByteString
pgGuardNotNull f mb =
    case mb of
        Nothing -> PG.returnError PG.UnexpectedNull f ""
        Just b -> return b

{-# INLINABLE pgGuardNotNull #-}
-- | Like the 'Pg.FromField' instance for 'Text.Text' but doesn't check the
-- 'Pg.Field' type. ENUMS WON'T WORK OTHERWISE! thanks to k0001 for the tip.
pgTextFromFieldNoTypeCheck :: PG.FieldParser Text.Text
pgTextFromFieldNoTypeCheck f mb = do
    b <- pgGuardNotNull f mb
    case Text.decodeUtf8' b of
        Left e -> PG.conversionError e
        Right t -> return t
{-# INLINABLE pgTextFromFieldNoTypeCheck #-}
