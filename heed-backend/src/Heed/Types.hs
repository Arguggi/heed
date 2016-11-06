{-# LANGUAGE GADTs #-}

module Heed.Types where

import Control.Exception.Base
import qualified Database.PostgreSQL.Simple as PG
import Network.HTTP.Client
       (HasHttpManager(..), HttpException, Manager)

class HasDbConnection a  where
    getDbConnection :: a -> PG.Connection

data HeedError where
        InvalidXML :: HeedError
        InvalidFeedData :: HeedError
        MultipleFeedsSameUrl :: HeedError
        InvalidUrl :: HttpException -> HeedError
        DownloadFailed :: HttpException -> HeedError
        HSqlException :: PG.SqlError -> HeedError
    deriving (Show)

instance Exception HeedError

data BackendConf = BackendConf
    { dbConnection :: PG.Connection
    , httpManager :: Manager
    }

instance HasHttpManager BackendConf where
    getHttpManager = httpManager

instance HasDbConnection BackendConf where
    getDbConnection = dbConnection

data CredStatus
    = Verified
    | Unverified
