module Heed.Types where

import Control.Exception.Base
import qualified Database.PostgreSQL.Simple as PG
import Network.HTTP.Client
       (HasHttpManager(..), Manager)

class HasDbConnection a  where
    getDbConnection :: a -> PG.Connection

data HeedError
    = InvalidXML
    | InvalidUrl
    | InvalidFeedData
    | DownloadFailed
    | MultipleFeedsSameUrl
    deriving (Eq, Show)

instance Exception HeedError

data BackendConf = BackendConf
    { dbConnection :: PG.Connection
    , httpManager :: Manager
    }

instance HasHttpManager BackendConf where
    getHttpManager = httpManager

instance HasDbConnection BackendConf where
    getDbConnection = dbConnection
