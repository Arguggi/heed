{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Heed.Types
    ( Backend
    , BackendConf(..)
    , ChanUpdates(..)
    , ExitType(..)
    , HeedError(..)
    , MonadDb
    , MonadHttp
    , MonadLog
    , MonadTime
    , ThreadState
    , catchHttp
    , downloadUrl
    , execQuery
    , getTime
    , liftJust
    , logMsg
    , logMsgIO
    , runBe
    , runQueryNoT
    , runTest
    , runTransaction
    , showUserHeedError
    -- * 'BackendConf' Lenses
    , updateChan
    , timedLogger
    , dbConnection
    , httpManager
    , threadMap
    ) where

import Control.Concurrent (ThreadId)
import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Concurrent.STM (TVar)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text as T
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import Heed.Database (FeedInfoHR, FeedInfoIdH)
import Network.HTTP.Client hiding (Proxy)
import qualified Opaleye.Trans as OT
import qualified System.Log.FastLogger as Log

data HeedError where
    InvalidFeedQuery :: HeedError
    InvalidXML :: HeedError
    InvalidFeedData :: HeedError
    InvalidOPMLData :: HeedError
    MultipleFeedsSameUrl :: HeedError
    InvalidUrl :: HttpException -> HeedError
    DownloadFailed :: HttpException -> HeedError
    HSqlException :: PG.SqlError -> HeedError
    deriving (Show)

showUserHeedError :: HeedError -> T.Text
showUserHeedError InvalidFeedQuery = "Invalid feed information sent"
showUserHeedError InvalidXML = "Invalid feed format"
showUserHeedError InvalidFeedData = "Invalid feed data"
showUserHeedError InvalidOPMLData = "Invalid opml data"
showUserHeedError MultipleFeedsSameUrl = "Feed is already present in the database"
showUserHeedError (InvalidUrl _) = "Invalid URL"
showUserHeedError (DownloadFailed _) = "Download failed"
showUserHeedError (HSqlException _) = "Database error"

instance Exception HeedError

data ChanUpdates
    = SendItems FeedInfoHR
                Int64
    | UpdateFeedList FeedInfoHR

type ThreadState = Map.Map FeedInfoIdH ThreadId

data BackendConf = BackendConf
    { _dbConnection :: PG.Connection
    , _httpManager :: Manager
    , _updateChan :: BChan.BroadcastChan BChan.In ChanUpdates
    , _timedLogger :: Log.TimedFastLogger
    , _threadMap :: TVar ThreadState
    }

makeLenses ''BackendConf

data ExitType
    = UserExit
    | WsDisconnect
    deriving (Eq, Show)

newtype Backend a = Backend
    { runBackend :: ExceptT HeedError (ReaderT BackendConf IO) a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadCatch
               , MonadError HeedError
               , MonadIO
               , MonadReader BackendConf
               , MonadThrow
               )

newtype Testing a = Testing
    { runTesting :: ReaderT PG.Connection IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader PG.Connection)

runBe
    :: (MonadIO m)
    => BackendConf -> Backend a -> m (Either HeedError a)
runBe conf = liftIO . flip runReaderT conf . runExceptT . runBackend

runTest
    :: (MonadIO m)
    => PG.Connection -> Testing a -> m a
runTest conn = liftIO . flip runReaderT conn . runTesting

class Monad m =>
      MonadHttp m where
    downloadUrl :: T.Text -> m BSL.ByteString

instance MonadHttp IO where
    downloadUrl url = do
        manager <- newManager defaultManagerSettings
        request <- parseRequest $ "GET " <> T.unpack url
        responseBody <$> httpLbs request manager

instance MonadHttp Backend where
    downloadUrl url = do
        manager <- asks _httpManager
        request <- catchHttp InvalidUrl . parseRequest $ "GET " <> T.unpack url
        catchHttp DownloadFailed . liftIO $ responseBody <$> httpLbs request manager

class Monad m =>
      MonadDb m where
    execQuery :: OT.Transaction a -> m a

instance MonadDb Backend where
    execQuery query = do
        db <- asks _dbConnection
        catchSql HSqlException $ runTransaction db query

instance MonadDb Testing where
    execQuery query = do
        db <- ask
        runTransaction db query

catchExcep
    :: (Exception e, MonadCatch m, MonadError HeedError m)
    => Proxy e -> (e -> HeedError) -> m a -> m a
catchExcep _ excep action = action `catch` (throwError . excep)

catchSql
    :: (MonadCatch m, MonadError HeedError m)
    => (PG.SqlError -> HeedError) -> m a -> m a
catchSql = catchExcep (Proxy :: Proxy PG.SqlError)

catchHttp
    :: (MonadCatch m, MonadError HeedError m)
    => (HttpException -> HeedError) -> m a -> m a
catchHttp = catchExcep (Proxy :: Proxy HttpException)

class (Monad m) =>
      MonadLog m where
    logMsg :: T.Text -> m ()

instance MonadLog Backend where
    logMsg msg = do
        l <- asks _timedLogger
        liftIO $ logMsgIO l msg

logMsgIO :: Log.TimedFastLogger -> T.Text -> IO ()
logMsgIO l msg =
    l $ \time -> Log.toLogStr time <> " - " <> Log.toLogStr msg <> Log.toLogStr ("\n" :: T.Text)

class (Monad m) =>
      MonadTime m where
    getTime :: m UTCTime

instance MonadTime IO where
    getTime = getCurrentTime

instance MonadTime Backend where
    getTime = liftIO getCurrentTime

liftJust
    :: (MonadError HeedError m)
    => HeedError -> Maybe a -> m a
liftJust e Nothing = throwError e
liftJust _ (Just a) = return a

-- | Run all queries in a transaction
runTransaction
    :: (MonadIO m)
    => PG.Connection -> OT.Transaction a -> m a
runTransaction conn trans = OT.runOpaleyeT conn $ OT.transaction trans

-- | Run all queries without starting a transaction
runQueryNoT
    :: (MonadIO m)
    => PG.Connection -> OT.Transaction a -> m a
runQueryNoT conn trans = OT.runOpaleyeT conn $ OT.run trans
