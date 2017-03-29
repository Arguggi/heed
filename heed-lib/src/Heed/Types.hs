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
        FeedListEmpty :: HeedError
    deriving (Show)

-- | Show 'HeedError' for humans
showUserHeedError :: HeedError -> T.Text
showUserHeedError InvalidFeedQuery = "Invalid feed information sent"
showUserHeedError InvalidXML = "Invalid feed format"
showUserHeedError InvalidFeedData = "Invalid feed data"
showUserHeedError InvalidOPMLData = "Invalid opml data"
showUserHeedError MultipleFeedsSameUrl = "Feed is already present in the database"
showUserHeedError FeedListEmpty = "Feed has no valid items "
showUserHeedError (InvalidUrl _) = "Invalid URL"
showUserHeedError (DownloadFailed _) = "Download failed"
showUserHeedError (HSqlException _) = "Database error"

instance Exception HeedError

data ChanUpdates
    = SendItems FeedInfoHR
                Int64
    | UpdateFeedList FeedInfoHR

-- | Keep track of every 'FeedInfoIdH' that runs an update thread
type ThreadState = Map.Map FeedInfoIdH ThreadId

data BackendConf = BackendConf
    { _dbConnection :: PG.Connection -- ^ Common postgresql connection
    , _httpManager :: Manager -- ^ Commond download Manager
    , _updateChan :: BChan.BroadcastChan BChan.In ChanUpdates -- ^ Common 'BChan.BroadcastChan'.
    -- Every feed thread will send updates to this and every user listens and filters
    -- the updates according to what feed he's subscribed to.
    , _timedLogger :: Log.TimedFastLogger
    , _threadMap :: TVar ThreadState -- ^ Map of every feed thread running
    }

makeLenses ''BackendConf

-- | Why tty quit
data ExitType
    = UserExit -- ^ User pressed exit button
    | WsDisconnect -- ^ We lost connection to the server
    deriving (Eq, Show)

-- | Used for backend
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

-- | Used for testing
newtype Testing a = Testing
    { runTesting :: ReaderT PG.Connection IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader PG.Connection)

-- | Stands for runBackend
runBe
    :: (MonadIO m)
    => BackendConf -> Backend a -> m (Either HeedError a)
runBe conf = liftIO . flip runReaderT conf . runExceptT . runBackend

-- | Used in testing
runTest
    :: (MonadIO m)
    => PG.Connection -> Testing a -> m a
runTest conn = liftIO . flip runReaderT conn . runTesting

-- | mtl class for fetching urls
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

-- | mtl class for interacting with the database
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

-- | Catch exceptions and transform them into a 'HeedError'
catchExcep
    :: (Exception e, MonadCatch m, MonadError HeedError m)
    => Proxy e -- ^ Type of exception to catch
    -> (e -> HeedError) -- ^ How to make it into a 'HeedError'
    -> m a -- ^ Action
    -> m a
catchExcep _ excep action = action `catch` (throwError . excep)

-- | 'catchExcep' specialized to 'PG.SqlError'
catchSql
    :: (MonadCatch m, MonadError HeedError m)
    => (PG.SqlError -> HeedError)
    -> m a -- ^ Action
    -> m a
catchSql = catchExcep (Proxy :: Proxy PG.SqlError)

-- | 'catchExcep' specialized to 'HTTPException'
catchHttp
    :: (MonadCatch m, MonadError HeedError m)
    => (HttpException -> HeedError) -> m a -> m a
catchHttp = catchExcep (Proxy :: Proxy HttpException)

-- | mtl class for logging messages on stdout
class (Monad m) =>
      MonadLog m where
    logMsg :: T.Text -> m ()

instance MonadLog Backend where
    logMsg msg = do
        l <- asks _timedLogger
        liftIO $ logMsgIO l msg

-- | 'logMsg' in 'IO'
logMsgIO :: Log.TimedFastLogger -> T.Text -> IO ()
logMsgIO l msg =
    l $ \time -> Log.toLogStr time <> " - " <> Log.toLogStr msg <> Log.toLogStr ("\n" :: T.Text)

-- | mtl class for getting time
class (Monad m) =>
      MonadTime m where
    getTime :: m UTCTime

instance MonadTime IO where
    getTime = getCurrentTime

instance MonadTime Backend where
    getTime = liftIO getCurrentTime

-- | lift a 'Maybe' k
liftJust
    :: (MonadError HeedError m)
    => HeedError -- ^ throws this error when 'Nothing'
    -> Maybe a
    -> m a
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
