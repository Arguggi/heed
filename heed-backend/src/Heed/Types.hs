{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heed.Types where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL
import Data.Proxy
import Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import Heed.Query
import Network.HTTP.Client hiding (Proxy)
import qualified Opaleye.Trans as OT

data HeedError where
        InvalidXML :: HeedError
        InvalidFeedData :: HeedError
        InvalidOPMLData :: HeedError
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

data CredStatus
    = Verified
    | Unverified

newtype Backend a = Backend
    { runBackend :: ExceptT HeedError (ReaderT BackendConf IO) a
    } deriving (Functor, Applicative, Monad, MonadError HeedError, MonadReader BackendConf, MonadIO, MonadCatch, MonadThrow)

runBe
    :: (MonadIO m)
    => BackendConf -> Backend a -> m (Either HeedError a)
runBe conf = liftIO . flip runReaderT conf . runExceptT . runBackend

class Monad m =>
      MonadHttp m  where
    downloadUrl :: T.Text -> m BSL.ByteString

instance MonadHttp Backend where
    downloadUrl url = do
        manager <- asks httpManager
        request <- catchHttp InvalidUrl . parseRequest $ "GET " ++ T.unpack url
        catchHttp DownloadFailed . liftIO $ responseBody <$> httpLbs request manager

class Monad m =>
      MonadDb m  where
    execQuery :: OT.Transaction a -> m a

instance MonadDb Backend where
    execQuery query = do
        db <- asks dbConnection
        catchSql HSqlException $ runTransaction db query

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
      MonadStdOut m  where
    stdOut :: T.Text -> m ()

instance MonadStdOut Backend where
    stdOut = liftIO . TIO.putStrLn

class (Monad m) =>
      MonadTime m  where
    getTime :: m UTCTime

instance MonadTime Backend where
    getTime = liftIO getCurrentTime

liftJust
    :: (MonadError HeedError m)
    => HeedError -> Maybe a -> m a
liftJust e Nothing = throwError e
liftJust _ (Just a) = return a