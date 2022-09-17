{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Heed.Server (genAuthMain) where

import qualified BroadcastChan as BChan
import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar)
import Control.Monad (forM, forM_, forever, join, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Crypto.KDF.BCrypt (validatePassword)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Serialize (Serialize, decode, encode)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import qualified Heed.Commands as HC
import Heed.Crypto (generateToken)
import Heed.Database (UserFeedInfoPrefHR)
import qualified Heed.Database as DB
import Heed.Extract
  ( addFeed,
    broadcastUpdate,
    forceUpdate,
    startUpdateThread,
  )
import qualified Heed.Query as HQ
import Heed.Types
  ( BackendConf,
    ChanUpdates (..),
    HeedError,
    ThreadState,
    execDelete,
    execInsert,
    execSelect,
    execUpdate,
    runBe,
    showUserHeedError,
    threadMap,
    updateChan,
  )
import Heed.Utils (Port, fork_)
import Lens.Micro.Platform ((^.))
import Network.HTTP.Types (badRequest400)
import Network.Socket (SockAddr)
import Network.Wai
  ( Application,
    Request,
    requestHeaders,
    responseLBS,
  )
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnOpen)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSC
import qualified Safe
import Servant (throwError)
import Servant.API
  ( OctetStream,
    Post,
    ReqBody,
    (:<|>) ((:<|>)),
    (:>),
  )
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Raw (Raw)
import Servant.Server
  ( Context (EmptyContext, (:.)),
    Handler,
    Server,
    Tagged (..),
    err401,
    serveWithContext,
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )

import Network.Wai.Middleware.Prometheus ( def, prometheus)

-- | run our server
genAuthMain :: BackendConf -> Port -> IO ()
genAuthMain conf port =
  runSettings settings (prometheus def $ serveWithContext genAuthAPI (genAuthServerContext conf) (genAuthServer conf))
    where settings = setOnOpen connected $ setPort port defaultSettings

connected :: SockAddr -> IO Bool
connected sockAddr = do
    putStrLn $ (show sockAddr) <> " connected"
    return True

data UserName = UserName
  { unUserName :: Text,
    unUserId :: Int
  }

-- | Our API, with auth-protection
type AuthGenAPI = "auth" :> (ReqBody '[OctetStream] HC.AuthData :> Post '[OctetStream] HC.Token) :<|> AuthProtect "cookie-auth" :> Raw

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | A method that, when given a password, will return a Tok.
-- This is our bespoke (and bad) authentication logic.
lookupTok :: BackendConf -> ByteString -> Handler UserName
lookupTok conf authToken = do
  tokenInfo :: Either HeedError (Maybe DB.UserH) <- (fmap listToMaybe) <$> (runBe conf $ execSelect $ HQ.verifyToken (decodeUtf8 authToken))
  case tokenInfo of
    Right (Just user) -> return $ UserName (DB._userName user) (DB._getUserId . DB._userId $ user)
    _ -> throwError err401

-- | The auth handler wraps a function from Request -> Handler Tok
-- we look for a Cookie and pass the value of the cookie to `lookupTok`.
authHandler :: BackendConf -> AuthHandler Request UserName
authHandler conf =
  let handler req =
        case lookup "auth-token" (requestHeaders req) of
          Nothing -> throwError err401
          Just authCookieKey -> lookupTok conf authCookieKey
   in mkAuthHandler handler

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: BackendConf -> Context (AuthHandler Request UserName ': '[])
genAuthServerContext conf = authHandler conf :. EmptyContext

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = UserName

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We dont' worry about the authentication instrumentation here,
-- that is taken care of by supplying context
genAuthServer :: BackendConf -> Server AuthGenAPI
genAuthServer conf = checkCreds conf :<|> app conf

checkCreds :: BackendConf -> HC.AuthData -> Handler HC.Token
checkCreds conf (HC.AuthData usern pw) = do
  liftIO . TIO.putStrLn $ "Received login request for " <> usern
  userDbM :: Either HeedError [DB.UserH] <- runBe conf $ execSelect (HQ.getUser usern)
  case fmap head userDbM of
    Right userDb ->
      if validatePassword (encodeUtf8 pw) (encodeUtf8 . DB._userPassword $ userDb)
        then do
          liftIO . TIO.putStrLn $ "Valid password for " <> usern
          newToken <- generateToken
          _ <- runBe conf $ execUpdate $ HQ.saveTokenDb newToken (DB._userId userDb)
          return $ HC.Token newToken
        else do
          liftIO . TIO.putStrLn $ "Invalid password for " <> usern
          throwError err401
    Left _ -> do
      liftIO . TIO.putStrLn $ "No such user " <> usern
      throwError err401

app :: BackendConf -> UserName -> Tagged Handler Application
app conf uname = Tagged $ websocketsOr connectionOptions (wsApp conf uname) backupApp
  where
    connectionOptions =
      WS.ConnectionOptions
        (return ())
        (WS.PermessageDeflateCompression WS.defaultPermessageDeflate)
        True
        WSC.NoSizeLimit
        WSC.NoSizeLimit

wsApp :: BackendConf -> UserName -> WS.ServerApp
wsApp conf uname pending_conn = do
  TIO.putStrLn $ unUserName uname <> " opened a websocket connection"
  -- User heed protocol
  let ar = WS.AcceptRequest (Just "heed") []
      uid = DB.UserId $ unUserId uname
  conn <- WS.acceptRequestWith pending_conn ar
  WSC.withPingThread conn 10 (return ()) $ do
    -- As soon as someone connects get the relevant feeds from the db
    -- since we will have to send them once the client tells us it's ready
    unreadFeeds <- fromRight [] <$> (runBe conf $ execSelect $ HQ.getUserUnreadFeedInfo uid)
    let sortedUnread = sort unreadFeeds
    allfeeds <- fromRight [] <$> (runBe conf $ execSelect $ HQ.getUserFeeds uid)
    -- Create a new Broadcast channel where updates are pushed from the update threads
    updateListener <- BChan.newBChanListener (conf ^. updateChan)
    sendUpdates updateListener conn allfeeds
    forever $ do
      commandM <- decode <$> WS.receiveData conn
      let command = either (const HC.InvalidReceived) id commandM
      case command of
        HC.Initialized -> do
          sendDown conn $ HC.Status (unUserName uname)
          sendDown conn $ HC.Feeds sortedUnread
        HC.GetFeedItems feedId -> do
          items <- fromRight [] <$> (runBe conf $ execSelect $ HQ.getUserItems uid (DB.FeedInfoId feedId))
          sendDown conn (HC.FeedItems items)
        HC.ItemRead itemId -> void . runBe conf $ execDelete $ HQ.readFeed uid (DB.FeedItemId itemId)
        HC.FeedRead feedId ->
          void . runBe conf $ HQ.allItemsRead (DB.FeedInfoId feedId) uid
        HC.NewFeed url updateEvery -> do
          newFeed <- runBe conf $ addFeed url updateEvery uid
          case newFeed of
            Left e -> do
                print e
                sendError conn e
            Right (feed, num) -> do
              now <- getCurrentTime
              _ <- BChan.writeBChan (conf ^. updateChan) (UpdateFeedList feed)
              _ <- BChan.writeBChan (conf ^. updateChan) (SendItems feed num)
              tid <- startUpdateThread now conf feed
              fork_ $ updateThreadMap (conf ^. threadMap) (feed ^. DB.feedInfoId) tid
              sendDown conn (HC.FeedAdded url)
        HC.ForceRefresh fid -> do
          updateE <- runBe conf $ forceUpdate (DB.FeedInfoId fid)
          case updateE of
            Left e -> sendError conn e
            Right update -> broadcastUpdate update (conf ^. updateChan) >> return ()
        HC.GetSingleFeedInfo fid -> do
          infoM <- runBe conf $ execSelect $ HQ.allFeedInfo (DB.FeedInfoId fid)
          let infoE = fmap Safe.headMay infoM
          case infoE of
            Right (Just info) -> sendDown conn (HC.EditableFeedInfo $ HC.toFeEditFeed info)
            _ -> sendDown conn (HC.BackendError "Invalid feed")
        HC.UpdatedFeedInfo feedEdit -> do
          let newName = feedEdit ^. HC.feEditName
              newInterval = feedEdit ^. HC.feEditUpdateEvery
              fid = DB.FeedInfoId $ feedEdit ^. HC.feEditId
              newUserPref = DB.UserFeedInfoPref uid fid newName
          -- Update Feed name
          oldPrefM :: Either HeedError (Maybe UserFeedInfoPrefHR) <- (fmap listToMaybe) <$> (runBe conf $ execSelect (HQ.userFeedName fid uid))
          case oldPrefM of
            Left _ -> putStrLn "SQL query failed"
            Right Nothing -> void $ runBe conf $ execInsert $ HQ.insertUserPrefName newUserPref
            Right (Just oldPref) ->
              when ((oldPref ^. DB.prefName) /= newName) (void $ runBe conf $ execUpdate $ HQ.updateUserPrefName newUserPref)
          sendDown conn $ HC.FeedInfoUpdated (feedEdit ^. HC.feEditId, newName)
          -- Get old update interval
          oldUpdateIntervalM :: Either HeedError (Maybe Int) <- (fmap listToMaybe) <$> (runBe conf $ execSelect (HQ.feedUpdateInterval fid))
          needRestartM <-
            forM oldUpdateIntervalM $ \_ ->
              (fmap listToMaybe) <$> (runBe conf $ execUpdate (HQ.updateFeedInterval fid newInterval))
          -- Restart the update thread with the new interval
          -- and kill old thread
          forM_ (join needRestartM) $ \updated -> do
            now <- getCurrentTime
            tid <- startUpdateThread now conf (fromJust updated)
            updateThreadMap (conf ^. threadMap) fid tid
        HC.InvalidReceived -> putStrLn "Invalid command received"

sendError :: WS.Connection -> HeedError -> IO ()
sendError conn e = sendDown conn (HC.BackendError (showUserHeedError e))

sendDown ::
  (Serialize a) =>
  WS.Connection ->
  a ->
  IO ()
sendDown conn info = WS.sendBinaryData conn $ encode info

sendUpdates ::
  BChan.BroadcastChan BChan.Out ChanUpdates ->
  WS.Connection ->
  [DB.FeedInfoHR] ->
  IO ()
sendUpdates bchan wsconn userfeeds =
  fork_ . flip iterateM_ userfeeds $ \feeds -> do
    update <- BChan.readBChan bchan
    case update of
      Nothing -> putStrLn "Failed to send items" >> return []
      -- If user is subbed to updated feed find and send it
      Just (SendItems feed numItems) -> do
        forM_ (maybeSubFeed feed feeds) (sendNewItems wsconn . toFrontEndFeedInfo numItems)
        return feeds
      Just (UpdateFeedList feed) -> return $ feed : feeds
  where
    maybeSubFeed feed = find (\f -> (feed ^. DB.feedInfoId) == (f ^. DB.feedInfoId))

sendNewItems :: WS.Connection -> HC.FeFeedInfo -> IO ()
sendNewItems wsconn finfo = sendDown wsconn (HC.NewItems finfo)

toFrontEndFeedInfo :: Int64 -> DB.FeedInfoHR -> HC.FeFeedInfo
toFrontEndFeedInfo unread beInfo =
  HC.FeFeedInfo' (beInfo ^. DB.feedInfoId . DB.getFeedInfoId) (beInfo ^. DB.feedInfoName) unread

backupApp :: Application
backupApp _ respond = respond $ responseLBS badRequest400 [] "Bad request"

-- Stolen from monad-loops
{-# SPECIALIZE iterateM_ :: (a -> IO a) -> a -> IO b #-}

-- | Execute an action forever, feeding the result of each execution as the
--  input to the next.
iterateM_ ::
  Monad m =>
  (a -> m a) ->
  a ->
  m b
iterateM_ f = g
  where
    g x = f x >>= g

-- | A new thead has been started for a feed that we maybe already had in the db.
--   If we already had the feed we have to kill the old thread
--   This also has to be done when we modify a feed info since we may have modified
--   the updateEvery field
updateThreadMap ::
  (MonadIO m) =>
  TVar ThreadState ->
  DB.FeedInfoIdH ->
  ThreadId ->
  m ()
updateThreadMap tvar fid newThreadId =
  liftIO $ do
    oldTidM <-
      atomically $ do
        tidsmap <- readTVar tvar
        let !oldTid = Map.lookup fid tidsmap
        modifyTVar' tvar (Map.alter (\_ -> Just newThreadId) fid)
        return oldTid
    forM_ oldTidM killThread
