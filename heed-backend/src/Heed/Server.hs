{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Heed.Server where

import qualified Control.Concurrent.BroadcastChan as BChan
import Control.Lens hiding (Context)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (validatePassword)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Serialize (Serialize, decode, encode)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import qualified Heed.Commands as HC
import Heed.Crypto (generateToken)
import qualified Heed.Database as DB
import Heed.Extract
       (addFeed, broadcastUpdate, forceUpdate, startUpdateThread)
import qualified Heed.Query as HQ
import Heed.Types
       (BackendConf, dbConnection, runBe, runQueryNoT, runTransaction,
        showUserHeedError, updateChan)
import Heed.Utils (Port, fork_)
import Network.HTTP.Types (badRequest400)
import Network.Wai
       (Application, Request, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant (throwError)
import Servant.API
       ((:<|>)((:<|>)), (:>), OctetStream, Post, ReqBody)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Raw (Raw)
import Servant.Server
       (Context((:.), EmptyContext), Handler, Server, err401,
        serveWithContext)
import Servant.Server.Experimental.Auth
       (AuthHandler, AuthServerData, mkAuthHandler)

-- | run our server
genAuthMain :: BackendConf -> Port -> IO ()
genAuthMain conf port =
    run port (serveWithContext genAuthAPI (genAuthServerContext conf) (genAuthServer conf))

data UserName = UserName
    { unUserName :: Text
    , unUserId :: Int
    }

-- | Our API, with auth-protection
type AuthGenAPI = "auth" :> (ReqBody '[ OctetStream] HC.AuthData :> Post '[ OctetStream] HC.Token) :<|> AuthProtect "cookie-auth" :> Raw

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | A method that, when given a password, will return a Tok.
-- This is our bespoke (and bad) authentication logic.
lookupTok :: BackendConf -> ByteString -> Handler UserName
lookupTok conf authToken = do
    let dbConn = conf ^. dbConnection
    tokenInfo <- runTransaction dbConn $ HQ.verifyToken (decodeUtf8 authToken)
    case tokenInfo of
        Nothing -> throwError err401
        Just user -> return $ UserName (DB._userName user) (DB._getUserId . DB._userId $ user)

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
    liftIO $ putStrLn "Checking auth"
    let dbConn = conf ^. dbConnection
    userDbM <- runQueryNoT dbConn $ HQ.getUserDb usern
    case userDbM of
        Just userDb -> do
            liftIO $ putStrLn "got pw hash"
            let validPass =
                    validatePassword (encodeUtf8 pw) (encodeUtf8 . DB._userPassword $ userDb)
            if validPass
                then do
                    liftIO $ putStrLn "Verified"
                    newToken <- generateToken
                    _ <- runQueryNoT dbConn $ HQ.saveTokenDb newToken (DB._userId userDb)
                    return $ HC.Token newToken
                else (do liftIO $ putStrLn "Invalid password"
                         throwError err401)
        Nothing -> do
            liftIO $ putStrLn "No such user"
            throwError err401

app :: BackendConf -> UserName -> Application
app conf uname = websocketsOr WS.defaultConnectionOptions (wsApp conf uname) backupApp

wsApp :: BackendConf -> UserName -> WS.ServerApp
wsApp conf uname pending_conn = do
    TIO.putStrLn $ unUserName uname <> " opened a websocket connection"
    -- User heed protocol
    let ar = WS.AcceptRequest (Just "heed") []
        dbConn = conf ^. dbConnection
        uid = DB.UserId $ unUserId uname
    conn <- WS.acceptRequestWith pending_conn ar
    WS.forkPingThread conn 10
    -- As soon as someone connects get the relevant feeds from the db
    -- since we will have to send them once the client tells us it's ready
    unreadFeeds <- runQueryNoT dbConn $ HQ.getUserUnreadFeedInfo uid
    let sortedUnread = sort unreadFeeds
    allfeeds <- runQueryNoT dbConn $ HQ.getUserFeeds uid
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
                items <- runQueryNoT dbConn $ HQ.getUserItems uid (DB.FeedInfoId feedId)
                sendDown conn (HC.FeedItems items)
            HC.ItemRead itemId -> void . runQueryNoT dbConn $ HQ.readFeed uid (DB.FeedItemId itemId)
            HC.FeedRead feedId ->
                void . runQueryNoT dbConn $ HQ.allItemsRead (DB.FeedInfoId feedId) uid
            HC.NewFeed url updateEvery -> do
                newFeed <- runBe conf $ addFeed url updateEvery uid
                case newFeed of
                    Left e -> sendDown conn (HC.BackendError (showUserHeedError e))
                    Right (feed, _) -> do
                        now <- getCurrentTime
                        _ <- startUpdateThread now conf feed
                        sendDown conn (HC.FeedAdded url)
            HC.ForceRefresh fid -> do
                updateE <- runBe conf $ forceUpdate (DB.FeedInfoId fid)
                case updateE of
                    Left e -> sendDown conn (HC.BackendError (showUserHeedError e))
                    Right update -> broadcastUpdate update (conf ^. updateChan)
            HC.InvalidReceived -> putStrLn "Invalid command received"

sendDown
    :: (Serialize a)
    => WS.Connection -> a -> IO ()
sendDown conn info = WS.sendBinaryData conn $ encode info

sendUpdates :: BChan.BroadcastChan BChan.Out (DB.FeedInfoHR, Int64)
            -> WS.Connection
            -> [DB.FeedInfoHR]
            -> IO ()
sendUpdates bchan wsconn userfeeds =
    fork_ . forever $ do
        (feed, numItems) <- BChan.readBChan bchan
        when ((feed ^. DB.feedInfoId) `elem` subIds) $
            sendNewItems wsconn (toFrontEndFeedInfo feed numItems)
  where
    subIds = userfeeds ^.. traverse . DB.feedInfoId

sendNewItems :: WS.Connection -> HC.FeFeedInfo -> IO ()
sendNewItems wsconn finfo = sendDown wsconn (HC.NewItems finfo)

toFrontEndFeedInfo :: DB.FeedInfoHR -> Int64 -> HC.FeFeedInfo
toFrontEndFeedInfo beInfo =
    HC.FeFeedInfo' (beInfo ^. DB.feedInfoId . DB.getFeedInfoId) (beInfo ^. DB.feedInfoName)

backupApp :: Application
backupApp _ respond = respond $ responseLBS badRequest400 [] "Bad request"
