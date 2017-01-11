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

import Control.Monad (forever)
import Control.Monad.IO.Class
import Crypto.KDF.BCrypt
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Proxy (Proxy(Proxy))
import Data.Store (Store, decode, encode)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Heed.Commands
import Heed.Crypto
import Heed.Database
import Heed.Extract (addFeed)
import Heed.Query
import Heed.Types
import Heed.Utils (Port)
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
type AuthGenAPI = "auth" :> (ReqBody '[OctetStream] AuthData :> Post '[OctetStream] Token) :<|> AuthProtect "cookie-auth" :> Raw

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | A method that, when given a password, will return a Tok.
-- This is our bespoke (and bad) authentication logic.
lookupTok :: BackendConf -> ByteString -> Handler UserName
lookupTok conf authToken = do
    let dbConn = dbConnection conf
    tokenInfo <- runTransaction dbConn $ verifyToken (decodeUtf8 authToken)
    case tokenInfo of
        Nothing -> throwError err401
        Just user -> return $ UserName (userName user) (getUserId . userId $ user)

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

checkCreds :: BackendConf -> AuthData -> Handler Token
checkCreds conf (AuthData un pw) = do
    liftIO $ putStrLn "Checking auth"
    let dbConn = dbConnection conf
    userDbM <- runQueryNoT dbConn $ getUserDb un
    case userDbM of
        Just userDb -> do
            liftIO $ putStrLn "got pw hash"
            let validPass = validatePassword (encodeUtf8 pw) (encodeUtf8 . userPassword $ userDb)
            if validPass
                then do
                    liftIO $ putStrLn "Verified"
                    newToken <- generateToken
                    _ <- runQueryNoT dbConn $ saveTokenDb newToken (userId userDb)
                    return $ Token newToken
                else (do liftIO $ putStrLn "Invalid password"
                         throwError err401)
        Nothing -> do
            liftIO $ putStrLn "No such user"
            throwError err401

app :: BackendConf -> UserName -> Application
app conf uname = websocketsOr WS.defaultConnectionOptions (wsApp conf uname) backupApp

wsApp :: BackendConf -> UserName -> WS.ServerApp
wsApp conf uname pending_conn = do
    print $ unUserName uname <> " opened a websocket connection"
    -- User heed protocol
    let ar = WS.AcceptRequest (Just "heed") []
        dbConn = dbConnection conf
        uid = UserId $ unUserId uname
    conn <- WS.acceptRequestWith pending_conn ar
    WS.forkPingThread conn 10
    -- As soon as someone connects get the relevant feeds from the db
    -- since we will have to send them once the client tells us it's ready
    feeds <- runQueryNoT dbConn $ getUserFeedInfo uid
    forever $
        do commandM <- decode <$> WS.receiveData conn
           let command = either (const InvalidReceived) id commandM
           putStr "Received: "
           print command
           case command of
               Initialized -> do
                   sendDown conn $ Status (unUserName uname)
                   sendDown conn $ Feeds feeds
               GetFeedItems feedId -> do
                   items <- runQueryNoT dbConn $ getUserItems uid (FeedInfoId feedId)
                   sendDown conn (FeedItems items)
               ItemRead itemId -> do
                   deleted <- runQueryNoT dbConn $ readFeed uid (FeedItemId itemId)
                   putStrLn $ "Deleted " <> show deleted <> " unread items"
               FeedRead feedId -> do
                   deleted <- runQueryNoT dbConn $ allItemsRead (FeedInfoId feedId) uid
                   putStrLn $ "Deleted " <> show deleted <> " unread items"
               NewFeed url updateEvery -> do
                   newFeed <- runBe conf $ addFeed url updateEvery uid
                   case newFeed of
                       Left e -> sendDown conn (BackendError (showUserHeedError e))
                       Right _ -> sendDown conn (FeedAdded url)
               _ -> putStrLn "TODO"

sendDown
    :: (Store a)
    => WS.Connection -> a -> IO ()
sendDown conn info = WS.sendTextData conn $ encode info

backupApp :: Application
backupApp _ respond = respond $ responseLBS badRequest400 [] "Bad request"
