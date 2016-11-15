{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (ToJSON, decode, encode)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Heed.Commands
import Heed.Crypto
import Heed.Database
import Heed.Query
import Heed.Types
import Network.Wai (Application, Request, requestHeaders)
import Network.Wai.Application.Static
       (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant
       (NoContent, ServantErr(..), err303, throwError, throwError)
import Servant.API
       ((:<|>)((:<|>)), (:>), FormUrlEncoded, JSON, Post, ReqBody)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Raw (Raw)
import Servant.Server
       (Context((:.), EmptyContext), Handler, Server, err401, errBody,
        serveWithContext)
import Servant.Server.Experimental.Auth
       (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Utils.StaticFiles (serveDirectory)
import Web.Cookie
import Web.FormUrlEncoded

data AuthData = AuthData
    { username :: Text
    , password :: Text
    } deriving (Generic)

instance FromForm AuthData

-- | run our server
genAuthMain :: BackendConf -> IO ()
genAuthMain conf =
    run 8080 (serveWithContext genAuthAPI (genAuthServerContext conf) (genAuthServer conf))

data UserName = UserName
    { unUserName :: Text
    , unUserId :: Int
    }

-- | A Token we generate if username and password are correct
newtype Token = Token
    { unToken :: Text
    }

-- | Our API, with auth-protection
type AuthGenAPI = "login" :> (ReqBody '[FormUrlEncoded] AuthData :> Post '[JSON] NoContent :<|> Raw) :<|> AuthProtect "cookie-auth" :> Raw

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | A method that, when given a password, will return a Tok.
-- This is our bespoke (and bad) authentication logic.
lookupTok :: BackendConf -> ByteString -> Handler UserName
lookupTok conf cookies =
    case Map.lookup authCookieName $ Map.fromList (parseCookies cookies) of
        Nothing -> throwError err303InvalidAuth
        Just cookieToken -> do
            let dbConn = dbConnection conf
            tokenInfo <- verifyToken dbConn (decodeUtf8 cookieToken)
            case tokenInfo of
                Nothing -> throwError err303ToLogin
                (Just user) -> return $ UserName (userName user) (getUserId . userId $ user)

-- | The auth handler wraps a function from Request -> Handler Tok
-- we look for a Cookie and pass the value of the cookie to `lookupTok`.
authHandler :: BackendConf -> AuthHandler Request UserName
authHandler conf =
    let handler req =
            case lookup "Cookie" (requestHeaders req) of
                Nothing -> throwError err303ToLogin
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
genAuthServer conf =
    let loginPage = serveDirectory "./static/login/"
    in (checkCreds conf :<|> loginPage) :<|> app conf

checkCreds :: BackendConf -> AuthData -> Handler NoContent
checkCreds conf (AuthData un pw) = do
    liftIO $ putStrLn "Checking auth"
    let dbConn = dbConnection conf
    userDbM <- getUserDb dbConn un
    case userDbM of
        Just userDb -> do
            liftIO $ putStrLn "got pw hash"
            let validPass = validatePassword (encodeUtf8 pw) (encodeUtf8 . userPassword $ userDb)
            if validPass
                then do
                    liftIO $ putStrLn "Verified"
                    newToken <- generateToken
                    _ <- saveTokenDb dbConn newToken (userId userDb)
                    throwError $ err303ValidAuth (Token newToken)
                else (do liftIO $ putStrLn "Invalid password"
                         throwError err401)
        Nothing -> do
            liftIO $ putStrLn "No such user"
            throwError err401

authCookieName :: ByteString
authCookieName = "auth-token"

authCookie :: Token -> SetCookie
authCookie token =
    def
    { setCookieName = authCookieName
    , setCookieValue = encodeUtf8 . unToken $ token
    }

err303ToLogin :: ServantErr
err303ToLogin =
    err303
    { errBody = "Missing cookie header, redirecting to login"
    , errHeaders = [("Location", "/login/index.html")]
    }

err303InvalidAuth :: ServantErr
err303InvalidAuth =
    err303
    { errBody = "Missing cookie auth-token key, redirecting to login"
    , errHeaders = [("Location", "/login/index.html")]
    }

err303ValidAuth :: Token -> ServantErr
err303ValidAuth token =
    err303
    { errHeaders =
        [ ("Location", "/")
        , ("Set-Cookie", toStrict . toLazyByteString $ renderSetCookie (authCookie token))
        ]
    }

app :: BackendConf -> UserName -> Application
app conf uname = websocketsOr WS.defaultConnectionOptions (wsApp conf uname) backupApp

wsApp :: BackendConf -> UserName -> WS.ServerApp
wsApp conf uname pending_conn = do
    print $ unUserName uname <> " opened a websocket connection"
    -- User heed protocol
    let ar = WS.AcceptRequest (Just "heed")
        dbConn = dbConnection conf
        uid = UserId $ unUserId uname
    conn <- WS.acceptRequestWith pending_conn ar
    WS.forkPingThread conn 10
    -- As soon as someone connects get the relevant feeds from the db
    -- since we will have to send them once the client tells us it's ready
    feeds <- getUserFeedInfo dbConn uid
    forever $
        do commandM <- decode <$> WS.receiveData conn
           let command = fromMaybe InvalidReceived commandM
           putStr "Received: "
           print command
           case command of
               Initialized -> sendDown conn $ Feeds feeds
               GetFeedItems feedId -> do
                   items <- getUserItems dbConn uid (FeedInfoId feedId)
                   sendDown conn (FeedItems items)
               _ -> do
                   print command
                   putStrLn "TODO"

sendDown
    :: (ToJSON a)
    => WS.Connection -> a -> IO ()
sendDown conn info = WS.sendTextData conn $ encode info

backupApp :: Application
backupApp = staticApp $ defaultFileServerSettings "./heed-frontend/output/"
