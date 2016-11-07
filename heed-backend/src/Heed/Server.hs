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
import Data.Aeson (decode, encode)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.Wai (Application, Request, requestHeaders)
import Network.Wai.Application.Static
       (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
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

import Crypto.KDF.BCrypt
import Heed.Commands
import Heed.Crypto
import Heed.Database
import Heed.Query
import Heed.Types
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
       (ServerApp, acceptRequest, defaultConnectionOptions,
        forkPingThread, receiveData, sendBinaryData)

data AuthData = AuthData
    { username :: Text
    , password :: Text
    } deriving (Generic)

instance FromForm AuthData

-- | run our server
genAuthMain :: BackendConf -> IO ()
genAuthMain conf =
    run 8080 (serveWithContext genAuthAPI (genAuthServerContext conf) (genAuthServer conf))

-- | A Token we generate if username and password are correct
newtype UserName = UserName
    { unUserName :: Text
    }

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
            case length tokenInfo of
                0 -> throwError err303ToLogin
                _ -> do
                    liftIO . print $ "username: " <> (userName . head $ tokenInfo)
                    return $ UserName (userName . head $ tokenInfo)

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
        rssApp _ = app
    in (checkCreds conf :<|> loginPage) :<|> rssApp

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

app :: Application
app = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        putStrLn "new connection"
        conn <- acceptRequest pending_conn
        forkPingThread conn 10
        forever $
            do asd <- receiveData conn
               let qwe :: Maybe Up = decode asd
               putStrLn "received"
               print qwe
               putStrLn "sending"
               sendBinaryData conn (encode Testing)
    backupApp :: Application
    backupApp = staticApp $ defaultFileServerSettings "./heed-frontend/output/"
