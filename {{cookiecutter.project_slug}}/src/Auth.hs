{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Auth where

import           Control.Monad.Reader (asks)
import           Crypto.KDF.BCrypt (hashPassword, validatePassword)
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time
import           Database.Beam
import           Database.Beam.Postgres
import           GHC.Generics (Generic)
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.Internal.Cookie (applyCookieSettings, applySessionCookieSettings)
import           Servant.Auth.Server.Internal.JWT (makeJWT)
import           Web.Cookie (setCookieValue)

import           App
import           Database
import           Model


type AuthAPI =
  "auth" :>
    ("grant" :> ReqBody '[JSON] GrantRequest :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                       , Header "Set-Cookie" SetCookie ] GrantResponse))

data GrantRequest
  = GrantRequest
  { grantEmail :: Text
  , grantPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON GrantRequest

data GrantResponse
  = GrantResponse
  { grantExpiresAt :: UTCTime
  , grantToken :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GrantResponse

-- TODO: Send auth token in request body
postGrantHandler :: GrantRequest -> AppM (Headers '[ Header "Set-Cookie" SetCookie
                                                   , Header "Set-Cookie" SetCookie ] GrantResponse)
postGrantHandler req = do
  dbConn <- asks appDb
  cookieSettings <- asks appCookieSettings
  jwtSettings <- asks appJwtSettings
  mUser <- liftIO $ runBeamPostgres dbConn
          $ runSelectReturningOne $ select $ filter_ (\user -> _userEmail user ==. val_ (grantEmail req))
          $ all_ (_dbUsers db)
  case mUser of
    Nothing -> throwError err403
    Just user ->
      if validatePassword (encodeUtf8 (grantPassword req)) (encodeUtf8 (_userPassword user))
      then do
        now <- liftIO $ getCurrentTime
        let expiresAt = addUTCTime (nominalDay) now
        mApplyCookies <-
          liftIO $ myAcceptLogin
            (cookieSettings { cookieExpires = Just expiresAt })
            jwtSettings
            (AuthInfo (_userId user) (_userEmail user))
        case mApplyCookies of
          Nothing -> do
            liftIO $ putStrLn "mApplyCookies is nothing :<"
            throwError err401
          Just (jwt, applyCookies) -> do
            return $ applyCookies $ GrantResponse expiresAt jwt
      else throwError err403

authServer :: ServerT AuthAPI AppM
authServer = postGrantHandler

-- Modified version of this:
-- http://hackage.haskell.org/package/servant-auth-server-0.4.4.0/docs/src/Servant.Auth.Server.Internal.Cookie.html#makeSessionCookie
-- to also return the JWT

-- | Makes a cookie with session information.
myMakeSessionCookie :: ToJWT v => CookieSettings -> JWTSettings -> v -> IO (Maybe (Text, SetCookie))
myMakeSessionCookie cookieSettings jwtSettings v = do
  ejwt <- makeJWT v jwtSettings (cookieExpires cookieSettings)
  case ejwt of
    Left _ -> return Nothing
    Right jwt -> return
      $ Just (decodeUtf8 (LBS.toStrict jwt),
             applySessionCookieSettings cookieSettings
                              $ applyCookieSettings cookieSettings
                              $ def{ setCookieValue = LBS.toStrict jwt })

-- Modified version of this:
-- http://hackage.haskell.org/package/servant-auth-server-0.4.4.0/docs/src/Servant.Auth.Server.Internal.Cookie.html#acceptLogin
-- to also return the JWT

-- | For a JWT-serializable session, returns a function that decorates a
-- provided response object with XSRF and session cookies. This should be used
-- when a user successfully authenticates with credentials.
myAcceptLogin :: ( ToJWT session
               , AddHeader "Set-Cookie" SetCookie response withOneCookie
               , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies )
            => CookieSettings
            -> JWTSettings
            -> session
            -> IO (Maybe (Text, (response -> withTwoCookies)))
myAcceptLogin cookieSettings jwtSettings session = do
  mSessionCookie <- myMakeSessionCookie cookieSettings jwtSettings session
  case mSessionCookie of
    Nothing            -> pure Nothing
    Just (jwt, sessionCookie) -> do
      xsrfCookie <- makeXsrfCookie cookieSettings
      return $ Just (jwt, addHeader sessionCookie . addHeader xsrfCookie)
