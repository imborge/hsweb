{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Account
  ( AccountAPI
  , accountServer
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.Reader (asks, when)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.Aeson
import Data.Aeson.Casing
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server

import App
import Model
import Database

type AccountAPI = "account" :>
                    (ReqBody '[JSON] PostAccountRequest :> Post '[JSON] NoContent
                    :<|> Auth '[JWT, Cookie] AuthInfo :> "setPassword" :> ReqBody '[JSON] PostSetPasswordRequest :> Post '[JSON] PostSetPasswordResponse)

accountServer :: ServerT AccountAPI AppM
accountServer = postAccountHandler :<|> postSetPasswordHandler

data PostAccountRequest
  = PostAccountRequest
  { accountEmail :: Text
  , accountPassword :: Text
  } deriving (Eq, Generic, Show)
data PostAccountResponse = PostAccountResponse deriving (Eq, Generic, Show)

instance FromJSON PostAccountRequest where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
  
instance ToJSON PostAccountResponse where
  toJSON = genericToJSON $ aesonPrefix camelCase

data PostSetPasswordRequest
  = PostSetPasswordRequest
  { oldPassword :: Text
  , newPassword :: Text
  } deriving (Eq, Generic, Show)
data PostSetPasswordResponse = PostSetPasswordResponse deriving (Eq, Generic, Show)

instance FromJSON PostSetPasswordRequest
instance ToJSON PostSetPasswordResponse

postAccountHandler :: PostAccountRequest -> AppM NoContent
postAccountHandler req = do
  dbConn <- asks appDb
  liftIO $ insertUser dbConn (accountEmail req) (accountPassword req)
  return NoContent

postSetPasswordHandler :: AuthResult AuthInfo -> PostSetPasswordRequest -> AppM PostSetPasswordResponse
postSetPasswordHandler (Authenticated authInfo) req = do
  dbConn <- asks appDb
  mUser <- liftIO $ runBeamPostgres dbConn $ runSelectReturningOne
          $ select $ filter_ (\u -> _userId u ==. val_ (authUserId authInfo))
          $ all_ (_dbUsers db)
  case mUser of
    Nothing -> throwError err403
    Just user -> do
      isValidatedPassword <- return $ validatePassword (encodeUtf8 (oldPassword req)) (encodeUtf8 (_userPassword  user))
      if (not isValidatedPassword) then throwError err403 else return ()

      newPass <- liftIO $ hashPassword 12 (encodeUtf8 (newPassword req))
      liftIO $ runBeamPostgres dbConn
        $ runUpdate
        $ update (_dbUsers db)
          (\u -> _userPassword u <-. val_ (decodeUtf8 newPass))
          (\u -> _userId u ==. val_ (authUserId authInfo))
      return PostSetPasswordResponse
postSetPasswordHandler _ _ = throwError err401

insertUser :: Connection -> Text -> Text -> IO ()
insertUser dbConn mail pass = do
  pass' <- liftIO $ hashPassword 12 (encodeUtf8 pass)
  runBeamPostgres dbConn $ runInsert $ insert (_dbUsers db) $ insertExpressions [ User default_ (val_ mail) (val_ (decodeUtf8 pass')) ]
