{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model where

import Data.Aeson 
import Data.Aeson.Casing
import Data.Text (Text)
import Database.Beam
import Servant.Auth.Server

data UserT f
  = User
  { _userId :: Columnar f Int
  , _userEmail :: Columnar f Text
  , _userPassword :: Columnar f Text
  } deriving Generic

instance Beamable UserT

instance ToJSON User where
  toJSON (User userId email _) =
    object ["id" .= userId, "email" .= email]

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

-- AuthInfo does NOT belong database-stuff, its simply a model for the JWT payload
data AuthInfo
  = AuthInfo
  { authUserId :: Int
  , authUserEmail :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
  
instance ToJSON AuthInfo where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJWT AuthInfo
instance FromJWT AuthInfo
