{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where

import Database.Beam
import Data.Text (Text)
import Data.Aeson (ToJSON)

data UserT f
  = User
  { _userId :: Columnar f Int
  , _userEmail :: Columnar f Text
  } deriving Generic

instance Beamable UserT

instance ToJSON User

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId
