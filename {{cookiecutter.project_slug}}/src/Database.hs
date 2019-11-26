{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.Beam
import GHC.Generics (Generic)

import Model

data Db f
  = Db
  { _dbUsers :: f (TableEntity UserT)
  } deriving (Generic, Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings `withDbModification`
     dbModification
     { _dbUsers = modifyTableFields tableModification
                  { _userId = fieldNamed "id"
                  }
     }
