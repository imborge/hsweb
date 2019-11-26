{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Web where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson hiding (json)
import           Data.Text (Text)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           GHC.Generics (Generic)
import           Network.Wai.Handler.Warp hiding (FileInfo)
import           Network.Wai.Middleware.Cors
import           Servant

import           Database
import           Model

type API = "users" :> ReqBody '[JSON] PostUserRequest :> Post '[JSON] PostUserResponse

data PostUserRequest = PostUserRequest Text deriving (Eq, Generic, Show)
data PostUserResponse = PostUserResponse Text deriving (Eq, Generic, Show)

instance FromJSON PostUserRequest
instance ToJSON PostUserResponse

insertUser :: Connection -> Text -> IO ()
insertUser dbConn username = do
  runBeamPostgres dbConn $ runInsert $ insert (_dbUsers db) $ insertExpressions [ User default_ (val_ username) ]

postUserHandler :: Connection -> PostUserRequest -> Handler PostUserResponse
postUserHandler dbConn (PostUserRequest username) = do
  liftIO $ insertUser dbConn username
  return $ PostUserResponse username

server1 :: Connection -> Server API
server1 dbConn = postUserHandler dbConn

api :: Proxy API
api = Proxy

app :: Connection -> Application
app conn = simpleCors $ serve api (server1 conn)

server :: IO ()
server = do
  dbConn <- connectPostgreSQL "postgresql://{{cookiecutter.project_slug}}:{{cookiecutter.project_slug}}123@localhost/{{cookiecutter.project_slug}}_dev"
  run 8081 (app dbConn)
