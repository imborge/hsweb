{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Web where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Database.Beam.Postgres (Connection)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Auth.Server

import Account
import Auth
import Database
import Model
import App

type API = AccountAPI :<|> AuthAPI

apiServer :: ServerT API AppM
apiServer = accountServer :<|> authServer

api :: Proxy API
api = Proxy

nt :: AppConf -> AppM a -> Handler a
nt conf x = runReaderT x conf

app :: AppConf -> Application
app conf = serveWithContext api serverAuthContext $ hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt conf) apiServer
  where
    serverAuthContext = appCookieSettings conf :. appJwtSettings conf :. EmptyContext

server :: IO ()
server = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
  dbConn <- connectPostgreSQL "postgresql://{{cookiecutter.project_slug}}:{{cookiecutter.project_slug}}123@localhost/{{cookiecutter.project_slug}}_dev"
  let conf = AppConf
             { appDb = dbConn
             , appJwtSettings = jwtCfg
             , appCookieSettings = defaultCookieSettings
             , appJwtKey = myKey
             }
  run 8081 $ (app conf)
