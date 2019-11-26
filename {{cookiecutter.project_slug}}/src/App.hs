module App where

import Control.Monad.Reader (ReaderT)
import Crypto.JOSE.JWK (JWK)
import Database.Beam.Postgres (Connection)
import Servant (Handler)
import Servant.Auth.Server (JWTSettings, CookieSettings)

type AppM = ReaderT AppConf Handler

data AppConf
  = AppConf
  { appDb :: Connection
  , appJwtSettings :: JWTSettings
  , appCookieSettings :: CookieSettings
  , appJwtKey :: JWK
  }
