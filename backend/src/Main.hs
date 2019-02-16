{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import System.Envy
import GHC.Generics
import qualified Data.Text
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PG
import Data.Text.Lens
import Control.Lens
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Crypto.JOSE.JWK (JWK)

import qualified Data.ByteString.Lazy as LByteString

import Control.Arrow ((>>>))
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LText (Text, fromStrict)

import Servant.Auth.Server (AuthResult(..), AuthResult(..), Auth, JWT, ToJWT, FromJWT, JWTSettings, CookieSettings, defaultJWTSettings, defaultCookieSettings)

import Common

import TokenValidation
-- import qualified TokenValidation
-- import TokenValidation (TokenValidator)

data EnvConfig = EnvConfig
    { envConfigDbHost :: Text
    , envConfigDbUsername :: Text
    , envConfigDbDatabase :: Text
    , envConfigDbPassword :: Text
    , envConfigJwk :: Text
    } deriving (Generic, Show)

instance FromEnv EnvConfig where
    fromEnv = EnvConfig
              <$> env "DB_HOST"
              <*> env "DB_USERNAME"
              <*> env "DB_DATABASE"
              <*> env "DB_PASSWORD"
              <*> env "JWK"

data AppContext =
    AppContext
    { dbConnection :: PG.Connection
    -- , tokenValidator :: TokenValidator IO
    }

type API = Auth '[JWT] Client :> "users" :> Get '[JSON] [User]

usersGet :: Client -> ReaderT AppContext Handler [User]
usersGet client = do
    AppContext { .. } <- ask
    userNames <- liftIO $ PG.query_ dbConnection "SELECT NAME FROM users"
    pure $ map (User . PG.fromOnly) userNames

runAppHandler :: AppContext -> ReaderT AppContext m a -> m a
runAppHandler = flip runReaderT

server :: AppContext -> Server API
server ctx = hoistServerWithContext serverAPI (Proxy :: Proxy '[JWTSettings, CookieSettings]) (runAppHandler ctx) server'

-- servantCtx :: Proxy
-- servantCtx = Proxy

server' :: ServerT API (ReaderT AppContext Handler)
server' (Authenticated client) = usersGet client
server' _ = throwError err401

serverAPI :: Proxy API
serverAPI = Proxy

app :: AppContext -> JWK -> Application
app ctx jwk = serveWithContext serverAPI serverCtx (server ctx)
    where serverCtx =
            let jwtCfg = defaultJWTSettings jwk
            in defaultCookieSettings :. jwtCfg :. EmptyContext

main :: IO ()
main = do
    config <- decodeEnv >>= \case
                Right conf -> pure conf
                Left err -> error err
    dbConn <- dbConnect config
    jwk <- decodeJWK (envConfigJwk config)
    -- (tokenValidator, jwk) <- TokenValidation.newWithJwk (envConfigJwk config)

    let ctx = AppContext { dbConnection = dbConn }
                        --  , tokenValidator = tokenValidator }
    run 8080 (app ctx jwk)

dbConnect EnvConfig {..} =
    PG.connect PG.ConnectInfo { PG.connectHost = envConfigDbHost ^. unpacked
                   , PG.connectPort = 5432
                   , PG.connectUser = envConfigDbUsername ^. unpacked
                   , PG.connectPassword = envConfigDbPassword ^. unpacked
                   , PG.connectDatabase = envConfigDbDatabase ^. unpacked }
