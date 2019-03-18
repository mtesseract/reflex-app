{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import System.Envy
import GHC.Generics
import qualified Data.Text
import Data.Text (Text)
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
import TokenValidation

import Common

data EnvConfig = EnvConfig
    { envConfigPort :: Int
    , envConfigJwk :: Text
    } deriving (Generic, Show)

instance FromEnv EnvConfig where
    fromEnv = EnvConfig
              <$> env "PORT"
              <*> env "JWK"

data AppContext =
    AppContext
    { appPort :: Int }

type API = Auth '[JWT] Client :> PrivateAPI
            :<|> PublicAPI

type PublicAPI = "hello" :> Get '[JSON] Text
type PrivateAPI = "users" :> Get '[JSON] [User]

helloGet :: ReaderT AppContext Handler Text
helloGet = pure "Shalom"

usersGet :: Client -> ReaderT AppContext Handler [User]
usersGet _client = do
    pure []

privateServer :: AuthResult Client -> ServerT PrivateAPI (ReaderT AppContext Handler)
privateServer (Authenticated client) = usersGet client
privateServer _ = throwError err401

publicServer :: ServerT PublicAPI (ReaderT AppContext Handler)
publicServer = helloGet

runAppHandler :: AppContext -> ReaderT AppContext m a -> m a
runAppHandler = flip runReaderT

server :: AppContext -> Server API
server ctx = hoistServerWithContext serverAPI (Proxy :: Proxy '[JWTSettings, CookieSettings]) (runAppHandler ctx) server'

server' :: ServerT API (ReaderT AppContext Handler)
server' = privateServer :<|> publicServer

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
    jwk <- decodeJWK (envConfigJwk config)
    let ctx = AppContext { appPort = envConfigPort config }
    putStrLn $ "Starting frontend-server on port " <> show (appPort ctx)
    run (appPort ctx) (app ctx jwk)
