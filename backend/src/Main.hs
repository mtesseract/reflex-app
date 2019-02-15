{-# LANGUAGE DeriveGeneric #-}
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

import Servant.Auth.Server
import Web.JWT

import Common

data Config = Config { dbHost :: Text, dbUsername :: Text, dbDatabase :: Text, dbPassword :: Text }
    deriving (Generic, Show)

instance FromEnv Config where
    fromEnv = Config <$> env "DB_HOST" <*> env "DB_USERNAME" <*> env "DB_DATABASE" <*> env "DB_PASSWORD"

data AppContext = AppContext { dbConnection :: PG.Connection }

type API = Auth [JWTAuth] () :>  "users" :> Get '[JSON] [User]

users :: [User]
users = [User "foo"]

usersGet :: AuthResult () -> ReaderT AppContext Handler [User]
usersGet = do
    AppContext { .. } <- ask
    userNames <- liftIO $ PG.query_ dbConnection "SELECT NAME FROM users"
    pure $ map (User . PG.fromOnly) userNames

runAppHandler :: AppContext -> ReaderT AppContext m a -> m a
runAppHandler = flip runReaderT

server :: AppContext -> Server API
server ctx = hoistServer serverAPI (runAppHandler ctx) server'

server' :: ServerT API (ReaderT AppContext Handler)
server' = usersGet

serverAPI :: Proxy API
serverAPI = Proxy

app :: AppContext -> Application
app ctx = serve serverAPI (server ctx)

main :: IO ()
main = do
    config <- decodeEnv >>= \case
                Right conf -> pure conf
                Left err -> error err
    dbConn <- dbConnect config
    let ctx = AppContext { dbConnection = dbConn }
    run 8080 (app ctx)

dbConnect Config {..} =
    PG.connect PG.ConnectInfo { PG.connectHost = dbHost ^. unpacked
                   , PG.connectPort = 5432
                   , PG.connectUser = dbUsername ^. unpacked
                   , PG.connectPassword = dbPassword ^. unpacked
                   , PG.connectDatabase = dbDatabase ^. unpacked }