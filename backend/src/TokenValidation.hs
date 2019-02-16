module TokenValidation 
    ( TokenValidationError(..)
    , TokenValidator
    , new
    , validateToken
    ) where

import qualified Data.Aeson as Aeson
import qualified Crypto.JOSE as JOSE
import qualified Crypto.JWT as JWT
import Crypto.JOSE.JWK (JWK)

import qualified Crypto.JOSE.Compact as JOSE.Compact
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LText (Text, fromStrict)

import Control.Monad.Time
import Control.Monad.IO.Class
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT, withExceptT, liftEither)
import Control.Arrow ((>>>))
import Control.Monad (join, void)

import Control.Monad.Catch

-- | Can throw TokenValidationError.
decodeJWK :: MonadThrow m => Text -> m JOSE.JWK
decodeJWK =
    Text.encodeUtf8
    >>> Aeson.eitherDecodeStrict
    >>> either (throwM . InvalidJWK . Text.pack) pure

-- | Can throw TokenValidationError
decodeJWT :: MonadThrow m => Text -> m JWT.SignedJWT
decodeJWT = 
    Text.encodeUtf8
    >>> LByteString.fromStrict
    >>> runExceptT . JOSE.Compact.decodeCompact
    >>> fmap (either (throwM . JWTError) pure)
    >>> join

validateJWT :: (MonadThrow m, MonadTime m) => JOSE.JWK -> JWT.SignedJWT -> m JWT.ClaimsSet
validateJWT jwk jwt = do
    runExceptT (JWT.verifyClaims settings jwk jwt) >>= \case
        Right claims -> pure claims
        Left err -> throwM (JWTError err)

    where settings = JWT.defaultJWTValidationSettings (== "https://api.silverratio.net")

data TokenValidator m =
    TokenValidator { validate :: Text -> m () }

data TokenValidationError = InvalidJWK Text
                          | JWTError JWT.JWTError
    deriving (Eq, Show)

instance Exception TokenValidationError

new :: (MonadThrow m, MonadThrow n, MonadTime n) => Text -> m (TokenValidator n)
new jwkText = do
    jwk <- decodeJWK jwkText
    pure TokenValidator { validate = validate jwk }

    where

    validate :: (MonadThrow m, MonadTime m) => JWK -> Text -> m ()
    validate jwk token = do
        jwtSigned <- decodeJWT token
        void $ validateJWT jwk jwtSigned

validateToken :: (MonadThrow m, MonadTime m) => TokenValidator m -> Text -> m ()
validateToken TokenValidator { .. } = validate