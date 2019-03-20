{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

module SignIn.GHCJS where

-- import GHCJS.Foreign
-- import Data.JSString (JSString, pack)
-- import Data.Text (Text, unpack)

import Reflex.Dom
import SignIn.Event
import Control.Monad.IO.Class
import GHCJS.Foreign.Callback
import Language.Javascript.JSaddle
import Control.Monad
import Data.Bool (bool)

foreign import javascript unsafe "gapi.load('auth2', function() { console.log('Google Sign-In API initialized'); })"
  gapiLoadAuth2 :: IO ()

foreign import javascript unsafe "gapi.auth2.init({'client_id': '801316153281-lfji5gub4udj5732m394vp4mlne2144q.apps.googleusercontent.com'})"
  gapiAuth2Init :: IO ()

foreign import javascript unsafe "GoogleAuth.isSignedIn.listen($1)"
  gapiAuth2RegisterListener :: Callback (JSVal -> IO ()) -> IO ()

initialize :: (DomBuilder t m, MonadIO m) => m ()
initialize = do
  elAttr "script" ("src" =: "https://apis.google.com/js/platform.js") (pure ()) -- <> "async" =: "" <> "defer" =: "") (pure ())

embed :: (TriggerEvent t m, DomBuilder t m, MonadJSM m, MonadIO m) => m (Event t AuthEvent)
embed = do
  (ev, fn) <- newTriggerEvent
  cb <- liftIO $ asyncCallback1 (convertBool >=> fn)
  liftIO gapiLoadAuth2
  liftIO gapiAuth2Init
  liftIO $ gapiAuth2RegisterListener cb
  elAttr "div" ("class" =: "g-signin2") (pure ())
  pure ev

  where convertBool :: MonadJSM m => JSVal -> m AuthEvent
        convertBool val = do
          b <- liftJSM (valToBool val)
          pure (bool SignedOut SignedIn b)
  
