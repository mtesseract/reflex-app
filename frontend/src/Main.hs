{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module Main where

import           Reflex.Dom
import           Data.Default
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Common                         ( User )
import           Language.Javascript.JSaddle
import           Reflex.Dom.Widget.Basic
import GHCJS.Nullable

import GHCJS.DOM.Types (MonadDOM)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.History as History
import qualified GHCJS.Types                   as T
import qualified GHCJS.Foreign                 as F
import           GHCJS.DOM.Location             ( getHref )
import           JavaScript.Web.Location
-- import GHCJS.DOM.JSFFI.Generated.Location (getPathname)
import Language.Javascript.JSaddle.String
import Language.Javascript.JSaddle.Value

import           Control.Monad.IO.Class
import qualified Data.JSString                 as JSString

import GHCJS.Foreign.Callback (Callback, asyncCallback, asyncCallback1)
import GHCJS.DOM.Text (unText)
import Control.Monad (join)

foreign import javascript unsafe "appAuth.webAuth.authorize()"
  webAuthAuthorize :: IO ()

foreign import javascript unsafe "appAuth.signOut()"
  signOut :: IO ()

foreign import javascript unsafe "appAuth.tryRetrieveTokenFromURI()"
  tryRetrieveTokenFromURI :: IO ()

foreign import javascript unsafe "appAuth.registerSignInCallback($1)"
  registerSignInCallback :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "appAuth.registerSignOutCallback($1)"
  registerSignOutCallback :: Callback (IO ()) -> IO ()

data View = View1 | View2 deriving (Eq, Ord, Show)

getStartView :: MonadIO m => Location -> m View
getStartView location = do
  path :: Text <- textFromJSString <$> liftIO (getPathname location)
  pure (pathToView path)
  
pathToView = \case
  "/view1" -> View1
  "/view2" -> View2
  _ -> View1

viewToPath :: View -> Text
viewToPath = \case
  View1 -> "/view1"
  View2 -> "/view2"

convertToText :: JSVal -> Text
convertToText = pFromJSVal
-- convertToText :: MonadIO m => JSVal -> m Text
-- convertToText jsval = liftIO $
--   fromJSVal jsval >>= \case
--     Just a -> pure a
--     Nothing -> pure "failed to convert jsval to text"

data AppState = AppState
  {
    signedIn :: Bool
  }

data AppEvent = UserSignedIn
              | UserSignedOut
    deriving (Eq, Show, Ord)

main :: IO ()
main = mainWidget $ el "div" $ do
  (signInEvent', triggerSignInEvent :: JSVal -> IO ()) <- newTriggerEvent
  (signOutEvent', triggerSignOutEvent :: () -> IO ()) <- newTriggerEvent

  signInCallback <- liftIO $ asyncCallback1 triggerSignInEvent
  signOutCallback <- liftIO $ asyncCallback (triggerSignOutEvent ())
  liftIO $ registerSignInCallback signInCallback
  liftIO $ registerSignOutCallback signOutCallback

  let signInEvent = (const UserSignedIn) <$> signInEvent'
  let signOutEvent = (const UserSignedOut) <$> signOutEvent'
      appEvent = leftmost [signInEvent, signOutEvent]
  
  liftIO $ tryRetrieveTokenFromURI

  dynamicAppState <- foldDyn transition (AppState False) appEvent

  let dynamicAppView = renderState <$> dynamicAppState
  dyn_ dynamicAppView

  -- let a = pToJSVal ("not authenticated" :: Text)
  -- -- authDyn <- holdDyn a (signInEvent
  -- let authDyn' = convertToText <$> authDyn
  -- display authDyn'


  -- nonAuthView


 where

  renderState appState = do
    case signedIn appState of
      True -> authView
      False -> nonAuthView

  transition appEvent appState = case appEvent of
    UserSignedIn -> appState { signedIn = True }
    UserSignedOut -> appState { signedIn = False }

  authView :: ( PerformEvent t m
              , HasJSContext (Performable m)
              , MonadHold t m
              , TriggerEvent t m
              , PostBuild t m
              , MonadIO m, DomBuilder t m, MonadIO (Performable m)) => m ()
  authView = do
    loc :: Location <- liftIO getWindowLocation

    (buttonSignOut, _) <- elAttr' "button" ("id" =: "btn-login" 
                                            <> "class" =: "btn btn-primary btn-margin")
                                           (text "Sign Out")
    performEvent_ ((const (liftIO signOut)) <$> domEvent Click buttonSignOut)
    buttonViewOne <- fmap (const View1) <$> button "View 1"
    buttonViewTwo <- fmap (const View2) <$> button "View 2"
    let changeView = selectView <$> leftmost [buttonViewOne, buttonViewTwo]
    startView <- getStartView loc
    d <- holdDyn (selectView startView) changeView
    dyn_ d

  
  nonAuthView :: ( PerformEvent t m
                 , MonadIO m, DomBuilder t m, MonadIO (Performable m)) => m ()
  nonAuthView  = do
    (buttonSignIn, _) <- elAttr' "button" ("id" =: "btn-login" 
                                           <> "class" =: "btn btn-primary btn-margin")
                                          (text "Sign In")
    performEvent_ ((const (liftIO webAuthAuthorize)) <$> domEvent Click buttonSignIn)

  pushToHistory :: (MonadDOM m, MonadIO m) => View -> m ()
  pushToHistory view = do
    let path = viewToPath view
    history <- Window.getHistory =<< DOM.currentWindowUnchecked
    History.pushState history JSNull path (Just path)

  selectView
    :: ( DomBuilder t m
       , MonadJSM (Performable m)
       , HasJSContext (Performable m)
       , PerformEvent t m
       , TriggerEvent t m
       , MonadHold t m
       , PostBuild t m
       , MonadIO m
       , PerformEvent t m
       )
    => View
    -> m ()
  selectView view = do
    pushToHistory view
    case view of
      View1 -> view1
      View2 -> view2
   where

    view1 = do
      text "view 1"

    view2 = do
      buttonRetrieveUsers <- button "Retrieve users from DB"
      let apiRequestRetrieveUsers = XhrRequest "GET" urlRetrieveUsers def
          eventRetrieveUsersRequest =
            const apiRequestRetrieveUsers <$> buttonRetrieveUsers
      eventRetrieveUsersResponse <- performRequestAsync
        eventRetrieveUsersRequest
      let eventUsersRetrieved =
            (Text.pack . show . decodeUsers) <$> eventRetrieveUsersResponse
      dynUsersRetrieved <- holdDyn "None" eventUsersRetrieved
      dynText dynUsersRetrieved
     where
      decodeUsers      = decodeXhrResponse :: XhrResponse -> Maybe [User]
      urlRetrieveUsers = "https://api.silverratio.net/users"
