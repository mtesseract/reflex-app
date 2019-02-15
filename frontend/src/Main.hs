{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module Main where

import           Reflex.Dom
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Common                         ( User )
import           Language.Javascript.JSaddle
-- import GHCJS.Nullable

import GHCJS.DOM.Types (MonadDOM)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.History as History
-- import qualified GHCJS.Types                   as T
import qualified GHCJS.Foreign                 as F
import           GHCJS.DOM.Location             ( getHref )
import           JavaScript.Web.Location
-- import GHCJS.DOM.JSFFI.Generated.Location (getPathname)
import Language.Javascript.JSaddle.String
-- import Language.Javascript.JSaddle.Value

import           Control.Monad.IO.Class
import qualified Data.JSString                 as JSString

import GHCJS.Foreign.Callback (Callback, asyncCallback, asyncCallback1)

-- import GHCJS.DOM.Window (getLocalStorage)
-- import GHCJS.DOM.Storage (setItem, getItem)

import qualified Language.Javascript.JSaddle.Object as JSObj

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

foreign import javascript unsafe "console.log($1)"
  consoleLog :: JSVal -> IO ()

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

extractObjectField :: (MonadIO m, MonadJSM m, PFromJSVal a) => JSVal -> Text -> m (Maybe a)
extractObjectField obj field = do
  v <- liftJSM $ obj JSObj.! field
  pure $ pFromJSVal v

filterEvent :: Reflex t => (a -> Maybe b) -> Event t a -> Event t b
filterEvent f e =
  push (pure . f) e

filterEventMaybe :: Reflex t => Event t (Maybe a) -> Event t a
filterEventMaybe =
  filterEvent id

extractAuth :: (MonadIO m, MonadJSM m) => JSVal -> m (Maybe AppEvent)
extractAuth obj = do
  maybeAccessToken <- extractObjectField obj "accessToken"
  maybeIdToken <- extractObjectField obj "idToken"
  pure $ do
    myAccessToken <- maybeAccessToken
    myIdToken <- maybeIdToken
    let auth = AuthState { idToken = myIdToken, accessToken = myAccessToken }
    pure $ UserSignedIn auth

data AppState = Unauthenticated
              | Authenticated AuthState
  deriving (Eq, Show, Ord)

data AuthState = AuthState
  { idToken :: Text
  , accessToken :: Text
  } deriving (Eq, Show, Ord)

data AppEvent = UserSignedIn AuthState
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

  signInEvent <- performEvent $ (extractAuth <$> signInEvent')
  let signOutEvent = (const UserSignedOut) <$> signOutEvent'
      appEvent = leftmost [filterEventMaybe signInEvent, signOutEvent]
  
  liftIO $ tryRetrieveTokenFromURI

  dynamicAppState <- foldDyn transition Unauthenticated appEvent

  let dynamicAppView = renderState <$> dynamicAppState
  dyn_ dynamicAppView

 where

  renderState appState = do
    case appState of
      Authenticated auth -> authView auth
      Unauthenticated -> nonAuthView

  transition appEvent appState = case appEvent of
    UserSignedIn auth -> Authenticated auth
    UserSignedOut -> Unauthenticated

  authView :: ( PerformEvent t m
              , HasJSContext (Performable m)
              , MonadHold t m
              , TriggerEvent t m
              , PostBuild t m
              , MonadIO m, DomBuilder t m, MonadIO (Performable m))
           => AuthState
           -> m ()
  authView authState = do
    loc :: Location <- liftIO getWindowLocation

    (buttonSignOut, _) <- el' "button" (text "Sign Out")
    performEvent_ ((const (liftIO signOut)) <$> domEvent Click buttonSignOut)
    buttonViewOne <- fmap (const View1) <$> button "View 1"
    buttonViewTwo <- fmap (const View2) <$> button "View 2"
    let changeView = selectView <$> leftmost [buttonViewOne, buttonViewTwo]
    startView <- getStartView loc
    d <- holdDyn (selectView startView) changeView
    dyn_ d
    text ("Token = " <> accessToken authState)

  
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
