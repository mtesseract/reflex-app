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

foreign import javascript unsafe "webAuth.authorize()"
  webAuthAuthorize :: IO ()

data View = View1 | View2 deriving (Eq, Ord, Show)

getStartView :: MonadIO m => Location -> m View
getStartView location = do
  path :: Text <- textFromJSString <$> liftIO (getPathname location)
  pure (pathToView path)
  
  where pathToView = \case
          "/view1" -> View1
          "/view2" -> View2
          _ -> View1

viewToPath :: View -> Text
viewToPath = \case
  View1 -> "/view1"
  View2 -> "/view2"

main :: IO ()
main = mainWidget $ el "div" $ do
  loc :: Location <- liftIO getWindowLocation
  startView <- getStartView loc
  buttonViewOne <- fmap (const View1) <$> button "View 1"
  buttonViewTwo <- fmap (const View2) <$> button "View 2"
  let changeView = selectView <$> leftmost [buttonViewOne, buttonViewTwo]
  d <- holdDyn (selectView startView) changeView
  dyn_ d
  pure ()

 where

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
       )
    => View
    -> m ()
  selectView view = do
    pushToHistory view
    case view of
      View1 -> view1
      View2 -> view2
   where

    view1 = text "Hello"

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
