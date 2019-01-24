{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.Default
import qualified Data.Text as Text
import Common (User)
import Language.Javascript.JSaddle

data View = View1 | View2 deriving (Eq, Ord, Show)

main :: IO ()
main = mainWidget $ el "div" $ do
  buttonViewOne <- fmap (const View1) <$> button "View 1"
  buttonViewTwo <- fmap (const View2) <$> button "View 2"
  let changeView = selectView <$> leftmost [buttonViewOne, buttonViewTwo]
  d <- holdDyn (selectView View2) changeView
  dyn_ d
  pure ()

  where
    selectView :: (DomBuilder t m, MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, MonadHold t m, PostBuild t m) => View -> m ()
    selectView = \case
      View1 -> view1
      View2 -> view2
      where

        view1 =
          text "Hello"

        view2 = do
          buttonRetrieveUsers <- button "Retrieve users from DB"
          let apiRequestRetrieveUsers = XhrRequest "GET" urlRetrieveUsers def
              eventRetrieveUsersRequest = const apiRequestRetrieveUsers <$> buttonRetrieveUsers
          eventRetrieveUsersResponse <- performRequestAsync eventRetrieveUsersRequest
          let eventUsersRetrieved = (Text.pack . show . decodeUsers) <$> eventRetrieveUsersResponse
          dynUsersRetrieved <- holdDyn "None" eventUsersRetrieved
          dynText dynUsersRetrieved
          where
            decodeUsers = decodeXhrResponse :: XhrResponse -> Maybe [User]
            urlRetrieveUsers = "https://api.silverratio.net/users"
