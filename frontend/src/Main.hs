{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.Default
import qualified Data.Text as Text
import Common (User)

main :: IO ()
main = mainWidget $ el "div" $ do
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
