{-# LANGUAGE DeriveGeneric #-}

module Common (User(..)) where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User { userName :: Text } deriving (Generic, Eq, Show)

instance FromJSON User
instance ToJSON User

