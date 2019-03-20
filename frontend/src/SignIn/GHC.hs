{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

module SignIn.GHC where

import Reflex.Dom
import SignIn.Event
import Control.Monad.IO.Class

initialize :: (DomBuilder t m, MonadIO m) => m ()
initialize = undefined

embed :: (TriggerEvent t m, DomBuilder t m) => m (Event t AuthEvent)
embed = undefined
