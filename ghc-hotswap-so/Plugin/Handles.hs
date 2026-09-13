{-# LANGUAGE OverloadedStrings #-}

module Plugin.Handles
  ( hsNewSOHandle
  ) where

import           Foreign
import           Keter.GHC.Types
import qualified Plugin.Body

foreign export ccall "hs_soHandles"
  hsNewSOHandle :: IO (StablePtr SOHandles)

hsNewSOHandle :: IO (StablePtr SOHandles)
hsNewSOHandle = newStablePtr SOHandles
  { someData = "I live in a shared object"
  , initialize = Plugin.Body.initialize
  , middleware = Plugin.Body.middleware
  }
