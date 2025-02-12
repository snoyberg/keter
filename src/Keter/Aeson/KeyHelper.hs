{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Utilities for dealing with Aeson version update
module Keter.Aeson.KeyHelper
  ( module KeyMap
  , toKey
  , toText
  ) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap as KeyMap hiding (map)
import Data.Text qualified as Text
import Prelude (id)

toKey :: Text.Text -> Key.Key
toKey = Key.fromText

toText :: Key.Key -> Text.Text
toText = Key.toText
