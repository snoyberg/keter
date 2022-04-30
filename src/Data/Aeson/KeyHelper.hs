{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Utilities for dealing with Aeson version update

module Data.Aeson.KeyHelper
  ( module KeyMap
  , toKey
  , toText
  ) where

import Prelude (id)
import qualified Data.Text                   as Text

#if MIN_VERSION_aeson (2,0,0)
import qualified Data.Aeson.Key              as Key
import Data.Aeson.KeyMap                     as KeyMap hiding (map)

toKey :: Text.Text -> Key.Key
toKey = Key.fromText

toText :: Key.Key -> Text.Text
toText = Key.toText

#else
import Data.HashMap.Strict                   as KeyMap hiding (map)

toKey :: Text.Text -> Text.Text
toKey = id

toText :: Text.Text -> Text.Text
toText = id

#endif
