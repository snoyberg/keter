{-# LANGUAGE TypeFamilies #-}
module Keter.Types.Common where

import           Data.Aeson      (Object)
import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Keter.Prelude   (KIO)

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

data Plugin = Plugin
    { pluginGetEnv :: Appname -> Object -> IO [(Text, Text)]
    }

type Plugins = [Plugin]

-- | Used for versioning data types.
class ToCurrent a where
    type Previous a
    toCurrent :: Previous a -> a
instance ToCurrent a => ToCurrent (Maybe a) where
    type Previous (Maybe a) = Maybe (Previous a)
    toCurrent = fmap toCurrent

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = Text

type HostBS = ByteString
