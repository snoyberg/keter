module Keter.Types where

import Data.Text (Text)
import Data.Aeson (Object)
import Keter.Prelude (KIO)

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

data Plugin = Plugin
    { pluginGetEnv :: Appname -> Object -> KIO [(Text, Text)]
    }

type Plugins = [Plugin]
