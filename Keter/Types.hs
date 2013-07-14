{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Keter.Types where

import Data.Text (Text)
import Data.Aeson (Object)
import Keter.Prelude (KIO)
import qualified Keter.Types.V04 as V04
import Data.Yaml.FilePath
import Data.Aeson (FromJSON (..), (.:), (.:?), Value (Object), withObject, (.!=))
import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Filesystem.Path as F

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

data Plugin = Plugin
    { pluginGetEnv :: Appname -> Object -> KIO [(Text, Text)]
    }

type Plugins = [Plugin]

-- | Used for versioning data types.
class ToCurrent a where
    type Current a
    toCurrent :: a -> Current a
instance ToCurrent a => ToCurrent (Maybe a) where
    type Current (Maybe a) = Maybe (Current a)
    toCurrent = fmap toCurrent

-- Bundle configuration
data AppConfig = AppConfig
    { aconfigExec :: F.FilePath
    , aconfigArgs :: [Text]
    , aconfigHost :: Text
    , aconfigSsl :: Bool
    , aconfigExtraHosts :: Set Text
    , aconfigRaw :: Object
    }

instance ToCurrent V04.AppConfig where
    type Current V04.AppConfig = AppConfig
    toCurrent (V04.AppConfig a b c d e f) = AppConfig a b c d e f

instance ParseYamlFile AppConfig where
    parseYamlFile basedir = withObject "AppConfig" $ \o -> AppConfig
        <$> lookupBase basedir o "exec"
        <*> o .:? "args" .!= []
        <*> o .: "host"
        <*> o .:? "ssl" .!= False
        <*> o .:? "extra-hosts" .!= Set.empty
        <*> return o

data BundleConfig = BundleConfig
    { bconfigApp :: Maybe AppConfig
    , bconfigStaticHosts :: Set StaticHost
    , bconfigRedirects :: Set Redirect
    }

instance ToCurrent V04.Config where
    type Current V04.Config = BundleConfig
    toCurrent (V04.Config a b c) = BundleConfig (fmap toCurrent a) (Set.map toCurrent b) (Set.map toCurrent c)

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "Config" $ \o -> do
        current o <|>
            ((toCurrent :: V04.Config -> BundleConfig) <$> parseYamlFile basedir (Object o))
      where
        current o = BundleConfig
            <$> ((Just <$> parseYamlFile basedir (Object o)) <|> pure Nothing)
            <*> lookupBaseMaybe basedir o "static-hosts" .!= Set.empty
            <*> o .:? "redirects" .!= Set.empty

data StaticHost = StaticHost
    { shHost :: Text
    , shRoot :: F.FilePath
    }
    deriving (Eq, Ord)

instance ToCurrent V04.StaticHost where
    type Current V04.StaticHost = StaticHost
    toCurrent (V04.StaticHost a b) = StaticHost a b

instance ParseYamlFile StaticHost where
    parseYamlFile basedir = withObject "StaticHost" $ \o -> StaticHost
        <$> o .: "host"
        <*> lookupBase basedir o "root"

data Redirect = Redirect
    { redFrom :: Text
    , redTo :: Text
    }
    deriving (Eq, Ord)

instance ToCurrent V04.Redirect where
    type Current V04.Redirect = Redirect
    toCurrent (V04.Redirect a b) = Redirect a b

instance FromJSON Redirect where
    parseJSON (Object o) = Redirect
        <$> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"
