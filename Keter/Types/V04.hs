{-# LANGUAGE OverloadedStrings #-}
-- | Legacy types from Keter version 0.4. Retained to keep backwards
-- compatibility in config file format.
module Keter.Types.V04 where

import Prelude hiding (FilePath)
import Data.Yaml.FilePath
import Data.Aeson
import Control.Applicative
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Filesystem.Path as F
import Filesystem.Path (FilePath)

data AppConfig = AppConfig
    { configExec :: F.FilePath
    , configArgs :: [Text]
    , configHost :: Text
    , configSsl :: Bool
    , configExtraHosts :: Set Text
    , configRaw :: Object
    }

instance ParseYamlFile AppConfig where
    parseYamlFile basedir = withObject "AppConfig" $ \o -> AppConfig
        <$> lookupBase basedir o "exec"
        <*> o .:? "args" .!= []
        <*> o .: "host"
        <*> o .:? "ssl" .!= False
        <*> o .:? "extra-hosts" .!= Set.empty
        <*> return o

data Config = Config
    { configApp :: Maybe AppConfig
    , configStaticHosts :: Set StaticHost
    , configRedirects :: Set Redirect
    }

instance ParseYamlFile Config where
    parseYamlFile basedir = withObject "Config" $ \o -> Config
        <$> ((Just <$> parseYamlFile basedir (Object o)) <|> pure Nothing)
        <*> lookupBaseMaybe basedir o "static-hosts" .!= Set.empty
        <*> o .:? "redirects" .!= Set.empty

data StaticHost = StaticHost
    { shHost :: Text
    , shRoot :: FilePath
    }
    deriving (Eq, Ord)

instance ParseYamlFile StaticHost where
    parseYamlFile basedir = withObject "StaticHost" $ \o -> StaticHost
        <$> o .: "host"
        <*> lookupBase basedir o "root"

data Redirect = Redirect
    { redFrom :: Text
    , redTo :: Text
    }
    deriving (Eq, Ord)

instance FromJSON Redirect where
    parseJSON (Object o) = Redirect
        <$> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"
