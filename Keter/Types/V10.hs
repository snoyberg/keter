{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Keter.Types.V10 where

import Data.Text (Text)
import Data.Aeson (Object)
import Keter.Types.Common
import qualified Keter.Types.V04 as V04
import Data.Yaml.FilePath
import Data.Aeson (FromJSON (..), (.:), (.:?), Value (Object), withObject, (.!=))
import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Filesystem.Path as F
import Data.Default
import Data.String (fromString)
import Data.Conduit.Network (HostPreference)

-- Bundle configuration
data AppConfig = AppConfig
    { aconfigExec :: F.FilePath
    , aconfigArgs :: [Text]
    , aconfigHost :: Text
    , aconfigSsl :: Bool
    , aconfigExtraHosts :: Set Text
    , aconfigRaw :: Object
    }

instance ToCurrent AppConfig where
    type Previous AppConfig = V04.AppConfig
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

instance ToCurrent BundleConfig where
    type Previous BundleConfig = V04.BundleConfig
    toCurrent (V04.BundleConfig a b c) = BundleConfig (fmap toCurrent a) (Set.map toCurrent b) (Set.map toCurrent c)

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "Config" $ \o -> do
        current o <|>
            ((toCurrent :: V04.BundleConfig -> BundleConfig) <$> parseYamlFile basedir (Object o))
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

instance ToCurrent StaticHost where
    type Previous StaticHost = V04.StaticHost
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

instance ToCurrent Redirect where
    type Previous Redirect = V04.Redirect
    toCurrent (V04.Redirect a b) = Redirect a b

instance FromJSON Redirect where
    parseJSON (Object o) = Redirect
        <$> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

data KeterConfig = KeterConfig
    { kconfigDir :: F.FilePath
    , kconfigHostMan :: V04.PortSettings
    , kconfigHost :: HostPreference
    , kconfigPort :: Port
    , kconfigSsl :: Maybe V04.TLSConfig
    , kconfigSetuid :: Maybe Text
    , kconfigReverseProxy :: Set V04.ReverseProxyConfig
    , kconfigIpFromHeader :: Bool
    }

instance ToCurrent KeterConfig where
    type Previous KeterConfig = V04.KeterConfig
    toCurrent (V04.KeterConfig a b c d e f g h) = KeterConfig
        a
        b
        c
        d
        e
        f
        g
        h

instance Default KeterConfig where
    def = KeterConfig
        { kconfigDir = "."
        , kconfigHostMan = def
        , kconfigHost = "*"
        , kconfigPort = 80
        , kconfigSsl = Nothing
        , kconfigSetuid = Nothing
        , kconfigReverseProxy = Set.empty
        , kconfigIpFromHeader = False
        }

instance ParseYamlFile KeterConfig where
    parseYamlFile basedir = withObject "KeterConfig" $ \o ->
        current o <|>
            ((toCurrent :: V04.KeterConfig -> KeterConfig) <$> parseYamlFile basedir (Object o))
      where
        current o = KeterConfig
            <$> lookupBase basedir o "root"
            <*> o .:? "port-manager" .!= def
            <*> (fmap fromString <$> o .:? "host") .!= kconfigHost def
            <*> o .:? "port" .!= kconfigPort def
            <*> (o .:? "ssl" >>= maybe (return Nothing) (fmap Just . parseYamlFile basedir))
            <*> o .:? "setuid"
            <*> o .:? "reverse-proxy" .!= Set.empty
            <*> o .:? "ip-from-header" .!= False
