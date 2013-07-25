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
import Data.Default
import Data.String (fromString)
import Data.Conduit.Network (HostPreference)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Filesystem.Path.CurrentOS (encodeString)
import Keter.Types.Common
import Network.HTTP.ReverseProxy.Rewrite

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

data BundleConfig = BundleConfig
    { bconfigApp :: Maybe AppConfig
    , bconfigStaticHosts :: Set StaticHost
    , bconfigRedirects :: Set Redirect
    }

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "BundleConfig" $ \o -> BundleConfig
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

data KeterConfig = KeterConfig
    { kconfigDir :: F.FilePath
    , kconfigPortMan :: PortSettings
    , kconfigHost :: HostPreference
    , kconfigPort :: Port
    , kconfigSsl :: Maybe TLSConfig
    , kconfigSetuid :: Maybe Text
    , kconfigReverseProxy :: Set ReverseProxyConfig
    , kconfigIpFromHeader :: Bool
    }

instance Default KeterConfig where
    def = KeterConfig
        { kconfigDir = "."
        , kconfigPortMan = def
        , kconfigHost = "*"
        , kconfigPort = 80
        , kconfigSsl = Nothing
        , kconfigSetuid = Nothing
        , kconfigReverseProxy = Set.empty
        , kconfigIpFromHeader = False
        }

instance ParseYamlFile KeterConfig where
    parseYamlFile basedir = withObject "KeterConfig" $ \o -> KeterConfig
        <$> lookupBase basedir o "root"
        <*> o .:? "port-manager" .!= def
        <*> (fmap fromString <$> o .:? "host") .!= kconfigHost def
        <*> o .:? "port" .!= kconfigPort def
        <*> (o .:? "ssl" >>= maybe (return Nothing) (fmap Just . parseYamlFile basedir))
        <*> o .:? "setuid"
        <*> o .:? "reverse-proxy" .!= Set.empty
        <*> o .:? "ip-from-header" .!= False

data TLSConfig = TLSConfig !Warp.Settings !WarpTLS.TLSSettings

instance ParseYamlFile TLSConfig where
    parseYamlFile basedir = withObject "TLSConfig" $ \o -> do
        cert <- lookupBase basedir o "certificate"
        key <- lookupBase basedir o "key"
        host <- (fmap fromString <$> o .:? "host") .!= "*"
        port <- o .:? "port" .!= 443
        return $! TLSConfig
            Warp.defaultSettings
                { Warp.settingsHost = host
                , Warp.settingsPort = port
                }
            WarpTLS.defaultTlsSettings
                { WarpTLS.certFile = encodeString cert
                , WarpTLS.keyFile = encodeString key
                }

-- | Controls execution of the nginx thread. Follows the settings type pattern.
-- See: <http://www.yesodweb.com/book/settings-types>.
data PortSettings = PortSettings
    { portRange :: [Port]
      -- ^ Which ports to assign to apps. Default: 4000-4999
    }

instance Default PortSettings where
    def = PortSettings
        { portRange = [4000..4999]
        }

instance FromJSON PortSettings where
    parseJSON = withObject "PortSettings" $ \_ -> PortSettings
        <$> return (portRange def)
