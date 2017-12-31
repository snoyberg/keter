{-# LANGUAGE OverloadedStrings #-}
-- | Legacy types from Keter version 0.4. Retained to keep backwards
-- compatibility in config file format.
module Keter.Types.V04 where

import           Control.Applicative
import           Data.Aeson
import           Data.Bool
import           Data.Conduit.Network              (HostPreference)
import           Data.Default
import qualified Data.Set                          as Set
import           Data.String                       (fromString)
import           Data.Yaml.FilePath
import qualified System.FilePath                   as F
import           Keter.Types.Common
import           Network.HTTP.ReverseProxy.Rewrite
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import qualified Network.TLS.SessionManager        as TLSSession
import           Prelude                           hiding (FilePath)

data AppConfig = AppConfig
    { configExec       :: F.FilePath
    , configArgs       :: [Text]
    , configHost       :: Text
    , configSsl        :: Bool
    , configExtraHosts :: Set Text
    , configRaw        :: Object
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
    { bconfigApp         :: Maybe AppConfig
    , bconfigStaticHosts :: Set StaticHost
    , bconfigRedirects   :: Set Redirect
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
    , redTo   :: Text
    }
    deriving (Eq, Ord)

instance FromJSON Redirect where
    parseJSON (Object o) = Redirect
        <$> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

data KeterConfig = KeterConfig
    { kconfigDir                 :: F.FilePath
    , kconfigPortMan             :: PortSettings
    , kconfigHost                :: HostPreference
    , kconfigPort                :: Port
    , kconfigSsl                 :: Maybe TLSConfig
    , kconfigSetuid              :: Maybe Text
    , kconfigReverseProxy        :: Set ReverseProxyConfig
    , kconfigIpFromHeader        :: Bool
    , kconfigConnectionTimeBound :: Int
    -- ^ Maximum request time in milliseconds per connection.
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
        , kconfigConnectionTimeBound = fiveMinutes
        }


-- | Default connection time bound in milliseconds.
fiveMinutes :: Int
fiveMinutes = 5 * 60 * 1000

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
        <*> o .:? "connection-time-bound" .!= fiveMinutes

data TLSConfig = TLSConfig !Warp.Settings !WarpTLS.TLSSettings

instance ParseYamlFile TLSConfig where
    parseYamlFile basedir = withObject "TLSConfig" $ \o -> do
        cert <- lookupBase basedir o "certificate"
        key <- lookupBase basedir o "key"
        host <- (fmap fromString <$> o .:? "host") .!= "*"
        port <- o .:? "port" .!= 443
        session <- bool Nothing (Just TLSSession.defaultConfig) <$> o .:? "session" .!= False
        return $! TLSConfig
            ( Warp.setHost host
            $ Warp.setPort port
              Warp.defaultSettings)
            WarpTLS.defaultTlsSettings
                { WarpTLS.certFile = cert
                , WarpTLS.keyFile = key
                , WarpTLS.tlsSessionManagerConfig = session
                }

-- | Controls execution of the nginx thread. Follows the settings type pattern.
-- See: <http://www.yesodweb.com/book/settings-types>.
data PortSettings = PortSettings
    { portRange :: [Port]
      -- ^ Which ports to assign to apps. Defaults to unassigned ranges from IANA
    }

instance Default PortSettings where
    def = PortSettings
        -- Top 10 Largest IANA unassigned port ranges with no unauthorized uses known
        { portRange = [43124..44320]
                      ++ [28120..29166]
                      ++ [45967..46997]
                      ++ [28241..29117]
                      ++ [40001..40840]
                      ++ [29170..29998]
                      ++ [38866..39680]
                      ++ [43442..44122]
                      ++ [41122..41793]
                      ++ [35358..36000]
        }

instance FromJSON PortSettings where
    parseJSON = withObject "PortSettings" $ \_ -> PortSettings
        <$> return (portRange def)
