{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Keter.Types.V10 where

import Data.Aeson (Object)
import Keter.Types.Common
import qualified Keter.Types.V04 as V04
import Data.Yaml.FilePath
import Data.Aeson (FromJSON (..), (.:), (.:?), Value (Object), withObject, (.!=))
import Control.Applicative ((<$>), (<*>), pure, (<|>))
import qualified Data.Set as Set
import qualified Filesystem.Path.CurrentOS as F
import Data.Default
import Data.String (fromString)
import Data.Conduit.Network (HostPreference)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid (mempty)
import Network.HTTP.ReverseProxy.Rewrite (ReverseProxyConfig)
import Data.Maybe (fromMaybe)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

data BundleConfig = BundleConfig
    { bconfigStanzas :: !(Vector Stanza)
    , bconfigRaw     :: !Object -- ^ used for plugins
    }

instance ToCurrent BundleConfig where
    type Previous BundleConfig = V04.BundleConfig
    toCurrent (V04.BundleConfig webapp statics redirs) = BundleConfig (V.concat
        [ maybe V.empty V.singleton $ fmap (StanzaWebApp . toCurrent) webapp
        , V.fromList $ map (StanzaStaticFiles . toCurrent) $ Set.toList statics
        , V.fromList $ map (StanzaRedirect . toCurrent) $ Set.toList redirs
        ]) (maybe mempty V04.configRaw webapp)

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "Config" $ \o -> do
        current o <|>
            ((toCurrent :: V04.BundleConfig -> BundleConfig) <$> parseYamlFile basedir (Object o))
      where
        current o = BundleConfig
            <$> lookupBase basedir o "stanzas"
            <*> pure o

data ListeningPort = LPSecure !HostPreference !Port !F.FilePath !F.FilePath
                   | LPInsecure !HostPreference !Port

instance ParseYamlFile ListeningPort where
    parseYamlFile basedir = withObject "ListeningPort" $ \o -> do
        host <- (fmap fromString <$> o .:? "host") .!= "*"
        mcert <- lookupBaseMaybe basedir o "certificate"
        mkey <- lookupBaseMaybe basedir o "key"
        case (mcert, mkey) of
            (Nothing, Nothing) -> do
                port <- o .:? "port" .!= 80
                return $ LPInsecure host port
            (Just cert, Just key) -> do
                port <- o .:? "port" .!= 443
                return $ LPSecure host port cert key
            _ -> fail "Must provide both certificate and key files"

data KeterConfig = KeterConfig
    { kconfigDir :: F.FilePath
    , kconfigPortPool :: V04.PortSettings
    , kconfigListeners :: !(NonEmptyVector ListeningPort)
    , kconfigSetuid :: Maybe Text
    , kconfigBuiltinStanzas :: !(V.Vector Stanza)
    , kconfigIpFromHeader :: Bool
    }

instance ToCurrent KeterConfig where
    type Previous KeterConfig = V04.KeterConfig
    toCurrent (V04.KeterConfig dir portman host port ssl setuid rproxy ipFromHeader) = KeterConfig
        { kconfigDir = dir
        , kconfigPortPool = portman
        , kconfigListeners = NonEmptyVector (LPInsecure host port) (getSSL ssl)
        , kconfigSetuid = setuid
        , kconfigBuiltinStanzas = V.fromList $ map StanzaReverseProxy $ Set.toList rproxy
        , kconfigIpFromHeader = ipFromHeader
        }
      where
        getSSL Nothing = V.empty
        getSSL (Just (V04.TLSConfig s ts)) = V.singleton $ LPSecure
            (Warp.settingsHost s)
            (Warp.settingsPort s)
            (F.decodeString $ WarpTLS.certFile ts)
            (F.decodeString $ WarpTLS.keyFile ts)

instance Default KeterConfig where
    def = KeterConfig
        { kconfigDir = "."
        , kconfigPortPool = def
        , kconfigListeners = NonEmptyVector (LPInsecure "*" 80) V.empty
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
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
            <*> fmap (fromMaybe (kconfigListeners def)) (lookupBaseMaybe basedir o "listeners")
            <*> o .:? "setuid"
            <*> return V.empty
            <*> o .:? "ip-from-header" .!= False

data Stanza = StanzaStaticFiles StaticFilesConfig
            | StanzaRedirect RedirectConfig
            | StanzaWebApp WebAppConfig
            | StanzaReverseProxy ReverseProxyConfig
            -- FIXME background job, console app

-- | An action to be performed for a requested hostname.
--
-- This datatype is very similar to Stanza, but is necessarily separate since:
--
-- 1. Webapps will be assigned ports.
--
-- 2. Not all stanzas have an associated proxy action.
data ProxyAction = PAPort Port
                 | PAStatic StaticFilesConfig
                 | PARedirect RedirectConfig
                 | PAReverseProxy ReverseProxyConfig

instance ParseYamlFile Stanza where
    parseYamlFile basedir = withObject "Stanza" $ \o -> do
        typ <- o .: "type"
        case typ of
            "static-files" -> fmap StanzaStaticFiles $ parseYamlFile basedir $ Object o
            "redirect" -> fmap StanzaRedirect $ parseYamlFile basedir $ Object o
            "webapp" -> fmap StanzaWebApp $ parseYamlFile basedir $ Object o
            "reverse-proxy" -> fmap StanzaReverseProxy $ parseJSON $ Object o
            _ -> fail $ "Unknown stanza type: " ++ typ

data StaticFilesConfig = StaticFilesConfig
    { sfconfigRoot     :: !F.FilePath
    , sfconfigHosts    :: !(Set Host)
    , sfconfigListings :: !Bool
    -- FIXME basic auth
    }

instance ToCurrent StaticFilesConfig where
    type Previous StaticFilesConfig = V04.StaticHost
    toCurrent (V04.StaticHost host root) = StaticFilesConfig
        { sfconfigRoot = root
        , sfconfigHosts = Set.singleton host
        , sfconfigListings = True
        }

instance ParseYamlFile StaticFilesConfig where
    parseYamlFile basedir = withObject "StaticFilesConfig" $ \o -> StaticFilesConfig
        <$> lookupBase basedir o "root"
        <*> (o .: "hosts" <|> (Set.singleton <$> (o .: "host")))
        <*> o .:? "directory-listing" .!= False

data RedirectConfig = RedirectConfig
    { redirconfigHosts :: !(Set Host)
    , redirconfigStatus :: !Int
    , redirconfigActions :: !(Vector RedirectAction)
    }

instance ToCurrent RedirectConfig where
    type Previous RedirectConfig = V04.Redirect
    toCurrent (V04.Redirect from to) = RedirectConfig
        { redirconfigHosts = Set.singleton from
        , redirconfigStatus = 301
        , redirconfigActions = V.singleton $ RedirectAction SPAny
                             $ RDPrefix False to 80
        }

instance ParseYamlFile RedirectConfig where
    parseYamlFile _ = withObject "RedirectConfig" $ \o -> RedirectConfig
        <$> (o .: "hosts" <|> (Set.singleton <$> (o .: "host")))
        <*> o .:? "status" .!= 303
        <*> o .: "actions"

data RedirectAction = RedirectAction !SourcePath !RedirectDest

instance FromJSON RedirectAction where
    parseJSON = withObject "RedirectAction" $ \o -> RedirectAction
        <$> (maybe SPAny SPSpecific <$> (o .:? "path"))
        <*> parseJSON (Object o)

data SourcePath = SPAny
                | SPSpecific !Text

data RedirectDest = RDUrl !Text
                  | RDPrefix !IsSecure !Host !Port

instance FromJSON RedirectDest where
    parseJSON = withObject "RedirectDest" $ \o ->
        url o <|> prefix o
      where
        url o = RDUrl <$> o .: "url"
        prefix o = RDPrefix
            <$> o .:? "secure" .!= False
            <*> o .: "host"
            <*> o .: "port"

type IsSecure = Bool

data WebAppConfig = WebAppConfig
    { waconfigExec        :: !F.FilePath
    , waconfigArgs        :: !(Vector Text)
    , waconfigApprootHost :: !Text -- ^ primary host, used for approot
    , waconfigHosts       :: !(Set Text) -- ^ all hosts, not including the approot host
    , waconfigSsl         :: !Bool
    }

instance ToCurrent WebAppConfig where
    type Previous WebAppConfig = V04.AppConfig
    toCurrent (V04.AppConfig exec args host ssl hosts _raw) = WebAppConfig
        { waconfigExec = exec
        , waconfigArgs = V.fromList args
        , waconfigApprootHost = host
        , waconfigHosts = hosts
        , waconfigSsl = ssl
        }

instance ParseYamlFile WebAppConfig where
    parseYamlFile basedir = withObject "WebAppConfig" $ \o -> do
        (ahost, hosts) <-
            (do
                h <- o .: "host"
                return (h, Set.empty)) <|>
            (do
                hs <- o .: "hosts"
                case hs of
                    [] -> fail "Must provide at least one host"
                    h:hs' -> return (h, Set.fromList hs'))
        WebAppConfig
            <$> lookupBase basedir o "exec"
            <*> o .:? "args" .!= V.empty
            <*> return ahost
            <*> return hosts
            <*> o .:? "ssl" .!= False
