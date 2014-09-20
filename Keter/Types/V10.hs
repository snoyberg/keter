{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Keter.Types.V10 where

import Prelude hiding (FilePath)
import           System.Posix.Types      (EpochTime)
import Data.Aeson (Object, ToJSON (..))
import Keter.Types.Common
import qualified Keter.Types.V04 as V04
import Data.Yaml.FilePath
import Data.Aeson (FromJSON (..), (.:), (.:?), Value (Object, String), withObject, (.!=))
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Set as Set
import qualified Filesystem.Path.CurrentOS as F
import Data.Default
import Data.String (fromString)
import Data.Conduit.Network (HostPreference)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.HTTP.ReverseProxy.Rewrite (ReverseProxyConfig)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Aeson ((.=), Value (Bool), object)
import Data.Word (Word)

data BundleConfig = BundleConfig
    { bconfigStanzas :: !(Vector (Stanza ()))
    , bconfigPlugins :: !Object -- ^ settings used for plugins
    }

instance ToCurrent BundleConfig where
    type Previous BundleConfig = V04.BundleConfig
    toCurrent (V04.BundleConfig webapp statics redirs) = BundleConfig
        { bconfigStanzas = V.concat
            [ maybe V.empty V.singleton $ fmap (StanzaWebApp . toCurrent) webapp
            , V.fromList $ map (StanzaStaticFiles . toCurrent) $ Set.toList statics
            , V.fromList $ map (StanzaRedirect . toCurrent) $ Set.toList redirs
            ]
        , bconfigPlugins =
            case webapp >>= HashMap.lookup "postgres" . V04.configRaw of
                Just (Bool True) -> HashMap.singleton "postgres" (Bool True)
                _ -> HashMap.empty
        }

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "BundleConfig" $ \o -> do
        case HashMap.lookup "stanzas" o of
            Nothing -> (toCurrent :: V04.BundleConfig -> BundleConfig) <$> parseYamlFile basedir (Object o)
            Just _ -> current o
      where
        current o = BundleConfig
            <$> lookupBase basedir o "stanzas"
            <*> o .:? "plugins" .!= HashMap.empty

instance ToJSON BundleConfig where
    toJSON BundleConfig {..} = object
        [ "stanzas" .= bconfigStanzas
        , "plugins" .= bconfigPlugins
        ]

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
    , kconfigBuiltinStanzas :: !(V.Vector (Stanza ()))
    , kconfigIpFromHeader :: Bool
    , kconfigExternalHttpPort :: !Int
    -- ^ External HTTP port when generating APPROOTs.
    , kconfigExternalHttpsPort :: !Int
    -- ^ External HTTPS port when generating APPROOTs.
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
        , kconfigExternalHttpPort = 80
        , kconfigExternalHttpsPort = 443
        }
      where
        getSSL Nothing = V.empty
        getSSL (Just (V04.TLSConfig s ts)) = V.singleton $ LPSecure
            (Warp.getHost s)
            (Warp.getPort s)
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
        , kconfigExternalHttpPort = 80
        , kconfigExternalHttpsPort = 443
        }

instance ParseYamlFile KeterConfig where
    parseYamlFile basedir = withObject "KeterConfig" $ \o ->
        case HashMap.lookup "listeners" o of
            Just _ -> current o
            Nothing -> old o <|> current o
      where
        old o = (toCurrent :: V04.KeterConfig -> KeterConfig) <$> parseYamlFile basedir (Object o)
        current o = KeterConfig
            <$> lookupBase basedir o "root"
            <*> o .:? "port-manager" .!= def
            <*> fmap (fromMaybe (kconfigListeners def)) (lookupBaseMaybe basedir o "listeners")
            <*> o .:? "setuid"
            <*> return V.empty
            <*> o .:? "ip-from-header" .!= False
            <*> o .:? "external-http-port" .!= 80
            <*> o .:? "external-https-port" .!= 443

data Stanza port
    = StanzaStaticFiles !StaticFilesConfig
    | StanzaRedirect !RedirectConfig
    | StanzaWebApp !(WebAppConfig port)
    | StanzaReverseProxy !ReverseProxyConfig
    | StanzaBackground !BackgroundConfig
            -- FIXME console app
    deriving Show

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
    deriving Show

instance ParseYamlFile (Stanza ()) where
    parseYamlFile basedir = withObject "Stanza" $ \o -> do
        typ <- o .: "type"
        case typ of
            "static-files" -> fmap StanzaStaticFiles $ parseYamlFile basedir $ Object o
            "redirect" -> fmap StanzaRedirect $ parseYamlFile basedir $ Object o
            "webapp" -> fmap StanzaWebApp $ parseYamlFile basedir $ Object o
            "reverse-proxy" -> fmap StanzaReverseProxy $ parseJSON $ Object o
            "background" -> fmap StanzaBackground $ parseYamlFile basedir $ Object o
            _ -> fail $ "Unknown stanza type: " ++ typ

instance ToJSON (Stanza ()) where
    toJSON (StanzaStaticFiles x) = addStanzaType "static-files" x
    toJSON (StanzaRedirect x) = addStanzaType "redirect" x
    toJSON (StanzaWebApp x) = addStanzaType "webapp" x
    toJSON (StanzaReverseProxy x) = addStanzaType "reverse-proxy" x
    toJSON (StanzaBackground x) = addStanzaType "background" x

addStanzaType :: ToJSON a => Value -> a -> Value
addStanzaType t x =
    case toJSON x of
        Object o -> Object $ HashMap.insert "type" t o
        v -> v

data StaticFilesConfig = StaticFilesConfig
    { sfconfigRoot     :: !F.FilePath
    , sfconfigHosts    :: !(Set Host)
    , sfconfigListings :: !Bool
    -- FIXME basic auth
    }
    deriving Show

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

instance ToJSON StaticFilesConfig where
    toJSON StaticFilesConfig {..} = object
        [ "root" .= F.encodeString sfconfigRoot
        , "hosts" .= sfconfigHosts
        , "directory-listing" .= sfconfigListings
        ]

data RedirectConfig = RedirectConfig
    { redirconfigHosts :: !(Set Host)
    , redirconfigStatus :: !Int
    , redirconfigActions :: !(Vector RedirectAction)
    }
    deriving Show

instance ToCurrent RedirectConfig where
    type Previous RedirectConfig = V04.Redirect
    toCurrent (V04.Redirect from to) = RedirectConfig
        { redirconfigHosts = Set.singleton from
        , redirconfigStatus = 301
        , redirconfigActions = V.singleton $ RedirectAction SPAny
                             $ RDPrefix False to Nothing
        }

instance ParseYamlFile RedirectConfig where
    parseYamlFile _ = withObject "RedirectConfig" $ \o -> RedirectConfig
        <$> (o .: "hosts" <|> (Set.singleton <$> (o .: "host")))
        <*> o .:? "status" .!= 303
        <*> o .: "actions"

instance ToJSON RedirectConfig where
    toJSON RedirectConfig {..} = object
        [ "hosts" .= redirconfigHosts
        , "status" .= redirconfigStatus
        , "actions" .= redirconfigActions
        ]

data RedirectAction = RedirectAction !SourcePath !RedirectDest
    deriving Show

instance FromJSON RedirectAction where
    parseJSON = withObject "RedirectAction" $ \o -> RedirectAction
        <$> (maybe SPAny SPSpecific <$> (o .:? "path"))
        <*> parseJSON (Object o)

instance ToJSON RedirectAction where
    toJSON (RedirectAction path dest) =
        case toJSON dest of
            Object o ->
                case path of
                    SPAny -> Object o
                    SPSpecific x -> Object $ HashMap.insert "path" (String x) o
            v -> v

data SourcePath = SPAny
                | SPSpecific !Text
    deriving Show

data RedirectDest = RDUrl !Text
                  | RDPrefix !IsSecure !Host !(Maybe Port)
    deriving Show

instance FromJSON RedirectDest where
    parseJSON = withObject "RedirectDest" $ \o ->
        url o <|> prefix o
      where
        url o = RDUrl <$> o .: "url"
        prefix o = RDPrefix
            <$> o .:? "secure" .!= False
            <*> o .: "host"
            <*> o .:? "port"

instance ToJSON RedirectDest where
    toJSON (RDUrl url) = object ["url" .= url]
    toJSON (RDPrefix secure host mport) = object $ catMaybes
        [ Just $ "secure" .= secure
        , Just $ "host" .= host
        , case mport of
            Nothing -> Nothing
            Just port -> Just $ "port" .= port
        ]

type IsSecure = Bool

data WebAppConfig port = WebAppConfig
    { waconfigExec        :: !F.FilePath
    , waconfigArgs        :: !(Vector Text)
    , waconfigEnvironment :: !(Map Text Text)
    , waconfigApprootHost :: !Text -- ^ primary host, used for approot
    , waconfigHosts       :: !(Set Text) -- ^ all hosts, not including the approot host
    , waconfigSsl         :: !Bool
    , waconfigPort        :: !port
    }
    deriving Show

instance ToCurrent (WebAppConfig ()) where
    type Previous (WebAppConfig ()) = V04.AppConfig
    toCurrent (V04.AppConfig exec args host ssl hosts _raw) = WebAppConfig
        { waconfigExec = exec
        , waconfigArgs = V.fromList args
        , waconfigEnvironment = Map.empty
        , waconfigApprootHost = host
        , waconfigHosts = hosts
        , waconfigSsl = ssl
        , waconfigPort = ()
        }

instance ParseYamlFile (WebAppConfig ()) where
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
            <*> o .:? "env" .!= Map.empty
            <*> return ahost
            <*> return hosts
            <*> o .:? "ssl" .!= False
            <*> return ()

instance ToJSON (WebAppConfig ()) where
    toJSON WebAppConfig {..} = object
        [ "exec" .= F.encodeString waconfigExec
        , "args" .= waconfigArgs
        , "env" .= waconfigEnvironment
        , "hosts" .= (waconfigApprootHost : Set.toList waconfigHosts)
        , "ssl" .= waconfigSsl
        ]

data AppInput = AIBundle !FilePath !EpochTime
              | AIData !BundleConfig

data BackgroundConfig = BackgroundConfig
    { bgconfigExec :: !F.FilePath
    , bgconfigArgs :: !(Vector Text)
    , bgconfigEnvironment :: !(Map Text Text)
    , bgconfigRestartCount :: !RestartCount
    , bgconfigRestartDelaySeconds :: !Word
    }
    deriving Show

data RestartCount = UnlimitedRestarts | LimitedRestarts !Word
    deriving Show

instance FromJSON RestartCount where
    parseJSON (String "unlimited") = return UnlimitedRestarts
    parseJSON v = fmap LimitedRestarts $ parseJSON v

instance ParseYamlFile BackgroundConfig where
    parseYamlFile basedir = withObject "BackgroundConfig" $ \o -> BackgroundConfig
        <$> lookupBase basedir o "exec"
        <*> o .:? "args" .!= V.empty
        <*> o .:? "env" .!= Map.empty
        <*> o .:? "restart-count" .!= UnlimitedRestarts
        <*> o .:? "restart-delay-seconds" .!= 5

instance ToJSON BackgroundConfig where
    toJSON BackgroundConfig {..} = object $ catMaybes
        [ Just $ "exec" .= F.encodeString bgconfigExec
        , Just $ "args" .= bgconfigArgs
        , Just $ "env" .= bgconfigEnvironment
        , case bgconfigRestartCount of
            UnlimitedRestarts -> Nothing
            LimitedRestarts count -> Just $ "restart-count" .= count
        , Just $ "restart-delay-seconds" .= bgconfigRestartDelaySeconds
        ]
