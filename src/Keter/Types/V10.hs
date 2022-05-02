{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Keter.Types.V10 where

import           Control.Applicative               ((<$>), (<*>), (<|>))
import           Data.Aeson                        (FromJSON (..), ToJSON (..), Object,
                                                    Value (Object, String, Bool),
                                                    withObject, (.!=), (.:),
                                                    (.:?), object, (.=))
import           Data.Aeson.KeyHelper              as AK (lookup, singleton, empty, insert)
import qualified Data.CaseInsensitive              as CI
import           Data.Conduit.Network              (HostPreference)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe, isJust)
import qualified Data.Set                          as Set
import           Data.String                       (fromString)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import           Data.Word                         (Word)
import           Data.Yaml.FilePath
import qualified System.FilePath                   as F
import           Keter.Types.Common
import           Keter.Types.Middleware
import qualified Keter.Types.V04                   as V04
import           Network.HTTP.ReverseProxy.Rewrite (ReverseProxyConfig)
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import           System.Posix.Types                (EpochTime)

data BundleConfig = BundleConfig
    { bconfigStanzas :: !(Vector (Stanza ()))
    , bconfigPlugins :: !Object -- ^ settings used for plugins
    } deriving Show

instance ToCurrent BundleConfig where
    type Previous BundleConfig = V04.BundleConfig
    toCurrent (V04.BundleConfig webapp statics redirs) = BundleConfig
        { bconfigStanzas = V.concat
            [ maybe V.empty V.singleton $ fmap (flip Stanza False . StanzaWebApp . toCurrent) webapp
            , V.fromList $ map (flip Stanza False . StanzaStaticFiles . toCurrent) $ Set.toList statics
            , V.fromList $ map (flip Stanza False . StanzaRedirect . toCurrent) $ Set.toList redirs
            ]
        , bconfigPlugins =
            case webapp >>= AK.lookup "postgres" . V04.configRaw of
                Just (Bool True) -> AK.singleton "postgres" (Bool True)
                _ -> AK.empty
        }

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "BundleConfig" $ \o ->
        case AK.lookup "stanzas" o of
            Nothing -> (toCurrent :: V04.BundleConfig -> BundleConfig) <$> parseYamlFile basedir (Object o)
            Just _ -> current o
      where
        current o = BundleConfig
            <$> lookupBase basedir o "stanzas"
            <*> o .:? "plugins" .!= AK.empty

instance ToJSON BundleConfig where
    toJSON BundleConfig {..} = object
        [ "stanzas" .= bconfigStanzas
        , "plugins" .= bconfigPlugins
        ]

data ListeningPort = LPSecure !HostPreference !Port
                              !F.FilePath !(V.Vector F.FilePath) !F.FilePath
                              !Bool
                   | LPInsecure !HostPreference !Port

instance ParseYamlFile ListeningPort where
    parseYamlFile basedir = withObject "ListeningPort" $ \o -> do
        host <- (fmap fromString <$> o .:? "host") .!= "*"
        mcert <- lookupBaseMaybe basedir o "certificate"
        mkey <- lookupBaseMaybe basedir o "key"
        session <- o .:? "session" .!= False
        case (mcert, mkey) of
            (Nothing, Nothing) -> do
                port <- o .:? "port" .!= 80
                return $ LPInsecure host port
            (Just cert, Just key) -> do
                port <- o .:? "port" .!= 443
                chainCerts <- o .:? "chain-certificates"
                    >>= maybe (return V.empty) (parseYamlFile basedir)
                return $ LPSecure host port cert chainCerts key session
            _ -> fail "Must provide both certificate and key files"

data KeterConfig = KeterConfig
    { kconfigDir                 :: F.FilePath
    , kconfigPortPool            :: V04.PortSettings
    , kconfigListeners           :: !(NonEmptyVector ListeningPort)
    , kconfigSetuid              :: Maybe Text
    , kconfigBuiltinStanzas      :: !(V.Vector (Stanza ()))
    , kconfigIpFromHeader        :: Bool
    , kconfigExternalHttpPort    :: !Int
    -- ^ External HTTP port when generating APPROOTs.
    , kconfigExternalHttpsPort   :: !Int
    -- ^ External HTTPS port when generating APPROOTs.
    , kconfigEnvironment         :: !(Map Text Text)
    -- ^ Environment variables to be passed to all apps.
    , kconfigConnectionTimeBound :: !Int
    -- ^ Maximum request time in milliseconds per connection.
    , kconfigCliPort             :: !(Maybe Port)
    -- ^ Port for the cli to listen on

    , kconfigUnknownHostResponse  :: !(Maybe F.FilePath)
    , kconfigMissingHostResponse  :: !(Maybe F.FilePath)
    , kconfigProxyException       :: !(Maybe F.FilePath)
    }

instance ToCurrent KeterConfig where
    type Previous KeterConfig = V04.KeterConfig
    toCurrent (V04.KeterConfig dir portman host port ssl setuid rproxy ipFromHeader connectionTimeBound) = KeterConfig
        { kconfigDir = dir
        , kconfigPortPool = portman
        , kconfigListeners = NonEmptyVector (LPInsecure host port) (getSSL ssl)
        , kconfigSetuid = setuid
        , kconfigBuiltinStanzas = V.fromList $ map (flip Stanza False . (\rp -> StanzaReverseProxy rp [] Nothing)) $ Set.toList rproxy
        , kconfigIpFromHeader = ipFromHeader
        , kconfigExternalHttpPort = 80
        , kconfigExternalHttpsPort = 443
        , kconfigEnvironment = Map.empty
        , kconfigConnectionTimeBound = connectionTimeBound
        , kconfigCliPort             = Nothing
        , kconfigUnknownHostResponse  = Nothing
        , kconfigMissingHostResponse = Nothing
        , kconfigProxyException      = Nothing
        }
      where
        getSSL Nothing = V.empty
        getSSL (Just (V04.TLSConfig s cert key session)) = V.singleton $ LPSecure
            (Warp.getHost s)
            (Warp.getPort s)
            cert
            V.empty
            key
            (isJust session)

defaultKeterConfig :: KeterConfig
defaultKeterConfig = KeterConfig
        { kconfigDir = "."
        , kconfigPortPool = V04.defaultPortSettings
        , kconfigListeners = NonEmptyVector (LPInsecure "*" 80) V.empty
        , kconfigSetuid = Nothing
        , kconfigBuiltinStanzas = V.empty
        , kconfigIpFromHeader = False
        , kconfigExternalHttpPort = 80
        , kconfigExternalHttpsPort = 443
        , kconfigEnvironment = Map.empty
        , kconfigConnectionTimeBound = V04.fiveMinutes
        , kconfigCliPort             = Nothing
        , kconfigUnknownHostResponse  = Nothing
        , kconfigMissingHostResponse = Nothing
        , kconfigProxyException      = Nothing
        }

instance ParseYamlFile KeterConfig where
    parseYamlFile basedir = withObject "KeterConfig" $ \o ->
        case AK.lookup "listeners" o of
            Just _ -> current o
            Nothing -> old o <|> current o
      where
        old o = (toCurrent :: V04.KeterConfig -> KeterConfig) <$> parseYamlFile basedir (Object o)
        current o = KeterConfig
            <$> lookupBase basedir o "root"
            <*> o .:? "port-manager" .!= V04.defaultPortSettings
            <*> fmap (fromMaybe (kconfigListeners defaultKeterConfig)) (lookupBaseMaybe basedir o "listeners")
            <*> o .:? "setuid"
            <*> return V.empty
            <*> o .:? "ip-from-header" .!= False
            <*> o .:? "external-http-port" .!= 80
            <*> o .:? "external-https-port" .!= 443
            <*> o .:? "env" .!= Map.empty
            <*> o .:? "connection-time-bound" .!= V04.fiveMinutes
            <*> o .:? "cli-port"
            <*> o .:? "missing-host-response-file"
            <*> o .:? "unknown-host-response-file"
            <*> o .:? "proxy-exception-response-file"

-- | Whether we should force redirect to HTTPS routes.
type RequiresSecure = Bool

data Stanza port = Stanza (StanzaRaw port) RequiresSecure
  deriving Show

data StanzaRaw port
    = StanzaStaticFiles !StaticFilesConfig
    | StanzaRedirect !RedirectConfig
    | StanzaWebApp !(WebAppConfig port)
    | StanzaReverseProxy !ReverseProxyConfig ![ MiddlewareConfig ] !(Maybe Int)
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
data ProxyActionRaw
    = PAPort Port !(Maybe Int)
    | PAStatic StaticFilesConfig
    | PARedirect RedirectConfig
    | PAReverseProxy ReverseProxyConfig ![ MiddlewareConfig ] !(Maybe Int)
    deriving Show

type ProxyAction = (ProxyActionRaw, RequiresSecure)

instance ParseYamlFile (Stanza ()) where
    parseYamlFile basedir = withObject "Stanza" $ \o -> do
        typ <- o .: "type"
        needsHttps <- o .:? "requires-secure" .!= False
        raw <- case typ of
            "static-files"  -> fmap StanzaStaticFiles $ parseYamlFile basedir $ Object o
            "redirect"      -> fmap StanzaRedirect $ parseYamlFile basedir $ Object o
            "webapp"        -> fmap StanzaWebApp $ parseYamlFile basedir $ Object o
            "reverse-proxy" -> StanzaReverseProxy <$> parseJSON (Object o)
                                                  <*> o .:? "middleware" .!= []
                                                  <*> o .:? "connection-time-bound"
            "background"    -> fmap StanzaBackground $ parseYamlFile basedir $ Object o
            _ -> fail $ "Unknown stanza type: " ++ typ
        return $ Stanza raw needsHttps

instance ToJSON (Stanza ()) where
    toJSON (Stanza raw rs) = addRequiresSecure rs raw

addRequiresSecure :: ToJSON a => Bool -> a -> Value
addRequiresSecure rs x =
    case toJSON x of
        Object o -> Object $ AK.insert "requires-secure" (toJSON rs) o
        v -> v

instance ToJSON (StanzaRaw ()) where
    toJSON (StanzaStaticFiles x) = addStanzaType "static-files" x
    toJSON (StanzaRedirect x) = addStanzaType "redirect" x
    toJSON (StanzaWebApp x) = addStanzaType "webapp" x
    toJSON (StanzaReverseProxy x _ _) = addStanzaType "reverse-proxy" x
    toJSON (StanzaBackground x) = addStanzaType "background" x

addStanzaType :: ToJSON a => Value -> a -> Value
addStanzaType t x =
    case toJSON x of
        Object o -> Object $ AK.insert "type" t o
        v -> v

data StaticFilesConfig = StaticFilesConfig
    { sfconfigRoot       :: !F.FilePath
    , sfconfigHosts      :: !(Set Host)
    , sfconfigListings   :: !Bool
    -- FIXME basic auth
    , sfconfigMiddleware :: ![ MiddlewareConfig ]
    , sfconfigTimeout    :: !(Maybe Int)
    , sfconfigSsl        :: !SSLConfig
    }
    deriving Show

instance ToCurrent StaticFilesConfig where
    type Previous StaticFilesConfig = V04.StaticHost
    toCurrent (V04.StaticHost host root) = StaticFilesConfig
        { sfconfigRoot       = root
        , sfconfigHosts      = Set.singleton $ CI.mk host
        , sfconfigListings   = True
        , sfconfigMiddleware = []
        , sfconfigTimeout    = Nothing
        , sfconfigSsl        = SSLFalse
        }

instance ParseYamlFile StaticFilesConfig where
    parseYamlFile basedir = withObject "StaticFilesConfig" $ \o -> StaticFilesConfig
        <$> lookupBase basedir o "root"
        <*> (Set.map CI.mk <$> (o .: "hosts" <|> (Set.singleton <$> (o .: "host"))))
        <*> o .:? "directory-listing" .!= False
        <*> o .:? "middleware" .!= []
        <*> o .:? "connection-time-bound"
        <*> o .:? "ssl" .!= SSLFalse

instance ToJSON StaticFilesConfig where
    toJSON StaticFilesConfig {..} = object
        [ "root" .= sfconfigRoot
        , "hosts" .= Set.map CI.original sfconfigHosts
        , "directory-listing" .= sfconfigListings
        , "middleware" .= sfconfigMiddleware
        , "connection-time-bound" .= sfconfigTimeout
        , "ssl" .= sfconfigSsl
        ]

data RedirectConfig = RedirectConfig
    { redirconfigHosts   :: !(Set Host)
    , redirconfigStatus  :: !Int
    , redirconfigActions :: !(Vector RedirectAction)
    , redirconfigSsl     :: !SSLConfig
    }
    deriving Show

instance ToCurrent RedirectConfig where
    type Previous RedirectConfig = V04.Redirect
    toCurrent (V04.Redirect from to) = RedirectConfig
        { redirconfigHosts = Set.singleton $ CI.mk from
        , redirconfigStatus = 301
        , redirconfigActions = V.singleton $ RedirectAction SPAny
                             $ RDPrefix False (CI.mk to) Nothing
        , redirconfigSsl = SSLFalse
        }

instance ParseYamlFile RedirectConfig where
    parseYamlFile _ = withObject "RedirectConfig" $ \o -> RedirectConfig
        <$> (Set.map CI.mk <$> ((o .: "hosts" <|> (Set.singleton <$> (o .: "host")))))
        <*> o .:? "status" .!= 303
        <*> o .: "actions"
        <*> o .:? "ssl" .!= SSLFalse

instance ToJSON RedirectConfig where
    toJSON RedirectConfig {..} = object
        [ "hosts" .= Set.map CI.original redirconfigHosts
        , "status" .= redirconfigStatus
        , "actions" .= redirconfigActions
        , "ssl" .= redirconfigSsl
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
                    SPSpecific x -> Object $ AK.insert "path" (String x) o
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
            <*> (CI.mk <$> o .: "host")
            <*> o .:? "port"

instance ToJSON RedirectDest where
    toJSON (RDUrl url) = object ["url" .= url]
    toJSON (RDPrefix secure host mport) = object $ catMaybes
        [ Just $ "secure" .= secure
        , Just $ "host" .= CI.original host
        , case mport of
            Nothing -> Nothing
            Just port -> Just $ "port" .= port
        ]

type IsSecure = Bool

data WebAppConfig port = WebAppConfig
    { waconfigExec        :: !F.FilePath
    , waconfigArgs        :: !(Vector Text)
    , waconfigEnvironment :: !(Map Text Text)
    , waconfigApprootHost :: !Host -- ^ primary host, used for approot
    , waconfigHosts       :: !(Set Host) -- ^ all hosts, not including the approot host
    , waconfigSsl         :: !SSLConfig
    , waconfigPort        :: !port
    , waconfigForwardEnv  :: !(Set Text)
     -- | how long are connections supposed to last
    , waconfigTimeout     :: !(Maybe Int)
     -- | how long in microseconds the app gets before we expect it to bind to
     --   a port (default 90 seconds)
    , waconfigEnsureAliveTimeout :: !(Maybe Int)
    }
    deriving Show

instance ToCurrent (WebAppConfig ()) where
    type Previous (WebAppConfig ()) = V04.AppConfig
    toCurrent (V04.AppConfig exec args host ssl hosts _raw) = WebAppConfig
        { waconfigExec = exec
        , waconfigArgs = V.fromList args
        , waconfigEnvironment = Map.empty
        , waconfigApprootHost = CI.mk host
        , waconfigHosts = Set.map CI.mk hosts
        , waconfigSsl = if ssl then SSLTrue else SSLFalse
        , waconfigPort = ()
        , waconfigForwardEnv = Set.empty
        , waconfigTimeout = Nothing
        , waconfigEnsureAliveTimeout = Nothing
        }

instance ParseYamlFile (WebAppConfig ()) where
    parseYamlFile basedir = withObject "WebAppConfig" $ \o -> do
        (ahost, hosts) <-
            (do
                h <- o .: "host"
                return (CI.mk h, Set.empty)) <|>
            (do
                hs <- o .: "hosts"
                case hs of
                    [] -> fail "Must provide at least one host"
                    h:hs' -> return (CI.mk h, Set.fromList $ map CI.mk hs'))
        WebAppConfig
            <$> lookupBase basedir o "exec"
            <*> o .:? "args" .!= V.empty
            <*> o .:? "env" .!= Map.empty
            <*> return ahost
            <*> return hosts
            <*> o .:? "ssl" .!= SSLFalse
            <*> return ()
            <*> o .:? "forward-env" .!= Set.empty
            <*> o .:? "connection-time-bound"
            <*> o .:? "ensure-alive-time-bound"

instance ToJSON (WebAppConfig ()) where
    toJSON WebAppConfig {..} = object
        [ "exec" .= waconfigExec
        , "args" .= waconfigArgs
        , "env" .= waconfigEnvironment
        , "hosts" .= map CI.original (waconfigApprootHost : Set.toList waconfigHosts)
        , "ssl" .= waconfigSsl
        , "forward-env" .= waconfigForwardEnv
        , "connection-time-bound" .= waconfigTimeout
        ]

data AppInput = AIBundle !FilePath !EpochTime
              | AIData !BundleConfig
              deriving Show

data BackgroundConfig = BackgroundConfig
    { bgconfigExec                :: !F.FilePath
    , bgconfigArgs                :: !(Vector Text)
    , bgconfigEnvironment         :: !(Map Text Text)
    , bgconfigRestartCount        :: !RestartCount
    , bgconfigRestartDelaySeconds :: !Word
    , bgconfigForwardEnv          :: !(Set Text)
    }
    deriving Show

data RestartCount = UnlimitedRestarts | LimitedRestarts !Word
    deriving Show

instance FromJSON RestartCount where
    parseJSON (String "unlimited") = return UnlimitedRestarts
    parseJSON v = LimitedRestarts <$> parseJSON v

instance ParseYamlFile BackgroundConfig where
    parseYamlFile basedir = withObject "BackgroundConfig" $ \o -> BackgroundConfig
        <$> lookupBase basedir o "exec"
        <*> o .:? "args" .!= V.empty
        <*> o .:? "env" .!= Map.empty
        <*> o .:? "restart-count" .!= UnlimitedRestarts
        <*> o .:? "restart-delay-seconds" .!= 5
        <*> o .:? "forward-env" .!= Set.empty

instance ToJSON BackgroundConfig where
    toJSON BackgroundConfig {..} = object $ catMaybes
        [ Just $ "exec" .= bgconfigExec
        , Just $ "args" .= bgconfigArgs
        , Just $ "env" .= bgconfigEnvironment
        , case bgconfigRestartCount of
            UnlimitedRestarts -> Nothing
            LimitedRestarts count -> Just $ "restart-count" .= count
        , Just $ "restart-delay-seconds" .= bgconfigRestartDelaySeconds
        , Just $ "forward-env" .= bgconfigForwardEnv
        ]
