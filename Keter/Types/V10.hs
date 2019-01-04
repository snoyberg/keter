{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Keter.Types.V10 where

import           Control.Applicative               ((<$>), (<*>), (<|>), pure)
import           Data.Aeson                        (Object, ToJSON (..))
import           Data.Aeson                        (FromJSON (..), Value (Object, String, Bool),
                                                    withBool, withObject, (.!=),
                                                    (.:), (.:?))
import           Data.Aeson                        (Value (Bool), object, (.=))
import qualified Data.CaseInsensitive              as CI
import           Data.Conduit.Network              (HostPreference)
import           Data.Default
import qualified Data.HashMap.Strict               as HashMap
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as Set
import           Data.String                       (fromString)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import           Data.Word                         (Word)
import           Data.Yaml.FilePath
import           Keter.Types.Common
import           Keter.Types.Middleware
import qualified Keter.Types.V04                   as V04
import           Network.HTTP.ReverseProxy.Rewrite (ReverseProxyConfig)
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import qualified System.FilePath                   as F
import           System.Posix.Types                (EpochTime)

data BundleConfig = BundleConfig
    { bconfigStanzas :: !(Vector (Stanza ()))
    , bconfigPlugins :: !Object -- ^ settings used for plugins
    }

instance ToCurrent BundleConfig where
    type Previous BundleConfig = V04.BundleConfig
    toCurrent (V04.BundleConfig webapp statics redirs) = BundleConfig
        { bconfigStanzas = V.concat
            [ maybe V.empty V.singleton $ fmap (flip Stanza False . StanzaWebApp . toCurrent) webapp
            , V.fromList $ map (flip Stanza False . StanzaStaticFiles . toCurrent) $ Set.toList statics
            , V.fromList $ map (flip Stanza False . StanzaRedirect . toCurrent) $ Set.toList redirs
            ]
        , bconfigPlugins =
            case webapp >>= HashMap.lookup "postgres" . V04.configRaw of
                Just (Bool True) -> HashMap.singleton "postgres" (Bool True)
                _ -> HashMap.empty
        }

instance ParseYamlFile BundleConfig where
    parseYamlFile basedir = withObject "BundleConfig" $ \o ->
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

data ListeningPort = LPSecure !HostPreference !Port
                              !F.FilePath !(V.Vector F.FilePath) !F.FilePath
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
                chainCerts <- o .:? "chain-certificates"
                    >>= maybe (return V.empty) (parseYamlFile basedir)
                return $ LPSecure host port cert chainCerts key
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
        }
      where
        getSSL Nothing = V.empty
        getSSL (Just (V04.TLSConfig s ts)) = V.singleton $ LPSecure
            (Warp.getHost s)
            (Warp.getPort s)
            (WarpTLS.certFile ts)
            V.empty
            (WarpTLS.keyFile ts)

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
        , kconfigEnvironment = Map.empty
        , kconfigConnectionTimeBound = V04.fiveMinutes
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
            <*> o .:? "env" .!= Map.empty
            <*> o .:? "connection-time-bound" .!= V04.fiveMinutes

-- | Whether we should force redirect to HTTPS routes.
type RequiresSecure = Bool

data Stanza port = Stanza (StanzaRaw port) RequiresSecure

data StanzaRaw port
    = StanzaStaticFiles !StaticFilesConfig
    | StanzaRedirect !RedirectConfig
    | StanzaWebApp !(WebAppConfig port)
    | StanzaPod !(WebContainerSpec port) ![ ContainerSpec ]
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
            "pod"           -> do
              webcontainer <- parseYamlFile basedir =<< o .: "web-container"
              StanzaPod webcontainer <$> o .: "containers"
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
        Object o -> Object $ HashMap.insert "requires-secure" (toJSON rs) o
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
        Object o -> Object $ HashMap.insert "type" t o
        v -> v

data StaticFilesConfig = StaticFilesConfig
    { sfconfigRoot       :: !F.FilePath
    , sfconfigHosts      :: !(Set Host)
    , sfconfigListings   :: !Bool
    -- FIXME basic auth
    , sfconfigMiddleware :: ![ MiddlewareConfig ]
    , sfconfigTimeout    :: !(Maybe Int)
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
        }

instance ParseYamlFile StaticFilesConfig where
    parseYamlFile basedir = withObject "StaticFilesConfig" $ \o -> StaticFilesConfig
        <$> lookupBase basedir o "root"
        <*> (Set.map CI.mk <$> (o .: "hosts" <|> (Set.singleton <$> (o .: "host"))))
        <*> o .:? "directory-listing" .!= False
        <*> o .:? "middleware" .!= []
        <*> o .:? "connection-time-bound"

instance ToJSON StaticFilesConfig where
    toJSON StaticFilesConfig {..} = object
        [ "root" .= sfconfigRoot
        , "hosts" .= Set.map CI.original sfconfigHosts
        , "directory-listing" .= sfconfigListings
        , "middleware" .= sfconfigMiddleware
        , "connection-time-bound" .= sfconfigTimeout
        ]

data RedirectConfig = RedirectConfig
    { redirconfigHosts   :: !(Set Host)
    , redirconfigStatus  :: !Int
    , redirconfigActions :: !(Vector RedirectAction)
    }
    deriving Show

instance ToCurrent RedirectConfig where
    type Previous RedirectConfig = V04.Redirect
    toCurrent (V04.Redirect from to) = RedirectConfig
        { redirconfigHosts = Set.singleton $ CI.mk from
        , redirconfigStatus = 301
        , redirconfigActions = V.singleton $ RedirectAction SPAny
                             $ RDPrefix False (CI.mk to) Nothing
        }

instance ParseYamlFile RedirectConfig where
    parseYamlFile _ = withObject "RedirectConfig" $ \o -> RedirectConfig
        <$> (Set.map CI.mk <$> ((o .: "hosts" <|> (Set.singleton <$> (o .: "host")))))
        <*> o .:? "status" .!= 303
        <*> o .: "actions"

instance ToJSON RedirectConfig where
    toJSON RedirectConfig {..} = object
        [ "hosts" .= Set.map CI.original redirconfigHosts
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

data SSLConfig
    = SSLFalse
    | SSLTrue
    | SSL !F.FilePath !(V.Vector F.FilePath) !F.FilePath
    deriving (Show, Eq)

instance ParseYamlFile SSLConfig where
    parseYamlFile _ v@(Bool _) =
        withBool "ssl" ( \b ->
            return (if b then SSLTrue else SSLFalse) ) v
    parseYamlFile basedir v =  withObject "ssl" ( \o -> do
             mcert <- lookupBaseMaybe basedir o "certificate"
             mkey <- lookupBaseMaybe basedir o "key"
             case (mcert, mkey) of
                 (Just cert, Just key) -> do
                     chainCerts <- o .:? "chain-certificates"
                         >>= maybe (return V.empty) (parseYamlFile basedir)
                     return $ SSL cert chainCerts key
                 _ -> return SSLFalse
            ) v

instance ToJSON SSLConfig where
    toJSON SSLTrue = Bool True
    toJSON SSLFalse = Bool False
    toJSON (SSL c cc k) = object [ "certificate" .= c
                                 , "chain-certificates" .= cc
                                 , "key" .= k
                                 ]
instance FromJSON SSLConfig where
    parseJSON v@(Bool _) = withBool "ssl" ( \b ->
                    return (if b then SSLTrue else SSLFalse) ) v
    parseJSON v = withObject "ssl" ( \o -> do
                    mcert <- o .:? "certificate"
                    mkey <- o .:? "key"
                    case (mcert, mkey) of
                        (Just cert, Just key) -> do
                            chainCerts <- o .:? "chain-certificates" .!= V.empty
                            return $ SSL cert chainCerts key
                        _ -> return SSLFalse -- fail "Must provide both certificate and key files"
                    ) v

data WebAppConfig port = WebAppConfig
    { waconfigExec        :: !F.FilePath
    , waconfigArgs        :: !(Vector Text)
    , waconfigEnvironment :: !(Map Text Text)
    , waconfigApprootHost :: !Host -- ^ primary host, used for approot
    , waconfigHosts       :: !(Set Host) -- ^ all hosts, not including the approot host
    , waconfigSsl         :: !SSLConfig
    , waconfigPort        :: !port
    , waconfigForwardEnv  :: !(Set Text)
    , waconfigTimeout     :: !(Maybe Int)
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

type ContainerName = Text
type ImageName     = Text
type Repository    = Text
type MountName     = Text

data VolumeMount =
  VolumeMount { vmMountPath :: FilePath
              , vmName      :: MountName
              } deriving (Show)

data ContainerSpec =
  ContainerSpec { csName       :: !ContainerName
                , csImage      :: !ImageName
                , csRepository :: !(Maybe Repository)
                , csEnv        :: !(Map Text Text)
                , csForwardEnv :: !(Set Text)
                , csMounts     :: Vector VolumeMount
                , csLinks      :: Vector ContainerName
                } deriving (Show)

data WebContainerSpec port =
  WebContainerSpec { wcsName        :: !ContainerName
                   , wcsApprootHost :: !Host
                   , wcsHosts       :: !(Set Host) -- ^ all hosts, not including the approot host
                   , wcsSSLConfig   :: !SSLConfig
                   , wcsTimeout     :: !(Maybe Int)
                   , wcsPort        :: !port
                   } deriving (Show)

isWebContainerSpec :: ContainerSpec -> Bool
isWebContainerSpec cs = csName cs == "web"

instance FromJSON VolumeMount where
  parseJSON = withObject "VolumeMount" $ \o ->
    VolumeMount <$> o .: "mountPath"
                <*> o .: "name"

instance FromJSON ContainerSpec where
  parseJSON = withObject "ContainerSpec" $ \o ->
    ContainerSpec <$> o .: "name"
                  <*> o .: "image"
                  <*> o .:? "repository"
                  <*> o .:? "env" .!= Map.empty
                  <*> o .:? "forward-env" .!= Set.empty
                  <*> o .:? "volumeMounts" .!= V.empty
                  <*> o .:? "links" .!= V.empty

instance ParseYamlFile (WebContainerSpec ()) where
  parseYamlFile _ = withObject "WebContainerSpec" $ \o -> do
    (ahost, hosts) <-
      (do
          h <- o .: "host"
          return (CI.mk h, Set.empty)) <|>
      (do
          hs <- o .: "hosts"
          case hs of
            [] -> fail "Must provide at least one host"
            h:hs' -> return (CI.mk h, Set.fromList $ map CI.mk hs'))

    WebContainerSpec <$> o .: "name"
                     <*> pure ahost
                     <*> pure hosts
                     <*> o .: "ssl"
                     <*> o .:? "connection-time-bound"
                     <*> pure ()

data AppInput = AIBundle !FilePath !EpochTime
              | AIPod !FilePath !EpochTime
              | AIData !BundleConfig

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
