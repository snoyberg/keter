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
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid (mempty)

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

data KeterConfig = KeterConfig
    { kconfigDir :: F.FilePath
    , kconfigHostMan :: V04.PortSettings
    , kconfigHost :: HostPreference
    , kconfigPort :: Port
    , kconfigSsl :: Maybe V04.TLSConfig
    , kconfigSetuid :: Maybe Text
    , kconfigBuiltinStanzas :: !(V.Vector Stanza)
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
        (V.fromList $ map StanzaReverseProxy $ Set.toList g)
        h

instance Default KeterConfig where
    def = KeterConfig
        { kconfigDir = "."
        , kconfigHostMan = def
        , kconfigHost = "*"
        , kconfigPort = 80
        , kconfigSsl = Nothing
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
            <*> (fmap fromString <$> o .:? "host") .!= kconfigHost def
            <*> o .:? "port" .!= kconfigPort def
            <*> (o .:? "ssl" >>= maybe (return Nothing) (fmap Just . parseYamlFile basedir))
            <*> o .:? "setuid"
            <*> return V.empty
            <*> o .:? "ip-from-header" .!= False

data Stanza = StanzaStaticFiles StaticFilesConfig
            | StanzaRedirect RedirectConfig
            | StanzaWebApp WebAppConfig
            | StanzaReverseProxy V04.ReverseProxyConfig

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
        , sfconfigListings = False
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
        , redirconfigStatus = 303
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
