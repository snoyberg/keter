{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provides logging, versioning and some type aliases
module Keter.Common where

import Control.Exception (Exception, SomeException)
import Data.Aeson
       ( FromJSON
       , Object
       , ToJSON
       , Value(Bool)
       , object
       , withBool
       , withObject
       , (.!=)
       , (.:?)
       , (.=)
       )
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, original)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Yaml qualified
import Keter.Yaml.FilePath
import Language.Haskell.TH.Syntax qualified as TH
import Network.Socket (AddrInfo, SockAddr)
import Network.Wai qualified as Wai
import System.Exit (ExitCode)
import System.FilePath (FilePath, takeBaseName)

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

data Plugin = Plugin
    { pluginGetEnv :: Appname -> Object -> IO [(Text, Text)]
    }

type Plugins = [Plugin]

-- | Used for versioning data types.
class ToCurrent a where
    type Previous a
    toCurrent :: Previous a -> a
instance ToCurrent a => ToCurrent (Maybe a) where
    type Previous (Maybe a) = Maybe (Previous a)
    toCurrent = fmap toCurrent

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = CI Text

type HostBS = CI ByteString

getAppname :: FilePath -> Text
getAppname = pack . takeBaseName

data KeterException = CannotParsePostgres FilePath
                    | ExitCodeFailure FilePath ExitCode
                    | NoPortsAvailable
                    | InvalidConfigFile Data.Yaml.ParseException
                    | InvalidKeterConfigFile !FilePath !Data.Yaml.ParseException
                    | CannotReserveHosts !AppId !(Map Host AppId)
                    | FileNotExecutable !FilePath
                    | ExecutableNotFound !FilePath
                    | EnsureAliveShouldBeBiggerThenZero { keterExceptionGot:: !Int }
    deriving (Show, Typeable)
instance Exception KeterException

data AppId = AIBuiltin | AINamed !Appname
    deriving (Eq, Ord)
instance Show AppId where
    show AIBuiltin   = "/builtin/"
    show (AINamed t) = unpack t

data SSLConfig
    = SSLFalse
    | SSLTrue
    | SSL !FilePath !(Vector FilePath) !FilePath
    deriving (Show, Eq, Ord)

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
