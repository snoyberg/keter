{-# LANGUAGE OverloadedStrings #-}
module Keter.SSL
    ( TLSConfig (..)
    ) where

import Prelude hiding (FilePath)
import Data.Yaml ((.:), (.:?), (.!=))
import Data.Aeson (withObject)
import Control.Applicative ((<$>))
import Data.String (fromString)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Data.Yaml.FilePath

data TLSConfig = TLSConfig !Warp.Settings !WarpTLS.TLSSettings

instance ParseYamlFile TLSConfig where
    parseYamlFile basedir = withObject "TLSConfig" $ \o -> do
        cert <- getFilePath basedir o "certificate"
        key <- getFilePath basedir o "key"
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
