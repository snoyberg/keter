{-# LANGUAGE OverloadedStrings #-}
module Keter.SSL
    ( TLSConfigNoDir
    , setDir
    ) where

import Prelude hiding (FilePath)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?), (.!=), Value (Object))
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.String (fromString)
import System.FilePath ((</>))
import Filesystem.Path.CurrentOS (FilePath, encodeString)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

setDir :: FilePath -> TLSConfigNoDir -> (Warp.Settings, WarpTLS.TLSSettings)
setDir dir' (TLSConfigNoDir s ts') =
    (s, ts)
  where
    dir = encodeString dir'
    ts = ts'
        { WarpTLS.certFile = dir </> WarpTLS.certFile ts'
        , WarpTLS.keyFile = dir </> WarpTLS.keyFile ts'
        }

data TLSConfigNoDir = TLSConfigNoDir !Warp.Settings !WarpTLS.TLSSettings

instance FromJSON TLSConfigNoDir where
    parseJSON (Object o) = do
        cert <- o .: "certificate"
        key <- o .: "key"
        host <- (fmap fromString <$> o .:? "host") .!= "*"
        port <- o .:? "port" .!= 443
        return $ TLSConfigNoDir
            Warp.defaultSettings
                { Warp.settingsHost = host
                , Warp.settingsPort = port
                }
            WarpTLS.defaultTlsSettings
                { WarpTLS.certFile = cert
                , WarpTLS.keyFile = key
                }
    parseJSON _ = mzero
