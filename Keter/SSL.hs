{-# LANGUAGE OverloadedStrings #-}
module Keter.SSL
    ( TLSConfig (..)
    , TLSConfigNoDir
    , setDir
    , runTCPServerTLS
    ) where

import Prelude hiding (FilePath)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?), (.!=), Value (Object))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.String (fromString)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import Data.Conduit.Network.TLS

setDir :: FilePath -> TLSConfigNoDir -> TLSConfig
setDir dir (TLSConfigNoDir tls) = tls
    { tlsCertificate = dir </> tlsCertificate tls
    , tlsKey = dir </> tlsKey tls
    }

newtype TLSConfigNoDir = TLSConfigNoDir TLSConfig

instance FromJSON TLSConfigNoDir where
    parseJSON (Object o) = fmap TLSConfigNoDir $ tlsConfig
        <$> (fmap fromString <$> o .:? "host") .!= "*"
        <*> o .:? "port" .!= 443
        <*> (fromString <$> o .: "certificate")
        <*> (fromString <$> o .: "key")
    parseJSON _ = mzero
