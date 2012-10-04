{-# LANGUAGE OverloadedStrings #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , PortLookup
    , HostList
    , reverseProxySsl
    , setDir
    , TLSConfig
    , TLSConfigNoDir
    ) where

import Keter.Prelude ((++))
import Prelude hiding ((++), FilePath)
import Data.Conduit
import Data.Conduit.Network
import Data.ByteString (ByteString)
import Keter.PortManager (Port)
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString, toLazyByteString)
import Data.Monoid (mconcat)
import Keter.SSL
import Network.HTTP.ReverseProxy (rawProxyTo, ProxyDest (ProxyDest))
import Control.Applicative ((<$>))

-- | Mapping from virtual hostname to port number.
type PortLookup = ByteString -> IO (Maybe Port)

type HostList = IO [ByteString]

reverseProxy :: ServerSettings IO -> PortLookup -> HostList -> IO ()
reverseProxy settings x = runTCPServer settings . withClient x

reverseProxySsl :: TLSConfig -> PortLookup -> HostList -> IO ()
reverseProxySsl settings x = runTCPServerTLS settings . withClient x

withClient :: PortLookup
           -> HostList
           -> Application IO
withClient portLookup hostList =
    rawProxyTo getDest
  where
    getDest headers = do
        mport <- maybe (return Nothing) portLookup $ lookup "host" headers
        case mport of
            Nothing -> Left . srcToApp . toResponse <$> hostList
            Just port -> return $ Right $ ProxyDest "127.0.0.1" port

srcToApp :: Monad m => Source m ByteString -> Application m
srcToApp src appdata = src $$ appSink appdata

toResponse :: Monad m => [ByteString] -> Source m ByteString
toResponse hosts =
    mapM_ yield $ L.toChunks $ toLazyByteString $ front ++ mconcat (map go hosts) ++ end
  where
    front = fromByteString "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You may access the following sites:</p><ul>"
    end = fromByteString "</ul></body></html>"
    go host = fromByteString "<li><a href=\"http://" ++ fromByteString host ++ fromByteString "/\">" ++
              fromByteString host ++ fromByteString "</a></li>"
