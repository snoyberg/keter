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
import Keter.PortManager (PortEntry (..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString, toLazyByteString)
import Data.Monoid (mconcat)
import Keter.SSL
import Network.HTTP.ReverseProxy (rawProxyTo, ProxyDest (ProxyDest), waiToRaw)
import Control.Applicative ((<$>))
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status301)

-- | Mapping from virtual hostname to port number.
type PortLookup = ByteString -> IO (Maybe PortEntry)

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
            Just (PEPort port) -> return $ Right $ ProxyDest "127.0.0.1" port
            Just (PEStatic root) -> return $ Left $ waiToRaw $ staticApp $ defaultFileServerSettings root
            Just (PERedirect host) -> return $ Left $ waiToRaw $ redirectApp host

redirectApp :: ByteString -> Wai.Application
redirectApp host req = return $ Wai.responseLBS
    status301
    [("Location", dest)]
    (L.fromChunks [dest])
  where
    dest = S.concat
        [ "http://"
        , host
        , Wai.rawPathInfo req
        , Wai.rawQueryString req
        ]

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
