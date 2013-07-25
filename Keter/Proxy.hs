{-# LANGUAGE OverloadedStrings #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , HostLookup
    , reverseProxySsl
    , TLSConfig (..)
    ) where

import Prelude hiding ((++), FilePath)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network.HTTP.ReverseProxy (waiProxyToSettings, wpsSetIpHeader, SetIpHeader (..), ProxyDest (ProxyDest), WaiProxyResponse (..))
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status301, status200)
import qualified Network.HTTP.ReverseProxy.Rewrite as Rewrite
import Network.HTTP.Conduit (Manager)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid (mappend)
import Data.Default
import Keter.Types

-- | Mapping from virtual hostname to port number.
type HostLookup = ByteString -> IO (Maybe ProxyAction)

reverseProxy :: Bool -> Manager -> Warp.Settings -> HostLookup -> IO ()
reverseProxy useHeader manager settings = Warp.runSettings settings . withClient useHeader manager

reverseProxySsl :: Bool -> Manager -> WarpTLS.TLSSettings -> Warp.Settings -> HostLookup -> IO ()
reverseProxySsl useHeader manager tsettings settings = WarpTLS.runTLS tsettings settings . withClient useHeader manager

withClient :: Bool -- ^ use incoming request header for IP address
           -> Manager
           -> HostLookup
           -> Wai.Application
withClient useHeader manager portLookup =
    waiProxyToSettings getDest def
        { wpsSetIpHeader =
            if useHeader
                then SIHFromHeader
                else SIHFromSocket
        } manager
  where
    getDest req =
        case lookup "host" $ Wai.requestHeaders req of
            Nothing -> return $ WPRResponse missingHostResponse
            Just host -> processHost req host

    processHost req host = do
        mport <- liftIO $ portLookup host
        case mport of
            Nothing -> return $ WPRResponse $ unknownHostResponse host
            {- FIXME
            Just (PEPort port) -> return $ WPRProxyDest $ ProxyDest "127.0.0.1" port
            Just (PEStatic root) -> fmap WPRResponse $ staticApp (defaultFileServerSettings root) req
            Just (PERedirect dest) -> return $ WPRResponse $ redirectApp dest req
            Just (PEReverseProxy rpentry) -> fmap WPRResponse $ Rewrite.simpleReverseProxy rpentry req
            -}
      where
        mhost = lookup "host" $ Wai.requestHeaders req

redirectApp :: ByteString -> Wai.Request -> Wai.Response
redirectApp host req = Wai.responseLBS
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

missingHostResponse :: Wai.Response
missingHostResponse = Wai.ResponseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    $ copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"

unknownHostResponse :: ByteString -> Wai.Response
unknownHostResponse host = Wai.ResponseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
     `mappend` copyByteString host
     `mappend` copyByteString "</code>, is not recognized.</p></body></html>")
