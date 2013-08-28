{-# LANGUAGE OverloadedStrings #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , PortLookup
    , reverseProxySsl
    , setDir
    , TLSConfigNoDir
    ) where

import Prelude hiding ((++), FilePath)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Keter.PortManager (PortEntry (..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Keter.SSL
import Network.HTTP.ReverseProxy (waiProxyToSettings, wpsSetIpHeader, SetIpHeader (..), ProxyDest (ProxyDest), WaiProxyResponse (..))
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Middleware.Gzip
import qualified Network.Wai as Wai
import Network.HTTP.Types (status301, status200)
import qualified Keter.ReverseProxy as ReverseProxy
import Network.HTTP.Conduit (Manager)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid (mappend)

-- | Mapping from virtual hostname to port number.
type PortLookup = ByteString -> IO (Maybe PortEntry)

reverseProxy :: Bool -> [Wai.Middleware] -> Manager -> Warp.Settings -> PortLookup -> IO ()
reverseProxy useHeader wares manager settings =
        Warp.runSettings settings . insertMiddleware wares . withClient useHeader manager

reverseProxySsl :: Bool -> [Wai.Middleware] -> Manager -> WarpTLS.TLSSettings -> Warp.Settings -> PortLookup -> IO ()
reverseProxySsl useHeader wares manager tsettings settings =
        WarpTLS.runTLS tsettings settings . insertMiddleware wares . withClient useHeader manager

insertMiddleware :: [Wai.Middleware] -> Wai.Middleware
insertMiddleware = foldl (.) id

withClient :: Bool -- ^ use incoming request header for IP address
           -> Manager
           -> PortLookup
           -> Wai.Application
withClient useHeader manager portLookup =
    waiProxyToSettings getDest def
        { wpsSetIpHeader =
            if useHeader
                then SIHFromHeader
                else SIHFromSocket
        } manager
  where
    getDest req = do
        mport <- liftIO $ maybe (return Nothing) portLookup mhost
        case mport of
            Nothing -> return $ WPRResponse $ toResponse mhost
            Just (PEPort port) -> return $ WPRProxyDest $ ProxyDest "127.0.0.1" port
            Just (PEStatic root) -> fmap WPRResponse $ staticApp (defaultFileServerSettings root) req
            Just (PERedirect host) -> return $ WPRResponse $ redirectApp host req
            Just (PEReverseProxy rpentry) -> fmap WPRResponse $ ReverseProxy.simpleReverseProxy rpentry req
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

toResponse :: Maybe ByteString -> Wai.Response
toResponse mhost = Wai.ResponseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    $ case mhost of
        Nothing -> copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"
        Just host ->
            copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
            `mappend` copyByteString host
            `mappend` copyByteString "</code>, is not recognized.</p></body></html>"
