{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , HostLookup
    , TLSConfig (..)
    ) where

import           Blaze.ByteString.Builder          (copyByteString)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString                   as S
import qualified Data.ByteString.Char8             as S8
import qualified Data.CaseInsensitive              as CI
import           Data.Default
import           Data.Monoid                       (mappend, mempty)
import           Data.Text.Encoding                (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error          (lenientDecode)
import qualified Data.Vector                       as V
import qualified Filesystem.Path.CurrentOS         as F
import           Keter.Types
import           Network.HTTP.Conduit              (Manager)
import           Network.HTTP.ReverseProxy         (ProxyDest (ProxyDest),
                                                    SetIpHeader (..),
                                                    WaiProxyResponse (..),
                                                    waiProxyToSettings,
                                                    wpsSetIpHeader)
import qualified Network.HTTP.ReverseProxy.Rewrite as Rewrite
import           Network.HTTP.Types                (mkStatus, status200,
                                                    status301, status302,
                                                    status303, status307,
                                                    status404, status500)
import qualified Network.Wai                       as Wai
import           Network.Wai.Application.Static    (defaultFileServerSettings,
                                                    ssListing, staticApp)
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import           Network.Wai.Middleware.Gzip       (gzip, def)
import           Prelude                           hiding (FilePath, (++))
import           WaiAppStatic.Listing              (defaultListing)
import System.Timeout.Lifted (timeout)

-- | Mapping from virtual hostname to port number.
type HostLookup = ByteString -> IO (Maybe ProxyAction)

reverseProxy :: Bool
             -> Manager -> HostLookup -> ListeningPort -> IO ()
reverseProxy useHeader manager hostLookup listener =
    run $ gzip def $ withClient isSecure useHeader manager hostLookup
  where
    warp host port = Warp.setHost host $ Warp.setPort port Warp.defaultSettings
    (run, isSecure) =
        case listener of
            LPInsecure host port -> (Warp.runSettings (warp host port), False)
            LPSecure host port cert key -> (WarpTLS.runTLS
                (WarpTLS.tlsSettings (F.encodeString cert) (F.encodeString key))
                (warp host port), True)

withClient :: Bool -- ^ is secure?
           -> Bool -- ^ use incoming request header for IP address
           -> Manager
           -> HostLookup
           -> Wai.Application
withClient isSecure useHeader manager portLookup req0 sendResponse =
    timeBound (5 * 60 * 1000 * 1000) (waiProxyToSettings getDest def
        { wpsSetIpHeader =
            if useHeader
                then SIHFromHeader
                else SIHFromSocket
        } manager req0 sendResponse)
  where
    protocol
        | isSecure = "https"
        | otherwise = "http"

    -- FIXME This is a temporary workaround for
    -- https://github.com/snoyberg/keter/issues/29. After some research, it
    -- seems like Warp is behaving properly here. I'm still not certain why the
    -- http call (from http-conduit) inside waiProxyToSettings could ever block
    -- infinitely without the server it's connecting to going down, so that
    -- requires more research. Meanwhile, this prevents the file descriptor
    -- leak from occurring.
    timeBound us f = do
        mres <- timeout us f
        case mres of
            Just res -> return res
            Nothing -> sendResponse $ Wai.responseLBS status500 [] "timeBound"

    getDest :: Wai.Request -> IO WaiProxyResponse
    getDest req =
        case Wai.requestHeaderHost req of
            Nothing -> return $ WPRResponse missingHostResponse
            Just host -> processHost req host

    processHost :: Wai.Request -> S.ByteString -> IO WaiProxyResponse
    processHost req host = do
        -- Take the host name up until the port number.
        mport <- liftIO $ portLookup $ S.takeWhile (/= 58) host
        case mport of
            Nothing -> return $ WPRResponse $ unknownHostResponse host
            Just (action, requiresSecure)
                | requiresSecure && not isSecure -> performHttpsRedirect host req
                | otherwise -> performAction req action

    performHttpsRedirect host =
        return . WPRResponse . redirectApp config
      where
        host' = CI.mk $ decodeUtf8With lenientDecode host
        config = RedirectConfig
            { redirconfigHosts = mempty
            , redirconfigStatus = 301
            , redirconfigActions = V.singleton $ RedirectAction SPAny
                                 $ RDPrefix True host' Nothing
            }

    performAction req (PAPort port) =
        return $ WPRModifiedRequest req' $ ProxyDest "127.0.0.1" port
      where
        req' = req
            { Wai.requestHeaders = ("X-Forwarded-Proto", protocol)
                                 : Wai.requestHeaders req
            }
    performAction _ (PAStatic StaticFilesConfig {..}) = do
        return $ WPRApplication $ staticApp (defaultFileServerSettings sfconfigRoot)
            { ssListing =
                if sfconfigListings
                    then Just defaultListing
                    else Nothing
            }
    performAction req (PARedirect config) = return $ WPRResponse $ redirectApp config req
    performAction _ (PAReverseProxy config) = return $ WPRApplication $ Rewrite.simpleReverseProxy manager config

redirectApp :: RedirectConfig -> Wai.Request -> Wai.Response
redirectApp RedirectConfig {..} req =
    V.foldr checkAction noAction redirconfigActions
  where
    checkAction (RedirectAction SPAny dest) _ = sendTo $ mkUrl dest
    checkAction (RedirectAction (SPSpecific path) dest) other
        | encodeUtf8 path == Wai.rawPathInfo req = sendTo $ mkUrl dest
        | otherwise = other

    noAction = Wai.responseBuilder
        status404
        [("Content-Type", "text/plain")]
        (copyByteString "File not found")

    sendTo url = Wai.responseBuilder
        status
        [("Location", url)]
        (copyByteString url)

    status =
        case redirconfigStatus of
            301 -> status301
            302 -> status302
            303 -> status303
            307 -> status307
            i   -> mkStatus i $ S8.pack $ show i

    mkUrl (RDUrl url) = encodeUtf8 url
    mkUrl (RDPrefix isSecure host mport) = S.concat
        [ if isSecure then "https://" else "http://"
        , encodeUtf8 $ CI.original host
        , case mport of
            Nothing -> ""
            Just port
                | isSecure && port == 443 -> ""
                | not isSecure && port == 80 -> ""
                | otherwise -> S8.pack $ ':' : show port
        , Wai.rawPathInfo req
        , Wai.rawQueryString req
        ]

missingHostResponse :: Wai.Response
missingHostResponse = Wai.responseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    $ copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"

unknownHostResponse :: ByteString -> Wai.Response
unknownHostResponse host = Wai.responseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    (copyByteString "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
     `mappend` copyByteString host
     `mappend` copyByteString "</code>, is not recognized.</p></body></html>")
