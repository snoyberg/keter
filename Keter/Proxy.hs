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
import           Data.Default
import           Data.Monoid                       (mappend)
import           Data.Text.Encoding                (encodeUtf8)
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
                                                    status404)
import qualified Network.Wai                       as Wai
import           Network.Wai.Application.Static    (defaultFileServerSettings,
                                                    ssListing, staticApp)
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as WarpTLS
import           Prelude                           hiding (FilePath, (++))
import           WaiAppStatic.Listing              (defaultListing)

-- | Mapping from virtual hostname to port number.
type HostLookup = ByteString -> IO (Maybe ProxyAction)

reverseProxy :: Bool -> Manager -> HostLookup -> ListeningPort -> IO ()
reverseProxy useHeader manager hostLookup listener =
    run $ withClient useHeader manager hostLookup
  where
    warp host port = Warp.defaultSettings
        { Warp.settingsHost = host
        , Warp.settingsPort = port
        }
    run =
        case listener of
            LPInsecure host port -> Warp.runSettings (warp host port)
            LPSecure host port cert key -> WarpTLS.runTLS
                (WarpTLS.tlsSettings (F.encodeString cert) (F.encodeString key))
                (warp host port)

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
            Just action -> performAction req action

    performAction _ (PAPort port) =
        return $ WPRProxyDest $ ProxyDest "127.0.0.1" port
    performAction req (PAStatic StaticFilesConfig {..}) =
        fmap WPRResponse $ staticApp (defaultFileServerSettings sfconfigRoot)
            { ssListing =
                if sfconfigListings
                    then Just defaultListing
                    else Nothing
            } req
    performAction req (PARedirect config) = return $ WPRResponse $ redirectApp config req
    performAction req (PAReverseProxy config) = fmap WPRResponse $ Rewrite.simpleReverseProxy manager config req

redirectApp :: RedirectConfig -> Wai.Request -> Wai.Response
redirectApp RedirectConfig {..} req =
    V.foldr checkAction noAction redirconfigActions
  where
    checkAction (RedirectAction SPAny dest) _ = sendTo $ mkUrl dest
    checkAction (RedirectAction (SPSpecific path) dest) other
        | encodeUtf8 path == Wai.rawPathInfo req = sendTo $ mkUrl dest
        | otherwise = other

    noAction = Wai.ResponseBuilder
        status404
        [("Content-Type", "text/plain")]
        (copyByteString "File not found")

    sendTo url = Wai.ResponseBuilder
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
    mkUrl (RDPrefix isSecure host port) = S.concat
        [ if isSecure then "https://" else "http://"
        , encodeUtf8 host
        , if (isSecure && port == 443) || (not isSecure && port == 80)
            then ""
            else S8.pack $ ':' : show port
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
