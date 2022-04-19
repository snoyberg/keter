{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE CPP #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , makeSettings
    , ProxySettings(..)
    , TLSConfig (..)
    ) where

import qualified Network.HTTP.Conduit      as HTTP
import qualified Data.CaseInsensitive      as CI
import qualified Keter.HostManager         as HostMan
import           Blaze.ByteString.Builder          (copyByteString, toByteString)
import           Blaze.ByteString.Builder.Html.Word(fromHtmlEscapedByteString)
import           Control.Applicative               ((<$>), (<|>))
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString                   as S
import qualified Data.ByteString.Char8             as S8
import qualified Data.CaseInsensitive              as CI
#if MIN_VERSION_http_reverse_proxy(0,6,0)
import           Network.Wai.Middleware.Gzip       (def)
#else
import           Data.Default                      (Default (..))
#endif
import           Data.Monoid                       (mappend, mempty)
import           Data.Text.Encoding                (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error          (lenientDecode)
import qualified Data.Vector                       as V
import           Keter.Types
import           Keter.Types.Middleware
import           Network.HTTP.Conduit              (Manager)

#if MIN_VERSION_http_reverse_proxy(0,4,2)
import           Network.HTTP.ReverseProxy         (defaultLocalWaiProxySettings)
#endif

#if MIN_VERSION_http_reverse_proxy(0,6,0)
import           Network.HTTP.ReverseProxy         (defaultWaiProxySettings)
#endif

import           Network.HTTP.ReverseProxy         (ProxyDest (ProxyDest),
                                                    SetIpHeader (..),
                                                    WaiProxyResponse (..),
                                                    LocalWaiProxySettings,
                                                    setLpsTimeBound,
                                                    waiProxyToSettings,
                                                    wpsSetIpHeader,
                                                    wpsGetDest)
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
import qualified Network.TLS.SessionManager        as TLSSession
import           Network.Wai.Middleware.Gzip       (gzip, GzipSettings(..), GzipFiles(..))
import           Prelude                           hiding (FilePath, (++))
import           WaiAppStatic.Listing              (defaultListing)
import qualified Network.TLS as TLS
import qualified System.Directory as Dir

#if !MIN_VERSION_http_reverse_proxy(0,6,0)
defaultWaiProxySettings = def
#endif

#if !MIN_VERSION_http_reverse_proxy(0,4,2)
defaultLocalWaiProxySettings = def
#endif


data ProxySettings = MkProxySettings
  { -- | Mapping from virtual hostname to port number.
    psHostLookup  :: ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))
  , psManager     :: !Manager
  , psConfig      :: !KeterConfig
  , psUnkownHost  :: ByteString -> ByteString
  , psMissingHost :: ByteString
  }

makeSettings :: KeterConfig -> HostMan.HostManager -> IO ProxySettings
makeSettings psConfig@KeterConfig {..} hostman = do
    psManager <- HTTP.newManager HTTP.tlsManagerSettings
    psMissingHost <- case kconfigMissingHostResponse of
      Nothing -> pure defaultMissingHostBody
      Just x -> taggedReadFile "unknown-host-response-file" x
    psUnkownHost <- case kconfigUnknownHostResponse  of
                Nothing -> pure defaultUnknownHostBody
                Just x -> const <$> taggedReadFile "missing-host-response-file" x
    pure $ MkProxySettings{..}
    where
        psHostLookup = HostMan.lookupAction hostman . CI.mk

taggedReadFile :: String -> FilePath -> IO ByteString
taggedReadFile tag file = do
        isExist <- Dir.doesFileExist file
        if isExist then S.readFile file else do
          wd <- Dir.getCurrentDirectory
          error $ "could not find " <> tag <> " on path '" <> file <> "' with working dir '" <> wd <> "'"

reverseProxy :: ProxySettings -> ListeningPort -> IO ()
reverseProxy settings listener =
    run $ gzip def{gzipFiles = GzipPreCompressed GzipIgnore} $ withClient isSecure settings
  where
    warp host port = Warp.setHost host $ Warp.setPort port Warp.defaultSettings
    (run, isSecure) =
        case listener of
            LPInsecure host port -> (Warp.runSettings (warp host port), False)
            LPSecure host port cert chainCerts key session -> (WarpTLS.runTLS
                (connectClientCertificates (psHostLookup settings) session $ WarpTLS.tlsSettingsChain
                    cert
                    (V.toList chainCerts)
                    key)
                (warp host port), True)

connectClientCertificates :: (ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))) -> Bool -> WarpTLS.TLSSettings -> WarpTLS.TLSSettings
connectClientCertificates hl session s =
    let
        newHooks@TLS.ServerHooks{..} = WarpTLS.tlsServerHooks s
        -- todo: add nested lookup
        newOnServerNameIndication (Just n) =
             maybe mempty snd <$> hl (S8.pack n)
        newOnServerNameIndication Nothing =
             return mempty -- we could return default certificate here
    in
        s { WarpTLS.tlsServerHooks = newHooks{TLS.onServerNameIndication = newOnServerNameIndication}
          , WarpTLS.tlsSessionManagerConfig = if session then (Just TLSSession.defaultConfig) else Nothing }

withClient :: Bool -- ^ is secure?
           -> ProxySettings
           -> Wai.Application
withClient isSecure MkProxySettings {..} =
    waiProxyToSettings
       (error "First argument to waiProxyToSettings forced, even thought wpsGetDest provided")
       defaultWaiProxySettings
        { wpsSetIpHeader =
            if useHeader
                then SIHFromHeader
                else SIHFromSocket
        ,  wpsGetDest = Just getDest
        } psManager
  where
    useHeader :: Bool
    useHeader = kconfigIpFromHeader psConfig

    -- calculate the number of microseconds since the
    -- configuration option is in milliseconds
    bound :: Int
    bound = kconfigConnectionTimeBound psConfig * 1000
    protocol
        | isSecure = "https"
        | otherwise = "http"

    -- FIXME This is a workaround for
    -- https://github.com/snoyberg/keter/issues/29. After some research, it
    -- seems like Warp is behaving properly here. I'm still not certain why the
    -- http call (from http-conduit) inside waiProxyToSettings could ever block
    -- infinitely without the server it's connecting to going down, so that
    -- requires more research. Meanwhile, this prevents the file descriptor
    -- leak from occurring.

    addjustGlobalBound :: Maybe Int -> LocalWaiProxySettings
    addjustGlobalBound to = go `setLpsTimeBound` defaultLocalWaiProxySettings
      where
        go = case to <|> Just bound of
               Just x | x > 0 -> Just x
               _              -> Nothing

    getDest :: Wai.Request -> IO (LocalWaiProxySettings, WaiProxyResponse)
    getDest req =
        case Wai.requestHeaderHost req of
            Nothing -> do
              return (defaultLocalWaiProxySettings, WPRResponse $ missingHostResponse psMissingHost)
            Just host -> processHost req host

    processHost :: Wai.Request -> S.ByteString -> IO (LocalWaiProxySettings, WaiProxyResponse)
    processHost req host = do
        -- Perform two levels of lookup. First: look up the entire host. If
        -- that fails, try stripping off any port number and try again.
        mport <- liftIO $ do
            mport1 <- psHostLookup host
            case mport1 of
                Just _ -> return mport1
                Nothing -> do
                    let host' = S.takeWhile (/= 58) host
                    if host' == host
                        then return Nothing
                        else psHostLookup host'
        case mport of
            Nothing -> do
              return (defaultLocalWaiProxySettings, WPRResponse $ unknownHostResponse host (psUnkownHost host))
            Just ((action, requiresSecure), _)
                | requiresSecure && not isSecure -> performHttpsRedirect host req
                | otherwise -> performAction req action

    performHttpsRedirect host =
        return . (addjustGlobalBound Nothing,) . WPRResponse . redirectApp config
      where
        host' = CI.mk $ decodeUtf8With lenientDecode host
        config = RedirectConfig
            { redirconfigHosts = mempty
            , redirconfigStatus = 301
            , redirconfigActions = V.singleton $ RedirectAction SPAny
                                 $ RDPrefix True host' Nothing
            , redirconfigSsl = SSLTrue
            }

    performAction req (PAPort port tbound) =
        return (addjustGlobalBound tbound, WPRModifiedRequest req' $ ProxyDest "127.0.0.1" port)
      where
        req' = req
            { Wai.requestHeaders = ("X-Forwarded-Proto", protocol)
                                 : Wai.requestHeaders req
            }
    performAction _ (PAStatic StaticFilesConfig {..}) =
        return (addjustGlobalBound sfconfigTimeout, WPRApplication $ processMiddleware sfconfigMiddleware $ staticApp (defaultFileServerSettings sfconfigRoot)
            { ssListing =
                if sfconfigListings
                    then Just defaultListing
                    else Nothing
            })
    performAction req (PARedirect config) = return (addjustGlobalBound Nothing, WPRResponse $ redirectApp config req)
    performAction _ (PAReverseProxy config rpconfigMiddleware tbound) =
       return (addjustGlobalBound tbound, WPRApplication
                $ processMiddleware rpconfigMiddleware
                $ Rewrite.simpleReverseProxy psManager config
              )

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

defaultMissingHostBody :: ByteString
defaultMissingHostBody = "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"

missingHostResponse :: ByteString -> Wai.Response
missingHostResponse missingHost = Wai.responseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8")]
    $ copyByteString missingHost

defaultUnknownHostBody :: ByteString -> ByteString
defaultUnknownHostBody host =
  "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
  <> escapeHtml host <> "</code>, is not recognized.</p></body></html>"

unknownHostResponse :: ByteString -> ByteString -> Wai.Response
unknownHostResponse host body = Wai.responseBuilder
    status200
    [("Content-Type", "text/html; charset=utf-8"), ("X-Forwarded-Host", host)]
    (copyByteString body)

escapeHtml :: ByteString -> ByteString
escapeHtml = toByteString . fromHtmlEscapedByteString
