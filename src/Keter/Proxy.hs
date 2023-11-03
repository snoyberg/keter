{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.Functor ((<&>))
import qualified Keter.HostManager         as HostMan
import           Blaze.ByteString.Builder          (copyByteString, toByteString)
import           Blaze.ByteString.Builder.Html.Word(fromHtmlEscapedByteString)
import           Control.Applicative               ((<$>), (<|>))
import           Control.Monad.Reader              (ask)
import           Control.Monad.IO.Unlift           (withRunInIO)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString                   as S
import qualified Data.ByteString.Char8             as S8
#if MIN_VERSION_http_reverse_proxy(0,6,0)
import           Network.Wai.Middleware.Gzip       (def)
#endif
import           Data.Monoid                       (mappend, mempty)
import           Data.Proxy
import           Data.Tagged
import           Data.Text                         as T (pack, unwords)
import           Data.Text.Encoding                (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error          (lenientDecode)
import qualified Data.Vector                       as V
import           GHC.Exts (fromString)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Keter.Config
import           Keter.Config.Middleware
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
                                                    wpsOnExc,
                                                    wpsGetDest)
import qualified Keter.Rewrite as Rewrite
import           Data.ByteString            (ByteString)
import Keter.Common
import           System.FilePath            (FilePath)
import           Control.Monad.Logger
import           Control.Exception          (SomeException)
import           Network.HTTP.Types                (mkStatus,
                                                    status301, status302,
                                                    status303, status307,
                                                    status404, status502)
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
import Keter.Context

#if !MIN_VERSION_http_reverse_proxy(0,6,0)
defaultWaiProxySettings = def
#endif

#if !MIN_VERSION_http_reverse_proxy(0,4,2)
defaultLocalWaiProxySettings = def
#endif


data ProxySettings = MkProxySettings
  { -- | Mapping from virtual hostname to port number.
    psHostLookup     :: ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))
  , psManager        :: !Manager
  , psIpFromHeader   :: Bool
  , psConnectionTimeBound :: Int
  , psUnknownHost    :: ByteString -> ByteString
  , psMissingHost    :: ByteString
  , psProxyException :: ByteString
  }

makeSettings :: HostMan.HostManager -> KeterM KeterConfig ProxySettings
makeSettings hostman = do
    KeterConfig{..} <- ask
    psManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    psMissingHost <- taggedReadFile kconfigMissingHostResponse defaultMissingHostBody id
    psUnknownHost <- taggedReadFile kconfigUnknownHostResponse defaultUnknownHostBody const
    psProxyException <- taggedReadFile kconfigProxyException defaultProxyException id
    -- calculate the number of microseconds since the
    -- configuration option is in milliseconds
    let psConnectionTimeBound = kconfigConnectionTimeBound * 1000
    let psIpFromHeader = kconfigIpFromHeader
    pure $ MkProxySettings{..}
    where
        psHostLookup = HostMan.lookupAction hostman . CI.mk


taggedReadFile :: forall r key. KnownSymbol key
               => Tagged key (Maybe FilePath) -> r -> (ByteString -> r) -> KeterM KeterConfig r
taggedReadFile (Tagged Nothing)     fallback _               = pure fallback
taggedReadFile (Tagged (Just file)) fallback processContents = do
  isExist <- liftIO $ Dir.doesFileExist file
  if isExist then liftIO (S.readFile file) <&> processContents else do
    wd <- liftIO Dir.getCurrentDirectory
    logWarnN . T.unwords $ ["could not find", tag, "on path", quote file, "with working dir", quote wd]
    return fallback
  where
    tag = fromString $ symbolVal (Proxy :: Proxy key)
    quote = ("'" <>) . (<> "'") . fromString

reverseProxy :: ListeningPort -> KeterM ProxySettings ()
reverseProxy listener = do
  settings <- ask
  let (run, isSecure) =
          case listener of
              LPInsecure host port -> 
                  (liftIO . Warp.runSettings (warp host port), False)
              LPSecure host port cert chainCerts key session -> 
                  (liftIO . WarpTLS.runTLS
                      (connectClientCertificates (psHostLookup settings) session $ WarpTLS.tlsSettingsChain
                          cert
                          (V.toList chainCerts)
                          key)
                      (warp host port), True)
  withClient isSecure >>= run . gzip def{gzipFiles = GzipPreCompressed GzipIgnore}
  where
    warp host port = Warp.setHost host $ Warp.setPort port Warp.defaultSettings

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
           -> KeterM ProxySettings Wai.Application
withClient isSecure = do
    cfg@MkProxySettings{..} <- ask
    let useHeader = psIpFromHeader
    withRunInIO $ \rio ->
        pure $ waiProxyToSettings
           (error "First argument to waiProxyToSettings forced, even thought wpsGetDest provided")
           defaultWaiProxySettings
            { wpsSetIpHeader =
                if useHeader
                    then SIHFromHeader
                    else SIHFromSocket
            ,  wpsGetDest = Just (getDest cfg)
            ,  wpsOnExc = handleProxyException (\app e -> rio $ logException app e) psProxyException
            } psManager
  where
    logException :: Wai.Request -> SomeException -> KeterM ProxySettings ()
    logException a b = logErrorN $ pack $ 
      "Got a proxy exception on request " <> show a <> " with exception "  <> show b


    getDest :: ProxySettings -> Wai.Request -> IO (LocalWaiProxySettings, WaiProxyResponse)
    getDest cfg@MkProxySettings{..} req =
        case Wai.requestHeaderHost req of
            Nothing -> do
              return (defaultLocalWaiProxySettings, WPRResponse $ missingHostResponse psMissingHost)
            Just host -> processHost cfg req host

    processHost :: ProxySettings -> Wai.Request -> S.ByteString -> IO (LocalWaiProxySettings, WaiProxyResponse)
    processHost cfg@MkProxySettings{..} req host = do
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
            Nothing -> do -- we don't know the host that was asked for
              return (defaultLocalWaiProxySettings, WPRResponse $ unknownHostResponse host (psUnknownHost host))
            Just ((action, requiresSecure), _)
                | requiresSecure && not isSecure -> performHttpsRedirect cfg host req
                | otherwise -> performAction psManager isSecure psConnectionTimeBound req action

    performHttpsRedirect MkProxySettings{..} host =
        return . (addjustGlobalBound psConnectionTimeBound Nothing,) . WPRResponse . redirectApp config
      where
        host' = CI.mk $ decodeUtf8With lenientDecode host
        config = RedirectConfig
            { redirconfigHosts = mempty
            , redirconfigStatus = 301
            , redirconfigActions = V.singleton $ RedirectAction SPAny
                                 $ RDPrefix True host' Nothing
            , redirconfigSsl = SSLTrue
            }

-- FIXME This is a workaround for
-- https://github.com/snoyberg/keter/issues/29. After some research, it
-- seems like Warp is behaving properly here. I'm still not certain why the
-- http call (from http-conduit) inside waiProxyToSettings could ever block
-- infinitely without the server it's connecting to going down, so that
-- requires more research. Meanwhile, this prevents the file descriptor
-- leak from occurring.
addjustGlobalBound :: Int -> Maybe Int -> LocalWaiProxySettings
addjustGlobalBound bound to = go `setLpsTimeBound` defaultLocalWaiProxySettings
  where
    go = case to <|> Just bound of
           Just x | x > 0 -> Just x
           _              -> Nothing


performAction :: Manager -> Bool -> Int -> Wai.Request -> ProxyActionRaw -> IO (LocalWaiProxySettings, WaiProxyResponse)
performAction psManager isSecure globalBound req = \case
  (PAPort port tbound) ->
    return (addjustGlobalBound globalBound tbound, WPRModifiedRequest req' $ ProxyDest "127.0.0.1" port)
      where
        req' = req
            { Wai.requestHeaders = ("X-Forwarded-Proto", protocol)
                                : Wai.requestHeaders req
            }
        protocol
            | isSecure = "https"
            | otherwise = "http"
  (PAStatic StaticFilesConfig {..}) ->
    return (addjustGlobalBound globalBound sfconfigTimeout, WPRApplication $ processMiddleware sfconfigMiddleware $ staticApp (defaultFileServerSettings sfconfigRoot)
        { ssListing =
            if sfconfigListings
                then Just defaultListing
                else Nothing
        })
  (PARedirect config) -> return (addjustGlobalBound globalBound Nothing, WPRResponse $ redirectApp config req)
  (PAReverseProxy config rpconfigMiddleware tbound) ->
       return (addjustGlobalBound globalBound tbound, WPRApplication
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

handleProxyException :: (Wai.Request -> SomeException -> IO ()) -> ByteString -> SomeException -> Wai.Application
handleProxyException handleException onexceptBody except req respond = do
  handleException req except
  respond $ missingHostResponse onexceptBody

defaultProxyException :: ByteString
defaultProxyException = "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>There was a proxy error, check the keter logs for details.</p></body></html>"

defaultMissingHostBody :: ByteString
defaultMissingHostBody = "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"

-- | Error, no host found in the header
missingHostResponse :: ByteString -> Wai.Response
missingHostResponse missingHost = Wai.responseBuilder
    status502
    [("Content-Type", "text/html; charset=utf-8")]
    $ copyByteString missingHost

defaultUnknownHostBody :: ByteString -> ByteString
defaultUnknownHostBody host =
  "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
  <> escapeHtml host <> "</code>, is not recognized.</p></body></html>"

-- | We found a host in the header, but we don't know about the host asked for.
unknownHostResponse :: ByteString -> ByteString -> Wai.Response
unknownHostResponse host body = Wai.responseBuilder
    status404
    [("Content-Type", "text/html; charset=utf-8"),
     ("X-Forwarded-Host",
      -- if an attacker manages to insert line breaks somehow,
      -- this is also vulnerable.
      escapeHtml host
     )]
    (copyByteString body)

escapeHtml :: ByteString -> ByteString
escapeHtml = toByteString . fromHtmlEscapedByteString
