{-# LANGUAGE OverloadedStrings #-}
-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , PortLookup
    , HostList
    , reverseProxySsl
    , setDir
    , SslConfig
    ) where

import Keter.Prelude ((++))
import Prelude hiding ((++), FilePath)
import Data.Conduit
import Data.Conduit.List (peek)
import Data.Conduit.Network
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Data.Char (isSpace, toLower)
import Control.Exception (onException)
import Keter.PortManager (Port)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString, toLazyByteString)
import Data.Monoid (mconcat)
import Keter.SSL

-- | Mapping from virtual hostname to port number.
type PortLookup = ByteString -> IO (Maybe Port)

type HostList = IO [ByteString]

reverseProxy :: ServerSettings -> PortLookup -> HostList -> IO ()
reverseProxy settings x = runTCPServer settings . withClient x

reverseProxySsl :: SslConfig -> PortLookup -> HostList -> IO ()
reverseProxySsl settings x = runTCPServerSsl settings . withClient x

withClient :: PortLookup
           -> HostList
           -> Source IO ByteString
           -> Sink ByteString IO ()
           -> IO ()
withClient portLookup hostList fromClient toClient = do
    (rsrc, mvhost) <- fromClient $$+ getVhost
    mport <- maybe (return Nothing) portLookup mvhost
    case mport of
        Nothing -> lift (fmap toResponse hostList) >>= mapM_ yield $$ toClient
        Just port -> runTCPClient (ClientSettings port "127.0.0.1") (withServer rsrc)
  where
    withServer rsrc fromServer toServer = do
        x <- newEmptyMVar
        tid1 <- forkIO $ (rsrc $$+- toServer) `onException` putMVar x True
        tid2 <- forkIO $ (fromServer $$ toClient) `onException` putMVar x False
        y <- takeMVar x
        killThread $ if y then tid2 else tid1

getVhost :: Monad m => Sink ByteString m (Maybe ByteString)
getVhost =
    peek >>= maybe (return Nothing) (return . go . drop 1 . S8.lines)
  where
    go [] = Nothing
    go (bs:bss)
        | S8.map toLower k == "host" = Just v
        | otherwise = go bss
      where
        (k, v') = S8.break (== ':') bs
        v = S8.takeWhile (not . isSpace) $ S8.dropWhile isSpace $ S8.drop 1 v'

toResponse :: [ByteString] -> [ByteString]
toResponse hosts =
    L.toChunks $ toLazyByteString $ front ++ mconcat (map go hosts) ++ end
  where
    front = fromByteString "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You may access the following sites:</p><ul>"
    end = fromByteString "</ul></body></html>"
    go host = fromByteString "<li><a href=\"http://" ++ fromByteString host ++ fromByteString "/\">" ++
              fromByteString host ++ fromByteString "</a></li>"
