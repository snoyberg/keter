{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.HostManager
    ( -- * Types
      Port
    , Host
    , HostManager
    , HostEntry (..)
      -- * Actions
    , getPort
    , releasePort
    , addEntry
    , removeEntry
    , lookupPort
      -- * Initialize
    , start
    ) where

import Keter.Prelude
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Control.Monad (forever, mplus)
import Data.ByteString.Char8 ()
import qualified Network
import qualified Data.ByteString as S
import Data.Text.Encoding (encodeUtf8)
import qualified Keter.ReverseProxy as ReverseProxy (RPEntry)
import Keter.Types

data Command = GetPort (Either SomeException Port -> KIO ())
             | ReleasePort Port
             | AddEntry Host HostEntry
             | RemoveEntry Host
             | AddDefaultEntry HostEntry
             | RemoveDefaultEntry
             | LookupPort S.ByteString (Maybe HostEntry -> KIO ())

-- | An abstract type which can accept commands and sends them to a background
-- nginx thread.
newtype HostManager = HostManager (Command -> KIO ())

-- | Start running a separate thread which will accept commands and modify
-- Nginx's behavior accordingly.
start :: PortSettings -> KIO (Either SomeException HostManager)
start PortSettings{..} = do
    chan <- newChan
    forkKIO $ flip S.evalStateT freshState $ forever $ do
        command <- lift $ readChan chan
        case command of
            GetPort f -> do
                ns0 <- S.get
                let loop :: NState -> KIO (Either SomeException Port, NState)
                    loop ns =
                        case nsAvail ns of
                            p:ps -> do
                                res <- liftIO $ Network.listenOn $ Network.PortNumber $ fromIntegral p
                                case res of
                                    Left (_ :: SomeException) -> do
                                        log $ RemovingPort p
                                        loop ns { nsAvail = ps }
                                    Right socket -> do
                                        res' <- liftIO $ Network.sClose socket
                                        case res' of
                                            Left e -> do
                                                $logEx e
                                                log $ RemovingPort p
                                                loop ns { nsAvail = ps }
                                            Right () -> return (Right p, ns { nsAvail = ps })
                            [] ->
                                case reverse $ nsRecycled ns of
                                    [] -> return (Left $ toException NoPortsAvailable, ns)
                                    ps -> loop ns { nsAvail = ps, nsRecycled = [] }
                (eport, ns) <- lift $ loop ns0
                S.put ns
                lift $ f eport
            ReleasePort p ->
                S.modify $ \ns -> ns { nsRecycled = p : nsRecycled ns }
            AddEntry h e -> change $ Map.insert (encodeUtf8 h) e
            RemoveEntry h -> change $ Map.delete $ encodeUtf8 h
            AddDefaultEntry e -> S.modify $ \ns -> ns { nsDefault = Just e }
            RemoveDefaultEntry -> S.modify $ \ns -> ns { nsDefault = Nothing }
            LookupPort h f -> do
                NState {..} <- S.get
                lift $ f $ mplus (Map.lookup h nsEntries) nsDefault
    return $ Right $ HostManager $ writeChan chan
  where
    change f = do
        ns <- S.get
        let entries = f $ nsEntries ns
        S.put $ ns { nsEntries = entries }
    freshState = NState portRange [] Map.empty Nothing

data NState = NState
    { nsAvail :: [Port]
    , nsRecycled :: [Port]
    , nsEntries :: Map.Map S.ByteString HostEntry
    , nsDefault :: Maybe HostEntry
    }

-- | Gets an unassigned port number.
getPort :: HostManager -> KIO (Either SomeException Port)
getPort (HostManager f) = do
    x <- newEmptyMVar
    f $ GetPort $ \p -> putMVar x p
    takeMVar x

-- | Inform the nginx thread that the given port number is no longer being
-- used, and may be reused by a new process. Note that recycling puts the new
-- ports at the end of the queue (FIFO), so that if an application holds onto
-- the port longer than expected, there should be no issues.
releasePort :: HostManager -> Port -> KIO ()
releasePort (HostManager f) p = f $ ReleasePort p

-- | Add a new entry to the configuration for the given hostname and reload
-- nginx. Will overwrite any existing configuration for the given host. The
-- second point is important: it is how we achieve zero downtime transitions
-- between an old and new version of an app.
addEntry :: HostManager -> Host -> HostEntry -> KIO ()
addEntry (HostManager f) h p = f $ case h of
    "*" -> AddDefaultEntry p
    _   -> AddEntry h p

data HostEntry = PEPort Port | PEStatic FilePath | PERedirect S.ByteString | PEReverseProxy ReverseProxy.RPEntry

-- | Remove an entry from the configuration and reload nginx.
removeEntry :: HostManager -> Host -> KIO ()
removeEntry (HostManager f) h = f $ case h of
    "*" -> RemoveDefaultEntry
    _   -> RemoveEntry h

lookupPort :: HostManager -> S.ByteString -> KIO (Maybe HostEntry)
lookupPort (HostManager f) h = do
    x <- newEmptyMVar
    f $ LookupPort h $ \p -> putMVar x p
    takeMVar x
