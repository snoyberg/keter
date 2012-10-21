{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.PortManager
    ( -- * Types
      Port
    , Host
    , PortManager
    , PortEntry (..)
      -- ** Settings
    , Settings
    , portRange
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
import Control.Monad (forever, mzero)
import Data.ByteString.Char8 ()
import qualified Network
import qualified Data.ByteString as S
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON (parseJSON), Value (Object))
import Control.Applicative ((<$>))

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = String

data Command = GetPort (Either SomeException Port -> KIO ())
             | ReleasePort Port
             | AddEntry Host PortEntry
             | RemoveEntry Host
             | LookupPort S.ByteString (Maybe PortEntry -> KIO ())

-- | An abstract type which can accept commands and sends them to a background
-- nginx thread.
newtype PortManager = PortManager (Command -> KIO ())

-- | Controls execution of the nginx thread. Follows the settings type pattern.
-- See: <http://www.yesodweb.com/book/settings-types>.
data Settings = Settings
    { portRange :: [Port]
      -- ^ Which ports to assign to apps. Default: 4000-4999
    }

instance Default Settings where
    def = Settings
        { portRange = [4000..4999]
        }

instance FromJSON Settings where
    parseJSON (Object _) = Settings
        <$> return (portRange def)
    parseJSON _ = mzero

-- | Start running a separate thread which will accept commands and modify
-- Nginx's behavior accordingly.
start :: Settings -> KIO (Either SomeException PortManager)
start Settings{..} = do
    chan <- newChan
    forkKIO $ flip S.evalStateT (NState portRange [] Map.empty) $ forever $ do
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
            LookupPort h f -> do
                NState {..} <- S.get
                lift $ f $ Map.lookup h nsEntries
    return $ Right $ PortManager $ writeChan chan
  where
    change f = do
        ns <- S.get
        let entries = f $ nsEntries ns
        S.put $ ns { nsEntries = entries }

data NState = NState
    { nsAvail :: [Port]
    , nsRecycled :: [Port]
    , nsEntries :: Map.Map S.ByteString PortEntry
    }

-- | Gets an unassigned port number.
getPort :: PortManager -> KIO (Either SomeException Port)
getPort (PortManager f) = do
    x <- newEmptyMVar
    f $ GetPort $ \p -> putMVar x p
    takeMVar x

-- | Inform the nginx thread that the given port number is no longer being
-- used, and may be reused by a new process. Note that recycling puts the new
-- ports at the end of the queue (FIFO), so that if an application holds onto
-- the port longer than expected, there should be no issues.
releasePort :: PortManager -> Port -> KIO ()
releasePort (PortManager f) p = f $ ReleasePort p

-- | Add a new entry to the configuration for the given hostname and reload
-- nginx. Will overwrite any existing configuration for the given host. The
-- second point is important: it is how we achieve zero downtime transitions
-- between an old and new version of an app.
addEntry :: PortManager -> Host -> PortEntry -> KIO ()
addEntry (PortManager f) h p = f $ AddEntry h p

data PortEntry = PEPort Port | PEStatic FilePath | PERedirect S.ByteString

-- | Remove an entry from the configuration and reload nginx.
removeEntry :: PortManager -> Host -> KIO ()
removeEntry (PortManager f) h = f $ RemoveEntry h

lookupPort :: PortManager -> S.ByteString -> KIO (Maybe PortEntry)
lookupPort (PortManager f) h = do
    x <- newEmptyMVar
    f $ LookupPort h $ \p -> putMVar x p
    takeMVar x
