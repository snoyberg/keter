{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- | Manages a pool of available ports and allocates them.
module Keter.PortPool
    ( -- * Types
      PortPool
      -- * Actions
    , getPort
    , releasePort
      -- * Initialize
    , start
    ) where

import           Control.Applicative     ((<$>))
import qualified Control.Concurrent.MVar as M
import           Keter.Prelude
import           Keter.Types
import qualified Network
import           Prelude                 (IO)

data PPState = PPState
    { ppAvail    :: ![Port]
    , ppRecycled :: !([Port] -> [Port])
    }

newtype PortPool = PortPool (MVar PPState)

-- | Gets an unassigned port number.
getPort :: PortPool -> KIO (Either SomeException Port)
getPort (PortPool mstate) =
    modifyMVar mstate loop
  where
    loop :: PPState -> KIO (PPState, Either SomeException Port)
    loop PPState {..} =
        case ppAvail of
            p:ps -> do
                let next = PPState ps ppRecycled
                res <- liftIO $ Network.listenOn $ Network.PortNumber $ fromIntegral p
                case res of
                    Left (_ :: SomeException) -> do
                        log $ RemovingPort p
                        loop next
                    Right socket -> do
                        res' <- liftIO $ Network.sClose socket
                        case res' of
                            Left e -> do
                                $logEx e
                                log $ RemovingPort p
                                loop next
                            Right () -> return (next, Right p)
            [] ->
                case ppRecycled [] of
                    [] -> return (PPState [] id, Left $ toException NoPortsAvailable)
                    ps -> loop $ PPState ps id

-- | Return a port to the recycled collection of the pool.  Note that recycling
-- puts the new ports at the end of the queue (FIFO), so that if an application
-- holds onto the port longer than expected, there should be no issues.
releasePort :: PortPool -> Port -> KIO ()
releasePort (PortPool mstate) p =
    modifyMVar_ mstate $ \(PPState avail recycled) -> return $ PPState avail $ recycled . (p:)

start :: PortSettings -> IO PortPool
start PortSettings{..} =
    PortPool <$> M.newMVar freshState
  where
    freshState = PPState portRange id
