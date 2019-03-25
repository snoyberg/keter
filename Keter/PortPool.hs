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
import           Control.Concurrent.MVar
import           Control.Exception
import           Keter.Types
import qualified Network
import           Network.Socket
import           Prelude                 hiding (log)

data PPState = PPState
    { ppAvail    :: ![Port]
    , ppRecycled :: !([Port] -> [Port])
    }

newtype PortPool = PortPool (MVar PPState)

-- | Gets an unassigned port number.
getPort :: (LogMessage -> IO ())
        -> PortPool
        -> IO (Either SomeException Port)
getPort log (PortPool mstate) =
    modifyMVar mstate loop
  where
    loop :: PPState -> IO (PPState, Either SomeException Port)
    loop PPState {..} =
        case ppAvail of
            p:ps -> do
                let next = PPState ps ppRecycled
                res <- try $ listenOn $ show p
                case res of
                    Left (_ :: SomeException) -> do
                        log $ RemovingPort p
                        loop next
                    Right socket -> do
                        res' <- try $ close socket
                        case res' of
                            Left e -> do
                                $logEx log e
                                log $ RemovingPort p
                                loop next
                            Right () -> return (next, Right p)
            [] ->
                case ppRecycled [] of
                    [] -> return (PPState [] id, Left $ toException NoPortsAvailable)
                    ps -> loop $ PPState ps id

    listenOn port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        bracketOnError
             (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
             (close)
             (\sock -> do
                 setSocketOption sock ReuseAddr 1
                 bind sock (addrAddress addr)
                 listen sock maxListenQueue
                 return sock
             )

-- | Return a port to the recycled collection of the pool.  Note that recycling
-- puts the new ports at the end of the queue (FIFO), so that if an application
-- holds onto the port longer than expected, there should be no issues.
releasePort :: PortPool -> Port -> IO ()
releasePort (PortPool mstate) p =
    modifyMVar_ mstate $ \(PPState avail recycled) -> return $ PPState avail $ recycled . (p:)

start :: PortSettings -> IO PortPool
start PortSettings{..} =
    PortPool <$> newMVar freshState
  where
    freshState = PPState portRange id
