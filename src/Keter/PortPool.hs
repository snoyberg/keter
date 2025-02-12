{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Data.Text (pack)
import Keter.Common
import Keter.Config
import Keter.Context
import Network.Socket
import Prelude hiding (log)

data PPState = PPState
    { ppAvail    :: ![Port]
    , ppRecycled :: !([Port] -> [Port])
    }

newtype PortPool = PortPool (MVar PPState)

-- | Gets an unassigned port number.
getPort :: PortPool
        -> KeterM cfg (Either SomeException Port)
getPort (PortPool mstate) =
    withRunInIO $ \rio -> modifyMVar mstate (rio . loop)
  where
    removePortMsg p = pack $
        "Port in use, removing from port pool: "
        ++ show p

    loop :: PPState -> KeterM cfg (PPState, Either SomeException Port)
    loop PPState {..} =
        case ppAvail of
            p:ps -> do
                let next = PPState ps ppRecycled
                res <- liftIO $ try $ listenOn $ show p
                case res of
                    Left (_ :: SomeException) -> do
                        $logInfo $ removePortMsg p
                        loop next
                    Right socket' -> do
                        res' <- liftIO $ try @SomeException $ close socket'
                        case res' of
                            Left e -> do
                                $logError $ pack $ show e
                                $logInfo $ removePortMsg p
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
             close
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
