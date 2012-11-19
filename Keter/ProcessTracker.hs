{-# LANGUAGE ForeignFunctionInterface #-}
-- | Ensures that processes are stopped when Keter shuts down.
module Keter.ProcessTracker
    ( ProcessTracker
    , trackProcess
    , initProcessTracker
    ) where

import System.Process.Internals
import Foreign.C (CInt (..))
import System.Posix.Types (CPid (..))
import Control.Concurrent.MVar (readMVar)

foreign import ccall unsafe "launch_process_tracker"
    c_launch_process_tracker :: IO CInt

foreign import ccall unsafe "track_process"
    c_track_process :: ProcessTracker -> CPid -> CInt -> IO ()

newtype ProcessTracker = ProcessTracker CInt

initProcessTracker :: IO ProcessTracker
initProcessTracker = do
    i <- c_launch_process_tracker
    if i == -1
        then error "Unable to launch process tracker"
        else return $ ProcessTracker i

trackProcess :: ProcessTracker -> ProcessHandle -> IO (IO ())
trackProcess pt (ProcessHandle mph) = do
    mpid <- readMVar mph
    case mpid of
        ClosedHandle{} -> return $ return ()
        OpenHandle pid -> do
            c_track_process pt pid 1
            return $ c_track_process pt pid 0
