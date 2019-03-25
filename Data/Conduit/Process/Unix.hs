{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Data.Conduit.Process.Unix
    ( -- * Process tracking
      -- $processTracker

      -- ** Types
      ProcessTracker
      -- ** Functions
    , initProcessTracker

      -- * Monitored process
    , MonitoredProcess
    , monitorProcess
    , terminateMonitoredProcess
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((***))
import           Control.Concurrent              (forkIO)
import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.MVar         (MVar, modifyMVar, modifyMVar_,
                                                  newEmptyMVar, newMVar,
                                                  putMVar, readMVar, swapMVar,
                                                  takeMVar)
import           Control.Exception               (Exception, SomeException,
                                                  bracketOnError, finally,
                                                  handle, mask_,
                                                  throwIO, try)
import           Control.Monad                   (void)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as S8
import           Data.Conduit                    (ConduitT, (.|), runConduit)
import           Data.Conduit.Binary             (sinkHandle, sourceHandle)
import qualified Data.Conduit.List               as CL
import           Data.IORef                      (IORef, newIORef, readIORef,
                                                  writeIORef)
import           Data.Time                       (getCurrentTime)
import           Data.Time                       (diffUTCTime)
import           Data.Typeable                   (Typeable)
import           Foreign.C.Types
import           Prelude                         (Bool (..), Either (..), IO,
                                                  Maybe (..), Monad (..), Show,
                                                  const, error,
                                                  map, maybe, show,
                                                  ($), ($!), (*), (<),
                                                  (==))
import           System.Exit                     (ExitCode)
import           System.IO                       (hClose)
import           System.Posix.IO.ByteString      ( closeFd, createPipe,
                                                  fdToHandle)
import           System.Posix.Signals            (sigKILL, signalProcess)
import           System.Posix.Types              (CPid (..))
import           System.Process                  (CmdSpec (..), CreateProcess (..),
                                                  StdStream (..), createProcess,
                                                  terminateProcess, waitForProcess)
import           System.Process.Internals        (ProcessHandle (..),
                                                  ProcessHandle__ (..))

processHandleMVar :: ProcessHandle -> MVar ProcessHandle__
#if MIN_VERSION_process(1, 6, 0)
processHandleMVar (ProcessHandle m _ _) = m
#elif MIN_VERSION_process(1, 2, 0)
processHandleMVar (ProcessHandle m _) = m
#else
processHandleMVar (ProcessHandle m) = m
#endif

withProcessHandle_
        :: ProcessHandle
        -> (ProcessHandle__ -> IO ProcessHandle__)
        -> IO ()
withProcessHandle_ ph io = modifyMVar_ (processHandleMVar ph) io

-- | Kill a process by sending it the KILL (9) signal.
--
-- Since 0.1.0
killProcess :: ProcessHandle -> IO ()
killProcess ph = withProcessHandle_ ph $ \p_ ->
    case p_ of
        ClosedHandle _ -> return p_
        OpenHandle h -> do
            signalProcess sigKILL h
            return p_

ignoreExceptions :: IO () -> IO ()
ignoreExceptions = handle (\(_ :: SomeException) -> return ())

-- $processTracker
--
-- Ensure that child processes are killed, regardless of how the parent process exits.
--
-- The technique used here is:
--
-- * Create a pipe.
--
-- * Fork a new child process that listens on the pipe.
--
-- * In the current process, send updates about processes that should be auto-killed.
--
-- * When the parent process dies, listening on the pipe in the child process will get an EOF.
--
-- * When the child process receives that EOF, it kills all processes it was told to auto-kill.
--
-- This code was originally written for Keter, but was moved to unix-process
-- conduit in the 0.2.1 release.

foreign import ccall unsafe "launch_process_tracker"
    c_launch_process_tracker :: IO CInt

foreign import ccall unsafe "track_process"
    c_track_process :: ProcessTracker -> CPid -> CInt -> IO ()

-- | Represents the child process which handles process cleanup.
--
-- Since 0.2.1
newtype ProcessTracker = ProcessTracker CInt

-- | Represents a child process which is currently being tracked by the cleanup
-- child process.
--
-- Since 0.2.1
data TrackedProcess = TrackedProcess !ProcessTracker !(IORef MaybePid) !(IO ExitCode)

data MaybePid = NoPid | Pid !CPid

-- | Fork off the child cleanup process.
--
-- This will ideally only be run once for your entire application.
--
-- Since 0.2.1
initProcessTracker :: IO ProcessTracker
initProcessTracker = do
    i <- c_launch_process_tracker
    if i == -1
        then throwIO CannotLaunchProcessTracker
        else return $! ProcessTracker i

-- | Since 0.2.1
data ProcessTrackerException = CannotLaunchProcessTracker
    deriving (Show, Typeable)
instance Exception ProcessTrackerException

-- | Begin tracking the given process. If the 'ProcessHandle' refers to a
-- closed process, no tracking will occur. If the process is closed, then it
-- will be untracked automatically.
--
-- Note that you /must/ compile your program with @-threaded@; see
-- 'waitForProcess'.
--
-- Since 0.2.1
trackProcess :: ProcessTracker -> ProcessHandle -> IO TrackedProcess
trackProcess pt ph = mask_ $ do
    mpid <- readMVar $ processHandleMVar ph
    mpid' <- case mpid of
        ClosedHandle{} -> return NoPid
        OpenHandle pid -> do
            c_track_process pt pid 1
            return $ Pid pid
    ipid <- newIORef mpid'
    baton <- newEmptyMVar
    let tp = TrackedProcess pt ipid (takeMVar baton)
    case mpid' of
        NoPid -> return ()
        Pid _ -> void $ forkIO $ do
            waitForProcess ph >>= putMVar baton
            untrackProcess tp
    return $! tp

-- | Explicitly remove the given process from the tracked process list in the
-- cleanup process.
--
-- Since 0.2.1
untrackProcess :: TrackedProcess -> IO ()
untrackProcess (TrackedProcess pt ipid _) = mask_ $ do
    mpid <- readIORef ipid
    case mpid of
        NoPid -> return ()
        Pid pid -> do
            c_track_process pt pid 0
            writeIORef ipid NoPid

-- | Fork and execute a subprocess, sending stdout and stderr to the specified
-- rotating log.
--
-- Since 0.2.1
forkExecuteLog :: ByteString -- ^ command
               -> [ByteString] -- ^ args
               -> Maybe [(ByteString, ByteString)] -- ^ environment
               -> Maybe ByteString -- ^ working directory
               -> Maybe (ConduitT () ByteString IO ()) -- ^ stdin
               -> (ByteString -> IO ()) -- ^ both stdout and stderr will be sent to this location
               -> IO ProcessHandle
forkExecuteLog cmd args menv mwdir mstdin rlog = bracketOnError
    setupPipe
    cleanupPipes
    usePipes
  where
    setupPipe = bracketOnError
        createPipe
        (\(x, y) -> closeFd x `finally` closeFd y)
        (\(x, y) -> (,) <$> fdToHandle x <*> fdToHandle y)
    cleanupPipes (x, y) = hClose x `finally` hClose y

    usePipes pipes@(readerH, writerH) = do
        (min, _, _, ph) <- createProcess CreateProcess
            { cmdspec = RawCommand (S8.unpack cmd) (map S8.unpack args)
            , cwd = S8.unpack <$> mwdir
            , env = map (S8.unpack *** S8.unpack) <$> menv
            , std_in = maybe Inherit (const CreatePipe) mstdin
            , std_out = UseHandle writerH
            , std_err = UseHandle writerH
            , close_fds = True
            , create_group = True
            , use_process_jobs = False
#if MIN_VERSION_process(1, 2, 0)
            , delegate_ctlc = False
#endif
#if MIN_VERSION_process(1, 3, 0)
            , detach_console = True
            , create_new_console = False
            , new_session = True
#endif
#if MIN_VERSION_process(1, 4, 0)
            , child_group = Nothing
            , child_user = Nothing
#endif
            }
        ignoreExceptions $ addAttachMessage pipes ph
        void $ forkIO $ ignoreExceptions $
            (runConduit $ sourceHandle readerH .| CL.mapM_ rlog) `finally` hClose readerH
        case (min, mstdin) of
            (Just h, Just source) -> void $ forkIO $ ignoreExceptions $
                (runConduit $ source .| sinkHandle h) `finally` hClose h
            (Nothing, Nothing) -> return ()
            _ -> error $ "Invariant violated: Data.Conduit.Process.Unix.forkExecuteLog"
        return ph

    addAttachMessage pipes ph = withProcessHandle_ ph $ \p_ -> do
        now <- getCurrentTime
        case p_ of
            ClosedHandle ec -> do
                rlog $ S8.concat
                    [ "\n\n"
                    , S8.pack $ show now
                    , ": Process immediately died with exit code "
                    , S8.pack $ show ec
                    , "\n\n"
                    ]
                cleanupPipes pipes
            OpenHandle h -> do
                rlog $ S8.concat
                    [ "\n\n"
                    , S8.pack $ show now
                    , ": Attached new process "
                    , S8.pack $ show h
                    , "\n\n"
                    ]
        return p_

data Status = NeedsRestart | NoRestart | Running ProcessHandle

-- | Run the given command, restarting if the process dies.
monitorProcess
    :: (ByteString -> IO ()) -- ^ log
    -> ProcessTracker
    -> Maybe S8.ByteString -- ^ setuid
    -> S8.ByteString -- ^ executable
    -> S8.ByteString -- ^ working directory
    -> [S8.ByteString] -- ^ command line parameter
    -> [(S8.ByteString, S8.ByteString)] -- ^ environment
    -> (ByteString -> IO ())
    -> (ExitCode -> IO Bool) -- ^ should we restart?
    -> IO MonitoredProcess
monitorProcess log processTracker msetuid exec dir args env' rlog shouldRestart = do
    mstatus <- newMVar NeedsRestart
    let loop mlast = do
            next <- modifyMVar mstatus $ \status ->
                case status of
                    NoRestart -> return (NoRestart, return ())
                    _ -> do
                        now <- getCurrentTime
                        case mlast of
                            Just last | diffUTCTime now last < 5 -> do
                                log $ "Process restarting too quickly, waiting before trying again: " `S8.append` exec
                                threadDelay $ 5 * 1000 * 1000
                            _ -> return ()
                        let (cmd, args') =
                                case msetuid of
                                    Nothing -> (exec, args)
                                    Just setuid -> ("sudo", "-E" : "-u" : setuid : "--" : exec : args)
                        res <- try $ forkExecuteLog
                            cmd
                            args'
                            (Just env')
                            (Just dir)
                            (Just $ return ())
                            rlog
                        case res of
                            Left e -> do
                                log $ "Data.Conduit.Process.Unix.monitorProcess: " `S8.append` S8.pack (show (e :: SomeException))
                                return (NeedsRestart, return ())
                            Right pid -> do
                                log $ "Process created: " `S8.append` exec
                                return (Running pid, do
                                    TrackedProcess _ _ wait <- trackProcess processTracker pid
                                    ec <- wait
                                    shouldRestart' <- shouldRestart ec
                                    if shouldRestart'
                                        then loop (Just now)
                                        else return ())
            next
    _ <- forkIO $ loop Nothing
    return $ MonitoredProcess mstatus

-- | Abstract type containing information on a process which will be restarted.
newtype MonitoredProcess = MonitoredProcess (MVar Status)

-- | Terminate the process and prevent it from being restarted.
terminateMonitoredProcess :: MonitoredProcess -> IO ()
terminateMonitoredProcess (MonitoredProcess mstatus) = do
    status <- swapMVar mstatus NoRestart
    case status of
        Running pid -> do
            terminateProcess pid
            threadDelay 1000000
            killProcess pid
        _ -> return ()
