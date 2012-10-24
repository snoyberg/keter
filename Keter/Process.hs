{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Keter.Process
    ( run
    , terminate
    , Process
    ) where

import Keter.Prelude
import Keter.Logger (Logger, attach, LogPipes (..), mkLogPipe)
import Data.Time (diffUTCTime)
import Data.Conduit.Process.Unix (forkExecuteFile, waitForProcess, killProcess, terminateProcess)
import System.Process (ProcessHandle)
import Prelude (error)
import Filesystem.Path.CurrentOS (encode)
import Data.Text.Encoding (encodeUtf8)
import Data.Conduit (($$))
import Control.Exception (onException)

data Status = NeedsRestart | NoRestart | Running ProcessHandle

-- | Run the given command, restarting if the process dies.
run :: Maybe Text -- ^ setuid
    -> FilePath -- ^ executable
    -> FilePath -- ^ working directory
    -> [String] -- ^ command line parameter
    -> [(String, String)] -- ^ environment
    -> Logger
    -> KIO Process
run msetuid exec dir args env logger = do
    mstatus <- newMVar NeedsRestart
    let loop mlast = do
            next <- modifyMVar mstatus $ \status ->
                case status of
                    NoRestart -> return (NoRestart, return ())
                    _ -> do
                        now <- getCurrentTime
                        case mlast of
                            Just last | diffUTCTime now last < 5 -> do
                                log $ ProcessWaiting exec
                                threadDelay $ 5 * 1000 * 1000
                            _ -> return ()
                        (pout, sout) <- mkLogPipe
                        (perr, serr) <- mkLogPipe
                        let cmd0 = encode exec
                            args0 = map encodeUtf8 args
                            (cmd, args') =
                                case msetuid of
                                    Nothing -> (cmd0, args0)
                                    Just setuid -> ("sudo", "-E" : "-u" : encodeUtf8 setuid : "--" : cmd0 : args0)
                        res <- liftIO $ forkExecuteFile
                            cmd
                            args'
                            (Just $ map (encodeUtf8 *** encodeUtf8) env)
                            (Just $ encode dir)
                            (Just $ return ())
                            (Just sout)
                            (Just serr)
                        case res of
                            Left e -> do
                                $logEx e
                                void $ liftIO $ return () $$ sout
                                void $ liftIO $ return () $$ serr
                                return (NeedsRestart, return ())
                            Right pid -> do
                                attach logger $ LogPipes pout perr
                                log $ ProcessCreated exec
                                return (Running pid, do
                                    _ <- liftIO $ waitForProcess pid `onException` killProcess pid
                                    loop (Just now))
            next
    forkKIO $ loop Nothing
    return $ Process mstatus

-- | Abstract type containing information on a process which will be restarted.
newtype Process = Process (MVar Status)

-- | Terminate the process and prevent it from being restarted.
terminate :: Process -> KIO ()
terminate (Process mstatus) = do
    status <- swapMVar mstatus NoRestart
    case status of
        Running pid -> do
            void $ liftIO $ terminateProcess pid
            threadDelay 1000000
            void $ liftIO $ killProcess pid
        _ -> return ()
