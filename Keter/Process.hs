{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Keter.Process
    ( run
    , terminate
    , Process
    ) where

import Keter.Prelude
import Data.Time (diffUTCTime)
import Data.Conduit.Process.Unix (forkExecuteLog, killProcess, terminateProcess, ProcessTracker, trackProcess, RotatingLog, waitForProcess)
import System.Process (ProcessHandle)
import Data.Text.Encoding (encodeUtf8)

data Status = NeedsRestart | NoRestart | Running ProcessHandle

-- | Run the given command, restarting if the process dies.
run :: ProcessTracker
    -> Maybe Text -- ^ setuid
    -> FilePath -- ^ executable
    -> FilePath -- ^ working directory
    -> [String] -- ^ command line parameter
    -> [(String, String)] -- ^ environment
    -> RotatingLog
    -> KIO Process
run processTracker msetuid exec dir args env rlog = do
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
                        let cmd0 = encodeUtf8 $ either id id $ toText exec
                            args0 = map encodeUtf8 args
                            (cmd, args') =
                                case msetuid of
                                    Nothing -> (cmd0, args0)
                                    Just setuid -> ("sudo", "-E" : "-u" : encodeUtf8 setuid : "--" : cmd0 : args0)
                        res <- liftIO $ forkExecuteLog
                            cmd
                            args'
                            (Just $ map (encodeUtf8 *** encodeUtf8) env)
                            (Just $ encodeUtf8 $ either id id $ toText dir)
                            (Just $ return ())
                            rlog
                        case res of
                            Left e -> do
                                $logEx e
                                return (NeedsRestart, return ())
                            Right pid -> do
                                log $ ProcessCreated exec
                                return (Running pid, do
                                    void $ liftIO $ do
                                        void $ trackProcess processTracker pid
                                        void $ waitForProcess pid
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
