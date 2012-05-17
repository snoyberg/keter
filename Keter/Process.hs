{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.Process
    ( run
    , terminate
    , Process
    ) where

import Keter.Prelude
import Keter.Logger
import qualified System.Process as SP

data Status = NeedsRestart | NoRestart | Running SP.ProcessHandle

-- | Run the given command, restarting if the process dies.
run :: FilePath -- ^ executable
    -> FilePath -- ^ working directory
    -> [String] -- ^ command line parameter
    -> [(String, String)] -- ^ environment
    -> Logger
    -> KIO Process
run exec dir args env logger = do
    mstatus <- newMVar NeedsRestart
    let loop = do
            next <- modifyMVar mstatus $ \status ->
                case status of
                    NoRestart -> return (NoRestart, return ())
                    _ -> do
                        -- FIXME put in some kind of rate limiting: if we last
                        -- tried to restart within five second, wait an extra
                        -- five seconds
                        res <- liftIO $ SP.createProcess cp
                        case res of
                            Left e -> do
                                $logEx e
                                return (NeedsRestart, return ())
                            Right (hin, hout, herr, ph) -> do
                                attach logger $ Handles hin hout herr
                                log $ ProcessCreated exec
                                return (Running ph, liftIO (SP.waitForProcess ph) >> loop)
            next
    forkKIO loop
    return $ Process mstatus
  where
    cp = (SP.proc (toString exec) $ map toString args)
        { SP.cwd = Just $ toString dir
        , SP.env = Just $ map (toString *** toString) env
        , SP.std_in = SP.CreatePipe
        , SP.std_out = SP.CreatePipe
        , SP.std_err = SP.CreatePipe
        , SP.close_fds = True
        }

-- | Abstract type containing information on a process which will be restarted.
newtype Process = Process (MVar Status)

-- | Terminate the process and prevent it from being restarted.
terminate :: Process -> KIO ()
terminate (Process mstatus) = do
    status <- swapMVar mstatus NoRestart
    case status of
        Running ph -> void $ liftIO $ SP.terminateProcess ph
        _ -> return ()
