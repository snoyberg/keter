module Keter.Process
    ( run
    , terminate
    , Process
    ) where

import qualified System.Process as SP
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as M

data Status = NeedsRestart | NoRestart | Running SP.ProcessHandle

-- | Run the given command, restarting if the process dies.
run :: FilePath -- ^ executable
    -> FilePath -- ^ working directory
    -> [String] -- ^ command line parameter
    -> [(String, String)] -- ^ environment
    -> IO Process
run exec dir args env = do
    mstatus <- M.newMVar NeedsRestart
    let loop = do
            next <- M.modifyMVar mstatus $ \status ->
                case status of
                    NoRestart -> return (NoRestart, return ())
                    _ -> do
                        (_, _, _, ph) <- SP.createProcess cp
                        return (Running ph, SP.waitForProcess ph >> loop)
            next
    _ <- forkIO loop
    return $ Process mstatus
  where
    cp = (SP.proc exec args)
        { SP.cwd = Just dir
        , SP.env = Just env
        , SP.std_in = SP.Inherit -- FIXME
        , SP.std_out = SP.Inherit -- FIXME
        , SP.std_err = SP.Inherit -- FIXME
        , SP.close_fds = True
        }

-- | Abstract type containing information on a process which will be restarted.
newtype Process = Process (M.MVar Status)

-- | Terminate the process and prevent it from being restarted.
terminate :: Process -> IO ()
terminate (Process mstatus) = do
    status <- M.swapMVar mstatus NoRestart
    case status of
        Running ph -> SP.terminateProcess ph
        _ -> return ()
