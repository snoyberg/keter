{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.Logger
    ( Logger
    , start
    , attach
    , detach
    , LogPipes (..)
    , LogPipe
    , mkLogPipe
    , dummy
    ) where

import Keter.Prelude
import qualified Prelude as P
import qualified Keter.LogFile as LogFile
import Control.Concurrent (killThread)
import qualified Data.ByteString as S
import Data.Conduit (Sink, await)
import qualified Control.Concurrent.MVar as M
import Control.Monad.Trans.Class (lift)

data LogPipes = LogPipes
    { stdOut :: LogPipe
    , stdErr :: LogPipe
    }

data LogPipe = LogPipe
    { readLogPipe :: KIO (Maybe S.ByteString)
    , closeLogPipe :: KIO ()
    }

mkLogPipe :: KIO (LogPipe, Sink S.ByteString P.IO ())
mkLogPipe = do
    toSink <- newEmptyMVar
    fromSink <- newEmptyMVar
    let pipe = LogPipe
            { readLogPipe = do
                putMVar toSink True
                takeMVar fromSink
            , closeLogPipe = do
                _ <- tryTakeMVar toSink
                putMVar toSink False
            }
        sink = do
            toCont <- lift $ M.takeMVar toSink
            if toCont
                then do
                    mbs <- await
                    lift $ M.putMVar fromSink mbs
                    maybe (return ()) (P.const sink) mbs
                else return ()
    return (pipe, sink)

newtype Logger = Logger (Command -> KIO ())

data Command = Attach LogPipes | Detach

start :: LogFile.LogFile -- ^ stdout
      -> LogFile.LogFile -- ^ stderr
      -> KIO Logger
start lfout lferr = do
    chan <- newChan
    forkKIO $ loop chan Nothing Nothing
    return $ Logger $ writeChan chan
  where
    killOld tid = do
        res <- liftIO $ killThread tid
        case res of
            Left e -> $logEx e
            Right () -> return ()

    loop chan moldout molderr = do
        c <- readChan chan
        maybe (return ()) killOld moldout
        maybe (return ()) killOld molderr
        case c of
            Detach -> do
                LogFile.close lfout
                LogFile.close lferr
            Attach (LogPipes out err) -> do
                LogFile.addChunk lfout "\n\nAttaching new process\n\n"
                LogFile.addChunk lferr "\n\nAttaching new process\n\n"
                let go logpipe lf = do
                        etid <- forkKIO' $ listener logpipe lf
                        case etid of
                            Left e -> do
                                $logEx e
                                closeLogPipe logpipe
                                return Nothing
                            Right tid -> return $ Just tid
                newout <- go out lfout
                newerr <- go err lferr
                loop chan newout newerr

listener :: LogPipe -> LogFile.LogFile -> KIO ()
listener out lf =
    loop
  where
    loop = do
        mbs <- readLogPipe out
        case mbs of
            Nothing -> return ()
            Just bs -> do
                LogFile.addChunk lf bs
                loop

attach :: Logger -> LogPipes -> KIO ()
attach (Logger f) h = f (Attach h)

detach :: Logger -> KIO ()
detach (Logger f) = f Detach

dummy :: Logger
dummy = Logger $ P.const $ return ()
