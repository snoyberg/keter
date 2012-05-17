{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.Logger
    ( Logger
    , start
    , attach
    , detach
    , Handles (..)
    , dummy
    ) where

import Keter.Prelude
import System.IO (Handle, hClose)
import qualified Prelude as P
import qualified Keter.LogFile as LogFile
import Control.Concurrent (killThread)
import qualified Data.ByteString as S
import Control.Exception (fromException, AsyncException (ThreadKilled))

data Handles = Handles
    { stdIn :: Maybe Handle
    , stdOut :: Maybe Handle
    , stdErr :: Maybe Handle
    }

newtype Logger = Logger (Command -> KIO ())

data Command = Attach Handles | Detach

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
            Attach (Handles min mout merr) -> do
                hmClose min
                let go mhandle lf =
                        case mhandle of
                            Nothing -> return Nothing
                            Just handle -> do
                                etid <- forkKIO' $ listener handle lf
                                case etid of
                                    Left e -> do
                                        $logEx e
                                        hmClose mhandle
                                        return Nothing
                                    Right tid -> return $ Just tid
                newout <- go mout lfout
                newerr <- go merr lferr
                loop chan newout newerr

hmClose :: Maybe Handle -> KIO ()
hmClose Nothing = return ()
hmClose (Just h) = liftIO (hClose h) >>= either $logEx return

listener :: Handle -> LogFile.LogFile -> KIO ()
listener out lf =
    loop
  where
    loop = do
        ebs <- liftIO $ S.hGetSome out 4096
        case ebs of
            Left e -> do
                case fromException e of
                    Just ThreadKilled -> return ()
                    _ -> $logEx e
                hmClose $ Just out
            Right bs
                | S.null bs -> hmClose (Just out)
                | otherwise -> do
                    LogFile.addChunk lf bs
                    listener out lf

attach :: Logger -> Handles -> KIO ()
attach (Logger f) h = f (Attach h)

detach :: Logger -> KIO ()
detach (Logger f) = f Detach

dummy :: Logger
dummy = P.error "Logger.dummy"
