{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Keter.TempFolder
    ( TempFolder
    , setup
    , getFolder
    ) where

import Keter.Prelude
import Data.Word (Word)
import Keter.Postgres (Appname)
import qualified Data.IORef as I
import System.Posix.Files (setOwnerAndGroup)
import System.Posix.Types (UserID, GroupID)
import qualified Filesystem.Path.CurrentOS as F

data TempFolder = TempFolder
    { tfRoot :: FilePath
    , tfCounter :: IORef Word
    }

setup :: FilePath -> KIO (Either SomeException TempFolder)
setup fp = liftIO $ do
    e <- isDirectory fp
    when e $ removeTree fp
    createTree fp
    c <- I.newIORef minBound
    return $ TempFolder fp c

getFolder :: Maybe (UserID, GroupID) -> TempFolder -> Appname -> KIO (Either SomeException FilePath)
getFolder muid TempFolder {..} appname = do
    !i <- atomicModifyIORef tfCounter $ \i -> (succ i, i)
    let fp = tfRoot </> fromText (appname ++ "-" ++ show i)
    liftIO $ do
        createTree fp
        case muid of
            Nothing -> return ()
            Just (uid, gid) -> setOwnerAndGroup (F.encodeString fp) uid gid
        return fp
