{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Keter.TempFolder
    ( TempFolder
    , setup
    , getFolder
    ) where

import qualified Data.IORef as I
import Data.Word (Word)
import System.Directory
    ( removeDirectoryRecursive, doesDirectoryExist
    , createDirectoryIfMissing, createDirectory
    )
import Keter.Postgres (Appname)
import Data.Text (unpack)
import Control.Monad (when)

data TempFolder = TempFolder
    { tfRoot :: FilePath
    , tfCounter :: I.IORef Word
    }

setup :: FilePath -> IO TempFolder
setup fp = do
    e <- doesDirectoryExist fp
    when e $ removeDirectoryRecursive fp
    createDirectoryIfMissing True fp
    c <- I.newIORef minBound
    return $ TempFolder fp c

getFolder :: TempFolder -> Appname -> IO FilePath
getFolder TempFolder {..} appname = do
    !i <- I.atomicModifyIORef tfCounter $ \i -> (i + 1, i)
    let fp = tfRoot ++ '/' : unpack appname ++ '-' : show i
    createDirectory fp
    return fp
