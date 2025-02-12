{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Handles allocation of temporary directories and unpacking of bundles into
-- them. Sets owner and group of all created files and directories as
-- necessary.
module Keter.TempTarball
    ( TempFolder
    , setup
    , unpackTempTar
    ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip (decompress)
import Control.Exception (bracketOnError, throwIO)
import Control.Monad (forM_, when)
import Data.ByteString.Lazy qualified as L
import Data.IORef qualified as I
import Data.Text (Text, pack, unpack)
import System.Directory qualified as D
import System.FilePath ((</>))
import System.Posix.Files (setOwnerAndGroup)
import System.Posix.Types (GroupID, UserID)

data TempFolder = TempFolder
    { tfRoot    :: FilePath
    , tfCounter :: I.IORef Word
    }

setup :: FilePath -> IO TempFolder
setup fp = do
    e <- D.doesDirectoryExist fp
    when e $ D.removeDirectoryRecursive fp
    D.createDirectoryIfMissing True fp
    c <- I.newIORef minBound
    return $ TempFolder fp c

getFolder :: Maybe (UserID, GroupID)
          -> TempFolder
          -> Text -- ^ prefix for folder name
          -> IO FilePath
getFolder muid TempFolder {..} appname = do
    !i <- I.atomicModifyIORef tfCounter $ \i -> (succ i, i)
    let fp = tfRoot </> unpack (appname <> "-" <> pack (show i))
    D.createDirectoryIfMissing True fp
    case muid of
        Nothing -> return ()
        Just (uid, gid) -> setOwnerAndGroup fp uid gid
    return fp

unpackTempTar :: Maybe (UserID, GroupID)
              -> TempFolder
              -> FilePath -- ^ bundle
              -> Text -- ^ prefix for folder name
              -> (FilePath -> IO a)
              -> IO a
unpackTempTar muid tf bundle appname withDir = do
    lbs <- L.readFile bundle
    bracketOnError (getFolder muid tf appname) D.removeDirectoryRecursive $ \dir -> do
        D.createDirectoryIfMissing True dir
        let entries = Tar.read $ decompress lbs
        Tar.unpack dir entries
        forM_ muid $ \perms ->
          Tar.foldEntries (setEntryPermission perms) (pure ()) throwIO entries
        withDir dir

setEntryPermission :: (UserID, GroupID) -> Tar.Entry ->  IO () -> IO ()
setEntryPermission (uid, gid) entry io =
  io >> setOwnerAndGroup (Tar.entryPath entry) uid gid
