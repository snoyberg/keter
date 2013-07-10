{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Handles allocation of temporary directories and unpacking of bundles into
-- them. Sets owner and group of all created files and directories as
-- necessary.
module Keter.TempFolder
    ( TempFolder
    , setup
    , unpackTempTar
    ) where

import qualified Codec.Archive.Tar         as Tar
import qualified Codec.Archive.Tar.Check   as Tar
import qualified Codec.Archive.Tar.Entry   as Tar
import           Codec.Compression.GZip    (decompress)
import           Control.Exception         (bracket, bracketOnError, throwIO)
import           Control.Monad             (unless)
import qualified Data.ByteString.Lazy      as L
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import qualified Data.IORef                as I
import           Data.Word                 (Word)
import qualified Filesystem                as F
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.Ptr               (castPtr)
import           Keter.Prelude
import           Keter.Types               (Appname)
import           Prelude                   (IO)
import           System.Posix.Files        (setFdOwnerAndGroup,
                                            setOwnerAndGroup)
import           System.Posix.IO           (FdOption (CloseOnExec), closeFd,
                                            createFile, fdWriteBuf, setFdOption)
import           System.Posix.Types        (GroupID, UserID)

data TempFolder = TempFolder
    { tfRoot    :: FilePath
    , tfCounter :: IORef Word
    }

setup :: FilePath -> IO TempFolder
setup fp = do
    e <- isDirectory fp
    when e $ removeTree fp
    createTree fp
    c <- I.newIORef minBound
    return $ TempFolder fp c

getFolder :: Maybe (UserID, GroupID) -> TempFolder -> Appname -> IO FilePath
getFolder muid TempFolder {..} appname = do
    !i <- I.atomicModifyIORef tfCounter $ \i -> (succ i, i)
    let fp = tfRoot </> fromText (appname ++ "-" ++ show i)
    createTree fp
    case muid of
        Nothing -> return ()
        Just (uid, gid) -> setOwnerAndGroup (F.encodeString fp) uid gid
    return fp

unpackTempTar :: Maybe (UserID, GroupID)
              -> TempFolder
              -> FilePath -- ^ bundle
              -> Appname
              -> (F.FilePath -> IO a)
              -> IO a
unpackTempTar muid tf bundle appname withDir = do
    lbs <- L.readFile $ F.encodeString bundle
    bracketOnError (getFolder muid tf appname) removeTree $ \dir -> do
        unpackTar muid dir $ Tar.read $ decompress lbs
        withDir dir

unpackTar :: Maybe (UserID, GroupID)
          -> FilePath -> Tar.Entries Tar.FormatError -> IO ()
unpackTar muid dir =
    loop . Tar.checkSecurity
  where
    loop Tar.Done = return ()
    loop (Tar.Fail e) = either throwIO throwIO e
    loop (Tar.Next e es) = go e >> loop es

    go e = do
        let fp = dir </> decodeString (Tar.entryPath e)
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> do
                case muid of
                    Nothing -> createTree $ F.directory fp
                    Just (uid, gid) -> createTreeUID uid gid $ F.directory fp
                let write fd bs = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
                        _ <- fdWriteBuf fd (castPtr ptr) (fromIntegral len)
                        return ()
                bracket
                    (do
                        fd <- createFile (F.encodeString fp) $ Tar.entryPermissions e
                        setFdOption fd CloseOnExec True
                        case muid of
                            Nothing -> return ()
                            Just (uid, gid) -> setFdOwnerAndGroup fd uid gid
                        return fd)
                    closeFd
                    (\fd -> mapM_ (write fd) (L.toChunks lbs))
            _ -> return ()

-- | Create a directory tree, setting the uid and gid of all newly created
-- folders.
createTreeUID :: UserID -> GroupID -> FilePath -> IO ()
createTreeUID uid gid =
    go
  where
    go fp = do
        exists <- F.isDirectory fp
        unless exists $ do
            go $ F.parent fp
            F.createDirectory False fp
            setOwnerAndGroup (F.encodeString fp) uid gid
