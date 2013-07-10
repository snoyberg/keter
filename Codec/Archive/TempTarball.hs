{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Handles allocation of temporary directories and unpacking of bundles into
-- them. Sets owner and group of all created files and directories as
-- necessary.
module Codec.Archive.TempTarball
    ( TempFolder
    , setup
    , unpackTempTar
    ) where

import qualified Codec.Archive.Tar         as Tar
import qualified Codec.Archive.Tar.Check   as Tar
import qualified Codec.Archive.Tar.Entry   as Tar
import           Codec.Compression.GZip    (decompress)
import           Control.Exception         (bracket, bracketOnError, throwIO)
import           Control.Monad             (unless, when)
import qualified Data.ByteString.Lazy      as L
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import qualified Data.IORef                as I
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Data.Word                 (Word)
import qualified Filesystem                as F
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.Ptr               (castPtr)
import           System.Posix.Files        (setFdOwnerAndGroup,
                                            setOwnerAndGroup)
import           System.Posix.IO           (FdOption (CloseOnExec), closeFd,
                                            createFile, fdWriteBuf, setFdOption)
import           System.Posix.Types        (GroupID, UserID)

data TempFolder = TempFolder
    { tfRoot    :: F.FilePath
    , tfCounter :: I.IORef Word
    }

setup :: F.FilePath -> IO TempFolder
setup fp = do
    e <- F.isDirectory fp
    when e $ F.removeTree fp
    F.createTree fp
    c <- I.newIORef minBound
    return $ TempFolder fp c

getFolder :: Maybe (UserID, GroupID)
          -> TempFolder
          -> Text -- ^ prefix for folder name
          -> IO F.FilePath
getFolder muid TempFolder {..} appname = do
    !i <- I.atomicModifyIORef tfCounter $ \i -> (succ i, i)
    let fp = tfRoot </> F.fromText (appname <> "-" <> pack (show i))
    F.createTree fp
    case muid of
        Nothing -> return ()
        Just (uid, gid) -> setOwnerAndGroup (F.encodeString fp) uid gid
    return fp

unpackTempTar :: Maybe (UserID, GroupID)
              -> TempFolder
              -> F.FilePath -- ^ bundle
              -> Text -- ^ prefix for folder name
              -> (F.FilePath -> IO a)
              -> IO a
unpackTempTar muid tf bundle appname withDir = do
    lbs <- L.readFile $ F.encodeString bundle
    bracketOnError (getFolder muid tf appname) F.removeTree $ \dir -> do
        unpackTar muid dir $ Tar.read $ decompress lbs
        withDir dir

unpackTar :: Maybe (UserID, GroupID)
          -> F.FilePath
          -> Tar.Entries Tar.FormatError
          -> IO ()
unpackTar muid dir =
    loop . Tar.checkSecurity
  where
    loop Tar.Done = return ()
    loop (Tar.Fail e) = either throwIO throwIO e
    loop (Tar.Next e es) = go e >> loop es

    go e = do
        let fp = dir </> F.decodeString (Tar.entryPath e)
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> do
                case muid of
                    Nothing -> F.createTree $ F.directory fp
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
createTreeUID :: UserID -> GroupID -> F.FilePath -> IO ()
createTreeUID uid gid =
    go
  where
    go fp = do
        exists <- F.isDirectory fp
        unless exists $ do
            go $ F.parent fp
            F.createDirectory False fp
            setOwnerAndGroup (F.encodeString fp) uid gid
