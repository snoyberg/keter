{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Keter.Conduit.LogFile
    ( RotatingLog(..)
    , rotationSpec
    , openRotatingLog
    , addChunk
    , close
    , defaultMaxTotal
    , defaultBufferSize
    ) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception              (bracket, bracketOnError,
                                                 finally)
import           Control.Monad                  (void, when)
import qualified Data.ByteString                as S
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Word                      (Word)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist, renameFile)
import           System.FilePath                ((<.>), (</>))
import qualified System.IO                      as SIO
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Mem.Weak                (addFinalizer)
import qualified System.Log.FastLogger          as FL

data Command = AddChunk !S.ByteString
             | Close

-- | Represents a folder used for totating log files.
--
-- Since 0.2.1
data RotatingLog = RotatingLog
  { rlLog :: forall a. FL.ToLogStr a => a -> IO ()
  , rlClose :: IO () 
  }

rotationSpec :: FilePath -> FL.FileLogSpec
rotationSpec dir =
    FL.FileLogSpec dir defaultMaxTotal maxBound -- TODO: do we want to overwrite logs after a certain point? leaving this INT_MAX for now

defaultMaxTotal :: Integer
defaultMaxTotal = 5 * 1024 * 1024 -- 5 MB

defaultBufferSize :: Int
defaultBufferSize = 256 -- TODO: Reasonable value?
