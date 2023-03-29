{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Keter.Conduit.LogFile
    ( RotatingLog(..)
    , defaultRotationSpec
    , defaultMaxTotal
    , defaultBufferSize
    ) where

import qualified System.Log.FastLogger as FL

-- | Represents a folder used for totating log files.
--
-- Since 0.2.1
data RotatingLog = RotatingLog
  { rlLog :: forall a. FL.ToLogStr a => a -> IO ()
  , rlClose :: IO () 
  }

defaultRotationSpec :: FilePath -> FL.FileLogSpec
defaultRotationSpec dir =
    FL.FileLogSpec dir defaultMaxTotal maxBound -- TODO: do we want to overwrite logs after a certain point? leaving this INT_MAX for now

defaultMaxTotal :: Integer
defaultMaxTotal = 5 * 1024 * 1024 -- 5 MB

defaultBufferSize :: Int
defaultBufferSize = 256 -- TODO: Reasonable value?
