{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Keter.Conduit.LogFile
    ( RotatingLog(..)
    , defaultRotationSpec
    , defaultMaxTotal
    , defaultBufferSize
    ) where

import qualified System.Log.FastLogger as FL

-- | Record wrapper over a fast logger (log,close) function tuple, just to make it less unwieldy and obscure.
data Logger = Logger
  { loggerLog :: forall a. FL.ToLogStr a => a -> IO ()
  , loggerClose :: IO () 
  }

defaultRotationSpec :: FilePath -> FL.FileLogSpec
defaultRotationSpec dir =
    FL.FileLogSpec dir defaultMaxTotal maxBound -- TODO: do we want to overwrite logs after a certain point? leaving this INT_MAX for now

defaultMaxTotal :: Integer
defaultMaxTotal = 5 * 1024 * 1024 -- 5 MB

defaultBufferSize :: Int
defaultBufferSize = 256 -- TODO: Reasonable value?
