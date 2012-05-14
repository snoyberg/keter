{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Keter.Prelude
    ( F.FilePath
    , T.Text
    , String
    , P.Monad (..)
    , P.Maybe (..)
    , P.Bool (..)
    , (P.$)
    , (P..)
    , LogMessage (..)
    , log
    , IO
    , toString
    , P.map
    , (A.***)
    , readFileLBS
    ) where

import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T
import qualified Prelude as P
import qualified Control.Arrow as A
import qualified Data.ByteString.Lazy as L

type String = T.Text
type IO = P.IO -- FIXME

log :: LogMessage -> IO ()
log = P.print

data LogMessage
    = ProcessCreated F.FilePath
    | InvalidBundle F.FilePath
    | ProcessDidNotStart F.FilePath
  deriving P.Show

class ToString a where
    toString :: a -> P.String

instance ToString P.String where
    toString = P.id
instance ToString T.Text where
    toString = T.unpack
instance ToString F.FilePath where
    toString = F.encodeString

readFileLBS :: F.FilePath -> IO L.ByteString
readFileLBS = L.readFile P.. toString
