{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Keter.Prelude
    ( T.Text
    , String
    , P.Monad (..)
    , P.Maybe (..)
    , P.Bool (..)
    , (P.$)
    , (P..)
    , LogMessage (..)
    , log
    , KIO
    , toString
    , P.map
    , (A.***)
    , readFileLBS
    , P.Either (..)
    , E.SomeException
    , runKIO
    , void
    , liftIO
    , forkKIO
    , (++)
    , P.minBound
    , P.succ
    , show
    , Control.Monad.when
      -- * Filepath
    , (F.</>)
    , F.fromText
    , F.FilePath
    , F.isDirectory
    , F.removeTree
    , F.createTree
      -- * MVar
    , M.MVar
    , newMVar
    , modifyMVar
    , swapMVar
      -- * IORef
    , I.IORef
    , newIORef
    , atomicModifyIORef
    ) where

import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import qualified Data.Text as T
import qualified Prelude as P
import qualified Control.Arrow as A
import qualified Data.ByteString.Lazy as L
import Prelude (($), (.))
import qualified Control.Exception as E
import qualified Control.Monad
import qualified Control.Applicative
import qualified Control.Concurrent.MVar as M
import Control.Concurrent (forkIO)
import qualified Data.IORef as I
import Data.Monoid (Monoid, mappend)

type String = T.Text

newtype KIO a = KIO { unKIO :: (LogMessage -> P.IO ()) -> P.IO a }

instance P.Monad KIO where
    return = KIO . P.const . P.return
    KIO x >>= y = KIO $ \f -> do
        x' <- x f
        let KIO mz = y x'
        mz f

instance P.Functor KIO where
    fmap = Control.Monad.liftM
instance Control.Applicative.Applicative KIO where
    (<*>) = Control.Monad.ap
    pure = P.return

log :: LogMessage -> KIO ()
log msg = do
    f <- getLogger
    void $ liftIO $ f msg
  where
    getLogger = KIO P.return

void :: P.Monad m => m a -> m ()
void f = f P.>> P.return ()

data LogMessage
    = ProcessCreated F.FilePath
    | InvalidBundle F.FilePath
    | ProcessDidNotStart F.FilePath
    | ExceptionThrown E.SomeException
  deriving P.Show

class ToString a where
    toString :: a -> P.String

instance ToString P.String where
    toString = P.id
instance ToString T.Text where
    toString = T.unpack
instance ToString F.FilePath where
    toString = F.encodeString

readFileLBS :: F.FilePath -> KIO (P.Either E.SomeException L.ByteString)
readFileLBS = liftIO . L.readFile P.. toString

liftIO :: P.IO a -> KIO (P.Either E.SomeException a)
liftIO = KIO . P.const . E.try

liftIO_ :: P.IO a -> KIO a
liftIO_ = KIO . P.const

runKIO :: (LogMessage -> P.IO ()) -> KIO a -> P.IO a
runKIO f (KIO g) = g f

newMVar :: a -> KIO (M.MVar a)
newMVar = liftIO_ . M.newMVar

modifyMVar :: M.MVar a -> (a -> KIO (a, b)) -> KIO b
modifyMVar m f = KIO $ \x -> M.modifyMVar m (\a -> unKIO (f a) x)

swapMVar :: M.MVar a -> a -> KIO a
swapMVar m = liftIO_ . M.swapMVar m

forkKIO :: KIO () -> KIO ()
forkKIO f = do
    x <- KIO P.return
    void $ liftIO $ forkIO $ unKIO f x

newIORef :: a -> KIO (I.IORef a)
newIORef = liftIO_ . I.newIORef

atomicModifyIORef :: I.IORef a -> (a -> (a, b)) -> KIO b
atomicModifyIORef x = liftIO_ . I.atomicModifyIORef x

(++) :: Monoid m => m -> m -> m
(++) = mappend

show :: P.Show a => a -> T.Text
show = T.pack . P.show
