{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , logEx
    , KIO
    , toString
    , P.map
    , (A.***)
    , readFileLBS
    , P.Either (..)
    , P.either
    , E.SomeException
    , runKIO
    , void
    , liftIO
    , forkKIO
    , forkKIO'
    , (++)
    , P.minBound
    , P.succ
    , show
    , Control.Monad.when
    , fromText
    , P.flip
    , P.Show
    , KeterException (..)
    , E.toException
    , newStdGen
    , Default (..)
    , P.Int
    , (P.&&)
    , (P.==)
    , (P./=)
    , (P.*)
    , P.fromIntegral
    , P.reverse
    , P.otherwise
    , timeout
    , threadDelay
    , P.id
    , P.filter
    , P.mapM_
    , P.fmap
    , P.not
    , P.maybe
    , (P.>)
    , (P.<)
    , (P.<=)
    , (P.+)
    , (P.-)
    , getCurrentTime
      -- * Filepath
    , (F.</>)
    , (F.<.>)
    , F.FilePath
    , F.isDirectory
    , F.isFile
    , F.removeTree
    , F.createTree
    , F.directory
    , F.rename
    , F.basename
    , F.toText
    , F.hasExtension
    , F.listDirectory
    , F.decodeString
      -- * MVar
    , M.MVar
    , newMVar
    , newEmptyMVar
    , modifyMVar
    , modifyMVar_
    , swapMVar
    , takeMVar
    , tryTakeMVar
    , putMVar
      -- * IORef
    , I.IORef
    , newIORef
    , atomicModifyIORef
      -- * Chan
    , C.Chan
    , newChan
    , readChan
    , writeChan
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
import Control.Concurrent (forkIO, ThreadId)
import qualified Control.Concurrent
import qualified Data.IORef as I
import Data.Monoid (Monoid, mappend)
import qualified Data.Text.Lazy.Builder as B
import Data.Typeable (Typeable)
import qualified Control.Concurrent.Chan as C
import qualified System.Random as R
import Data.Default (Default (..))
import System.Exit (ExitCode)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8
import qualified System.Timeout
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Time

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
    | InvalidBundle F.FilePath E.SomeException
    | ProcessDidNotStart F.FilePath
    | ExceptionThrown T.Text E.SomeException
    | RemovingPort P.Int
    | UnpackingBundle F.FilePath
    | TerminatingApp T.Text
    | FinishedReloading T.Text
    | TerminatingOldProcess T.Text
    | RemovingOldFolder F.FilePath
    | ReceivedInotifyEvent T.Text
    | ProcessWaiting F.FilePath

instance P.Show LogMessage where
    show (ProcessCreated f) = "Created process: " ++ F.encodeString f
    show (InvalidBundle f e) = P.concat
        [ "Unable to parse bundle file '"
        , F.encodeString f
        , "': "
        , P.show e
        ]
    show (ProcessDidNotStart fp) = P.concat
        [ "Could not start process within timeout period: "
        , F.encodeString fp
        ]
    show (ExceptionThrown t e) = P.concat
        [ T.unpack t
        , ": "
        , P.show e
        ]
    show (RemovingPort p) = "Port in use, removing from port pool: " ++ P.show p
    show (UnpackingBundle b) = P.concat
        [ "Unpacking bundle '"
        , F.encodeString b
        , "'"
        ]
    show (TerminatingApp t) = "Shutting down app: " ++ T.unpack t
    show (FinishedReloading t) = "App finished reloading: " ++ T.unpack t
    show (TerminatingOldProcess t) = "Sending old process TERM signal: " ++ T.unpack t
    show (RemovingOldFolder fp) = "Removing unneeded folder: " ++ F.encodeString fp
    show (ReceivedInotifyEvent t) = "Received unknown INotify event: " ++ T.unpack t
    show (ProcessWaiting f) = "Process restarting too quickly, waiting before trying again: " ++ F.encodeString f

logEx :: TH.Q TH.Exp
logEx = do
    let showLoc TH.Loc { TH.loc_module = m, TH.loc_start = (l, c) } = P.concat
            [ m
            , ":"
            , P.show l
            , ":"
            , P.show c
            ]
    loc <- P.fmap showLoc TH.qLocation
    [|log P.. ExceptionThrown (T.pack $(TH.lift loc))|]

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

newEmptyMVar :: KIO (M.MVar a)
newEmptyMVar = liftIO_ M.newEmptyMVar

modifyMVar :: M.MVar a -> (a -> KIO (a, b)) -> KIO b
modifyMVar m f = KIO $ \x -> M.modifyMVar m (\a -> unKIO (f a) x)

modifyMVar_ :: M.MVar a -> (a -> KIO a) -> KIO ()
modifyMVar_ m f = KIO $ \x -> M.modifyMVar_ m (\a -> unKIO (f a) x)

swapMVar :: M.MVar a -> a -> KIO a
swapMVar m = liftIO_ . M.swapMVar m

takeMVar :: M.MVar a -> KIO a
takeMVar = liftIO_ . M.takeMVar

tryTakeMVar :: M.MVar a -> KIO (P.Maybe a)
tryTakeMVar = liftIO_ . M.tryTakeMVar

putMVar :: M.MVar a -> a -> KIO ()
putMVar m = liftIO_ . M.putMVar m

forkKIO :: KIO () -> KIO ()
forkKIO = void . forkKIO'

forkKIO' :: KIO () -> KIO (P.Either E.SomeException ThreadId)
forkKIO' f = do
    x <- KIO P.return
    liftIO $ forkIO $ unKIO f x

newIORef :: a -> KIO (I.IORef a)
newIORef = liftIO_ . I.newIORef

atomicModifyIORef :: I.IORef a -> (a -> (a, b)) -> KIO b
atomicModifyIORef x = liftIO_ . I.atomicModifyIORef x

(++) :: Monoid m => m -> m -> m
(++) = mappend

show :: P.Show a => a -> T.Text
show = T.pack . P.show

class FromText a where
    fromText :: T.Text -> a
instance FromText T.Text where
    fromText = P.id
instance FromText F.FilePath where
    fromText = F.fromText
instance FromText B.Builder where
    fromText = B.fromText
instance FromText Blaze.Builder where
    fromText = Blaze.ByteString.Builder.Char.Utf8.fromText

data KeterException = CannotParsePostgres F.FilePath
                    | ExitCodeFailure F.FilePath ExitCode
                    | NoPortsAvailable
                    | InvalidConfigFile
    deriving (P.Show, Typeable)
instance E.Exception KeterException

newChan :: KIO (C.Chan a)
newChan = liftIO_ C.newChan

newStdGen :: KIO R.StdGen
newStdGen = liftIO_ R.newStdGen

readChan :: C.Chan a -> KIO a
readChan = liftIO_ . C.readChan

writeChan :: C.Chan a -> a -> KIO ()
writeChan c = liftIO_ . C.writeChan c

timeout :: P.Int -> KIO a -> KIO (P.Maybe a)
timeout seconds (KIO f) = KIO $ \x -> System.Timeout.timeout seconds $ f x

threadDelay :: P.Int -> KIO ()
threadDelay = liftIO_ . Control.Concurrent.threadDelay

getCurrentTime :: KIO Data.Time.UTCTime
getCurrentTime = liftIO_ Data.Time.getCurrentTime
