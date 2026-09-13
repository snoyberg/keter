-- Copyright (c) 2017-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-| A library for safely interacting with frequently updating shared
objects within a haskell binary. See more documentation at <https://github.com/fbsamples/ghc-hotswap/>.

Assuming you have some structure of data called `Foo`.

In common types:
```
type FooExport = IO (StablePtr Foo)
```

In the shared object:
```
foreign export ccall "hs_mySOFunction"
  hsHandle :: FooExport

hsHandle :: FooExport
hsHandle = newStablePtr Foo
  { ...
  }
```

In the main binary:
```
main = do
  myData <- registerHotswap "hs_mySOFunction" "/path/to/lib.o"
  (withSO myData) $ \Foo{..} -> do
    -- first version
    ...

  (swapSO myData) "/path/to/next_lib.o"
  (withSO myData) $ \Foo{..} -> do
    -- next version
    ...
```
-}

module Keter.GHC.Hotswap
  ( UpdatableSO
  , swapSO
  , withSO
  , registerHotswap
  ) where

import qualified Control.Concurrent.ReadWriteLock as L
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad

import GHCi.ObjLink
import Foreign

-- | Access control for a shared object that return a type `a` from the
-- shared object
data UpdatableSO a = UpdatableSO
  { swapSO :: FilePath -> IO ()
    -- ^ Loads and links the new object such that future calls to `withSO` will
    -- use the new objects. Existing calls in the old object will complete as
    -- normal and the old object will be unloaded when all references to it
    -- are dropped.
    -- The underlying work is not thread safe, so it's on the caller to
    -- appropriately serialize these calls to avoid accidentally skipping an
    -- update.
  , withSO :: forall b . (a -> IO b) -> IO b
    -- ^ Accessor for information out of the shared object. Use this to run
    -- something with data from the latest shared object. You are guaranteed
    -- to access the latest object and the object will be retained until
    -- the call finishes.
    -- Always eventually return from calling this function, otherwise
    -- objects will not be dropped.
  }

-- | Internal state associated with a single instance of a shared object
data SOState a = SOState
  { lock :: L.RWLock -- Protects the data so we know when to safely delete
  , path :: FilePath -- The local path to the object
  , val :: a         -- The extracted value we wanted
  }

-- | Loads a shared object, pulls out the particular symbol name, and returns
-- a control structure for interacting with the data
registerHotswap
  :: NFData a
  => String         -- exported c-name of the (:: IO (StablePtr a)) symbol
  -> FilePath       -- path to the first instance of the shared object
  -> IO (UpdatableSO a)  -- control structure
registerHotswap symbolName firstPath = do
  firstVal <- force <$> loadNewSO symbolName firstPath
  firstLock <- L.new
  sMVar <- newMVar SOState
    { lock = firstLock
    , path = firstPath
    , val = firstVal
    }
  return UpdatableSO
    { swapSO = updateState sMVar symbolName
    , withSO = unWrap sMVar
    }

-- | Safely runs an action on a value from the shared object
unWrap :: MVar (SOState a) -> (a -> IO b) -> IO b
unWrap mvar action = do
  SOState{..} <- readMVar mvar
  L.withRead lock $ action val

-- | Safely updates the state to handle an updated shared object
updateState
  :: NFData a
  => MVar (SOState a)  -- State to edit
  -> String            -- exported c-name of the symbol to lookup
  -> FilePath          -- path to the next instance of the shared object
  -> IO ()
updateState mvar symbolName nextPath = do
  newVal <- force <$> loadNewSO symbolName nextPath
  -- Build a new state for this version
  newLock <- L.new
  let
    newState = SOState
      { lock = newLock
      , path = nextPath
      , val = newVal
      }
  -- Swapping in the new state means all new calls to `withSO` from the client
  -- will use the new value. After this it's impossible for a new read lock to
  -- grab the old state
  oldState <- swapMVar mvar newState
  -- All readers in oldState will fall out, so we're safe to destroy state here
  L.withWrite (lock oldState) $
    unloadObj (path oldState)

-- Extract the function pointer as a callable Haskell function
foreign import ccall "dynamic"
  callExport :: FunPtr (IO (StablePtr a)) -> IO (StablePtr a)

-- | Nuts and bolts for bringing in a new object
loadNewSO :: String -> FilePath -> IO a
loadNewSO symName newSO = do
  -- initObjLinker is idempotent
  initObjLinker DontRetainCAFs

  loadObj newSO
  resolved <- resolveObjs
  unless resolved $ do
    unloadObj newSO
    throwIO (ErrorCall $ "Unable to resolve objects for " ++ newSO)
  c_sym <- lookupSymbol symName
  h <- case c_sym of
    Nothing -> do
      unloadObj newSO
      throwIO (ErrorCall "Could not find symbol")
    Just p_sym ->
      bracket (callExport $ castPtrToFunPtr p_sym) freeStablePtr deRefStablePtr
  -- Dump the symbol table to make room for when the next object comes in
  purgeObj newSO
  return h
