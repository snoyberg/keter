{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Keter.HostManager
    ( -- * Types
      HostManager
    , Reservations
      -- * Actions
    , reserveHosts
    , forgetReservations
    , activateApp
    , lookupAction
      -- * Initialize
    , start
    ) where

import           Control.Applicative
import           Control.Exception       (assert, throwIO)
import           Data.Either             (partitionEithers)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Text.Encoding      (encodeUtf8)
import           Keter.Types
import           Data.IORef

type HMState = Map.Map HostBS HostValue

data HostValue = HVActive !AppId !ProxyAction
               | HVReserved !AppId

newtype HostManager = HostManager (IORef HMState)

type Reservations = Set.Set Host

start :: IO HostManager
start = HostManager <$> newIORef Map.empty

-- | Reserve the given hosts so that no other application may use them. Does
-- not yet enable any action. The semantics are:
--
-- 1. If a requested host is currently actively used or by an app of the same name, it is
--    considered reserved.
--
-- 2. If a requested host is currently reserved by an app of the same name, it
--    is considered an error in calling this API. Only one app reservation can
--    happen at a time.
--
-- 3. If any requested host is currently used or reserved by an app with a
--    different name, then those values are returned as @Left@.
--
-- 4. Otherwise, the hosts which were reserved are returned as @Right@. This
--    does /not/ include previously active hosts.
reserveHosts :: HostManager
             -> AppId
             -> Set.Set Host
             -> IO Reservations
reserveHosts (HostManager mstate) aid hosts = either (throwIO . CannotReserveHosts aid) return =<< atomicModifyIORef mstate (\entries0 ->
    case partitionEithers $ map (checkHost entries0) $ Set.toList hosts of
        ([], toReserve) ->
            (Set.foldr reserve entries0 $ Set.unions toReserve, Right Set.empty)
        (conflicts, _) -> (entries0, Left $ Map.fromList conflicts))
  where
    checkHost entries0 host =
        case Map.lookup (encodeUtf8 host) entries0 of
            Nothing -> Right $ Set.singleton host
            Just (HVReserved aid') -> assert (aid /= aid')
                                    $ Left (host, aid')
            Just (HVActive aid' _)
                | aid == aid' -> Right Set.empty
                | otherwise   -> Left (host, aid')

    hvres = HVReserved aid
    reserve host es =
        assert (Map.notMember hostBS es) $ Map.insert hostBS hvres es
      where
        hostBS = encodeUtf8 host

-- | Forget previously made reservations.
forgetReservations :: HostManager
                   -> AppId
                   -> Reservations
                   -> IO ()
forgetReservations (HostManager mstate) app hosts = atomicModifyIORef mstate $ \state0 ->
    (Set.foldr forget state0 hosts, ())
  where
    forget host state =
        assert isReservedByMe $ Map.delete hostBS state
      where
        hostBS = encodeUtf8 host
        isReservedByMe =
            case Map.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just HVActive{} -> False

-- | Activate a new app. Note that you /must/ first reserve the hostnames you'll be using.
activateApp :: HostManager
            -> AppId
            -> Map.Map Host ProxyAction
            -> IO ()
activateApp (HostManager mstate) app actions = atomicModifyIORef mstate $ \state0 ->
    (Map.foldrWithKey activate state0 actions, ())
  where
    activate host action state =
        assert isOwnedByMe $ Map.insert hostBS (HVActive app action) state
      where
        hostBS = encodeUtf8 host
        isOwnedByMe =
            case Map.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just (HVActive app' _) -> app == app'

lookupAction :: HostManager
             -> HostBS
             -> IO (Maybe ProxyAction)
lookupAction (HostManager mstate) host = do
    state <- readIORef mstate
    return $ case Map.lookup host state of
        Nothing -> Nothing
        Just (HVActive _ action) -> Just action
        Just (HVReserved _) -> Nothing
