{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
module Keter.HostManager
    ( -- * Types
      HostManager
    , Reservations
      -- * Actions
    , reserveHosts
    , forgetReservations
    , activateApp
    , deactivateApp
    , reactivateApp
    , lookupAction
      -- * Initialize
    , start
    ) where

import           Control.Applicative
import           Control.Exception   (assert, throwIO)
import           Data.Either         (partitionEithers)
import           Data.IORef
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text.Encoding  (encodeUtf8)
import           Keter.Types
import           Keter.LabelMap      (LabelMap)
import qualified Keter.LabelMap      as LabelMap

type HMState = LabelMap HostValue

data HostValue = HVActive   !AppId !ProxyAction
               | HVReserved !AppId

newtype HostManager = HostManager (IORef HMState)

type Reservations = Set.Set Host

start :: IO HostManager
start = HostManager <$> newIORef LabelMap.empty

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
reserveHosts :: (LogMessage -> IO ())
             -> HostManager
             -> AppId
             -> Set.Set Host
             -> IO Reservations
reserveHosts log (HostManager mstate) aid hosts = do
  log $ ReservingHosts aid hosts
  either (throwIO . CannotReserveHosts aid) return =<< atomicModifyIORef mstate (\entries0 ->
    case partitionEithers $ map (checkHost entries0) $ Set.toList hosts of
        ([], Set.unions -> toReserve) ->
            (Set.foldr reserve entries0 toReserve, Right toReserve)
        (conflicts, _) -> (entries0, Left $ Map.fromList conflicts))
  where
    checkHost entries0 host =
        case LabelMap.labelAssigned hostBS entries0 of
            False -> Right $ Set.singleton host
            True  -> 
              case LabelMap.lookup hostBS entries0 of
                Nothing -> Right $ Set.singleton host
                Just (HVReserved aid') -> assert (aid /= aid')
                                        $ Left (host, aid')
                Just (HVActive aid' _)
                    | aid == aid' -> Right Set.empty
                    | otherwise   -> Left (host, aid')
      where hostBS = encodeUtf8 host

    hvres = HVReserved aid
    reserve host es =
        assert (not $ LabelMap.labelAssigned hostBS es) $ LabelMap.insert hostBS hvres es
      where
        hostBS = encodeUtf8 host

-- | Forget previously made reservations.
forgetReservations :: (LogMessage -> IO ())
                   -> HostManager
                   -> AppId
                   -> Reservations
                   -> IO ()
forgetReservations log (HostManager mstate) app hosts = do
    log $ ForgetingReservations app hosts
    atomicModifyIORef mstate $ \state0 ->
        (Set.foldr forget state0 hosts, ())
  where
    forget host state =
        assert isReservedByMe $ LabelMap.delete hostBS state
      where
        hostBS = encodeUtf8 host
        isReservedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just HVActive{} -> False

-- | Activate a new app. Note that you /must/ first reserve the hostnames you'll be using.
activateApp :: (LogMessage -> IO ())
            -> HostManager
            -> AppId
            -> Map.Map Host ProxyAction
            -> IO ()
activateApp log (HostManager mstate) app actions = do
    log $ ActivatingApp app $ Map.keysSet actions
    atomicModifyIORef mstate $ \state0 ->
        (activateHelper app state0 actions, ())

activateHelper :: AppId -> HMState -> Map Host ProxyAction -> HMState
activateHelper app =
    Map.foldrWithKey activate
  where
    activate host action state =
        assert isOwnedByMe $ LabelMap.insert hostBS (HVActive app action) state
      where
        hostBS = encodeUtf8 host
        isOwnedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just (HVActive app' _) -> app == app'

deactivateApp :: (LogMessage -> IO ())
              -> HostManager
              -> AppId
              -> Set Host
              -> IO ()
deactivateApp log (HostManager mstate) app hosts = do
    log $ DeactivatingApp app hosts
    atomicModifyIORef mstate $ \state0 ->
        (deactivateHelper app state0 hosts, ())

deactivateHelper :: AppId -> HMState -> Set Host -> HMState
deactivateHelper app =
    Set.foldr deactivate
  where
    deactivate host state =
        assert isOwnedByMe $ LabelMap.delete hostBS state
      where
        hostBS = encodeUtf8 host
        isOwnedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVActive app' _) -> app == app'
                Just HVReserved {} -> False

reactivateApp :: (LogMessage -> IO ())
              -> HostManager
              -> AppId
              -> Map Host ProxyAction
              -> Set Host
              -> IO ()
reactivateApp log (HostManager mstate) app actions hosts = do
    log $ ReactivatingApp app hosts (Map.keysSet actions)
    atomicModifyIORef mstate $ \state0 ->
        (activateHelper app (deactivateHelper app state0 hosts) actions, ())

lookupAction :: HostManager
             -> HostBS
             -> IO (Maybe ProxyAction)
lookupAction (HostManager mstate) host = do
    state <- readIORef mstate
    return $ case LabelMap.lookup host state of
        Nothing -> Nothing
        Just (HVActive _ action) -> Just action
        Just (HVReserved _) -> Nothing
