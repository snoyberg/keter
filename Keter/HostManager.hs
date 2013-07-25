{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Keter.HostManager
    ( -- * Types
      HostManager
    , Reservations
    , Conflicts
      -- * Actions
    , reserveHosts
    , forgetReservations
    , activateApp
    , lookupAction
      -- * Initialize
    , start
    ) where

import           Control.Applicative
import qualified Control.Concurrent.MVar as M
import           Control.Exception       (assert)
import           Data.ByteString.Char8   ()
import           Data.Either             (partitionEithers)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Text.Encoding      (encodeUtf8)
import           Keter.Prelude
import           Keter.Types
import           Prelude                 (null)
import           Prelude                 (IO)

type HMState = Map.Map HostBS HostValue

data HostValue = HVActive !Appname !ProxyAction
               | HVReserved !Appname

newtype HostManager = HostManager (MVar HMState) -- FIXME use an IORef instead

type Conflicts = Map.Map Host Appname
type Reservations = Set.Set Host

start :: IO HostManager
start = HostManager <$> M.newMVar Map.empty

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
             -> Appname
             -> Set.Set Host
             -> KIO (Either Conflicts Reservations)
reserveHosts (HostManager mstate) app hosts = modifyMVar mstate $ \entries0 ->
    return $ case partitionEithers $ map (checkHost entries0) $ Set.toList hosts of
        ([], toReserve) ->
            (Set.foldr reserve entries0 $ Set.unions toReserve, Right Set.empty)
        (conflicts, _) -> (entries0, Left $ Map.fromList conflicts)
  where
    checkHost entries0 host =
        case Map.lookup (encodeUtf8 host) entries0 of
            Nothing -> Right $ Set.singleton host
            Just (HVReserved app') -> assert (app /= app')
                                    $ Left (host, app')
            Just (HVActive app' _)
                | app == app' -> Right Set.empty
                | otherwise   -> Left (host, app')

    hvres = HVReserved app
    reserve host es =
        assert (Map.notMember hostBS es) $ Map.insert hostBS hvres es
      where
        hostBS = encodeUtf8 host

-- | Forget previously made reservations.
forgetReservations :: HostManager
                   -> Appname
                   -> Reservations
                   -> KIO ()
forgetReservations (HostManager mstate) app hosts = modifyMVar_ mstate $ \state0 ->
    return $ Set.foldr forget state0 hosts
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
            -> Appname
            -> Map.Map Host ProxyAction
            -> KIO ()
activateApp (HostManager mstate) app actions = modifyMVar_ mstate $ \state0 ->
    return $ Map.foldrWithKey activate state0 actions
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
             -> KIO (Maybe ProxyAction)
lookupAction (HostManager mstate) host = withMVar mstate $ \state ->
    return $ case Map.lookup host state of
        Nothing -> Nothing
        Just (HVActive _ action) -> Just action
        Just (HVReserved _) -> Nothing
