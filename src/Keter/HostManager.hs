{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Applicative
import Control.Exception (assert, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask)
import Data.CaseInsensitive qualified as CI
import Data.Either (partitionEithers)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Keter.Common
import Keter.Config
import Keter.Context
import Keter.LabelMap (LabelMap)
import Keter.LabelMap qualified as LabelMap
import Network.TLS qualified as TLS
import Prelude hiding (log)
import System.FilePath (FilePath)

data HostValue = HVActive   !AppId !ProxyAction !TLS.Credentials
               | HVReserved !AppId

newtype HostManager = HostManager (IORef (LabelMap HostValue))

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
reserveHosts :: AppId
             -> Set.Set Host
             -> KeterM HostManager Reservations
reserveHosts aid hosts = do
  (HostManager mstate) <- ask
  $logInfo $ pack $
      "Reserving hosts for app "
      ++ show aid
      ++ ": "
      ++ unwords (map (unpack . CI.original) $ Set.toList hosts)
  liftIO $ either (throwIO . CannotReserveHosts aid) return
    =<< atomicModifyIORef mstate (\entries0 ->
      case partitionEithers $ map (checkHost entries0) $ Set.toList hosts of
          ([], Set.unions -> toReserve) ->
              (Set.foldr reserve entries0 toReserve, Right toReserve)
          (conflicts, _) -> (entries0, Left $ Map.fromList conflicts))
  where
    checkHost entries0 host =
        if LabelMap.labelAssigned hostBS entries0
        then
          (case LabelMap.lookup hostBS entries0 of
            Nothing -> Right $ Set.singleton host
            Just (HVReserved aid') -> assert (aid /= aid')
                                    $ Left (host, aid')
            Just (HVActive aid' _ _)
                | aid == aid' -> Right Set.empty
                | otherwise   -> Left (host, aid'))
        else Right $ Set.singleton host
      where hostBS = encodeUtf8 $ CI.original host

    hvres = HVReserved aid
    reserve host es =
        assert (not $ LabelMap.labelAssigned hostBS es) $ LabelMap.insert hostBS hvres es
      where
        hostBS = encodeUtf8 $ CI.original host

-- | Forget previously made reservations.
forgetReservations :: AppId
                   -> Reservations
                   -> KeterM HostManager ()
forgetReservations app hosts = do
    (HostManager mstate) <- ask
    $logInfo $ pack $
        "Forgetting host reservations for app "
        ++ show app
        ++ ": "
        ++ unwords (map (unpack . CI.original) $ Set.toList hosts)
    liftIO $ atomicModifyIORef mstate $ \state0 ->
        (Set.foldr forget state0 hosts, ())
  where
    forget host state =
        assert isReservedByMe $ LabelMap.delete hostBS state
      where
        hostBS = encodeUtf8 $ CI.original host
        isReservedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just HVActive{} -> False

-- | Activate a new app. Note that you /must/ first reserve the hostnames you'll be using.
activateApp :: AppId
            -> Map.Map Host (ProxyAction, TLS.Credentials)
            -> KeterM HostManager ()
activateApp app actions = do
    (HostManager mstate) <- ask
    $logInfo $ pack $ concat
        [ "Activating app "
        , show app
        , " with hosts: "
        , unwords (map (unpack . CI.original) $ Set.toList (Map.keysSet actions))
        ]
    liftIO $ atomicModifyIORef mstate $ \state0 ->
        (activateHelper app state0 actions, ())

activateHelper :: AppId -> LabelMap HostValue -> Map Host (ProxyAction, TLS.Credentials) -> LabelMap HostValue
activateHelper app =
    Map.foldrWithKey activate
  where
    activate host (action, cr) state =
        assert isOwnedByMe $ LabelMap.insert hostBS (HVActive app action cr) state
      where
        hostBS = encodeUtf8 $ CI.original host
        isOwnedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVReserved app') -> app == app'
                Just (HVActive app' _ _) -> app == app'

deactivateApp :: AppId
              -> Set Host
              -> KeterM HostManager ()
deactivateApp app hosts = do
    $logInfo $ pack $ "Deactivating app " ++ show app ++ " with hosts: " ++ unwords (map (unpack . CI.original) $ Set.toList hosts)
    (HostManager mstate) <- ask
    liftIO $ atomicModifyIORef mstate $ \state0 ->
        (deactivateHelper app state0 hosts, ())

deactivateHelper :: AppId -> LabelMap HostValue -> Set Host -> LabelMap HostValue
deactivateHelper app =
    Set.foldr deactivate
  where
    deactivate host state =
        assert isOwnedByMe $ LabelMap.delete hostBS state
      where
        hostBS = encodeUtf8 $ CI.original host
        isOwnedByMe = LabelMap.labelAssigned hostBS state &&
            case LabelMap.lookup hostBS state of
                Nothing -> False
                Just (HVActive app' _ _) -> app == app'
                Just HVReserved {} -> False

reactivateApp :: AppId
              -> Map Host (ProxyAction, TLS.Credentials)
              -> Set Host
              -> KeterM HostManager ()
reactivateApp app actions hosts = do
    (HostManager mstate) <- ask
    $logInfo $ pack $ concat
        [ "Reactivating app "
        , show app
        , ".  Old hosts: "
        , unwords (map (unpack . CI.original) $ Set.toList hosts)
        , ". New hosts: "
        , unwords (map (unpack . CI.original) $ Set.toList (Map.keysSet actions))
        , "."
        ]
    liftIO $ atomicModifyIORef mstate $ \state0 ->
        (activateHelper app (deactivateHelper app state0 hosts) actions, ())

lookupAction :: HostManager
             -> HostBS
             -> IO (Maybe (ProxyAction, TLS.Credentials))
lookupAction (HostManager mstate) host = do
    state <- readIORef mstate
    return $ case LabelMap.lookup (CI.original host) state of
        Nothing -> Nothing
        Just (HVActive _ action cert) -> Just (action, cert)
        Just (HVReserved _) -> Nothing
