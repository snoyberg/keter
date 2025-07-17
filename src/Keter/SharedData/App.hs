{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Keter.SharedData.App
    ( App(..)
    , AppStartConfig(..)
    , RunningBackgroundApp(..)
    , RunningWebApp(..)
    , showApp
    ) where

import Control.Concurrent.STM (STM, TVar, readTVar)
import Data.Set (Set)
import Data.Text (Text, pack)
import Keter.Common (AppId, Host, Plugins, Port)
import Keter.Conduit.Process.Unix
       ( MonitoredProcess
       , ProcessTracker
       )
import Keter.Config (KeterConfig)
import Keter.HostManager (HostManager)
import Keter.Logger (Logger)
import Keter.PortPool (PortPool)
import Keter.TempTarball (TempFolder)
import System.Posix.Types (EpochTime, GroupID, UserID)

data RunningWebApp = RunningWebApp
    { rwaProcess            :: !MonitoredProcess
    , rwaPort               :: !Port
    , rwaEnsureAliveTimeOut :: !Int
    }

instance Show RunningWebApp where
  show (RunningWebApp {..})  = "RunningWebApp{rwaPort=" <> show rwaPort <> ", rwaEnsureAliveTimeOut=" <> show rwaEnsureAliveTimeOut <> ",..}"

newtype RunningBackgroundApp = RunningBackgroundApp
    { rbaProcess :: MonitoredProcess
    }

data AppStartConfig = AppStartConfig
    { ascTempFolder     :: !TempFolder
    , ascSetuid         :: !(Maybe (Text, (UserID, GroupID)))
    , ascProcessTracker :: !ProcessTracker
    , ascHostManager    :: !HostManager
    , ascPortPool       :: !PortPool
    , ascPlugins        :: !Plugins
    , ascKeterConfig    :: !KeterConfig
    }

data App = App
    { appModTime        :: !(TVar (Maybe EpochTime))
    , appRunningWebApps :: !(TVar [RunningWebApp])
    , appBackgroundApps :: !(TVar [RunningBackgroundApp])
    , appId             :: !AppId
    , appHosts          :: !(TVar (Set Host))
    , appDir            :: !(TVar (Maybe FilePath))
    , appAsc            :: !AppStartConfig
    , appLog           :: !(TVar (Maybe Logger))
    }

instance Show App where
  show App {appId} = "App{appId=" <> show appId <> "}"

-- | within an stm context we can show a lot more then the show instance can do
showApp :: App -> STM Text
showApp App{..} = do
  appModTime' <- readTVar appModTime
  appRunning' <- readTVar appRunningWebApps
  appHosts'   <- readTVar appHosts
  pure $ pack $
    show appId <>
    " modtime: " <> show appModTime' <>  ", webappsRunning: " <>  show appRunning' <> ", hosts: " <> show appHosts'
