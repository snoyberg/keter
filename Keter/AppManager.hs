{-# LANGUAGE RecordWildCards #-}
-- | Used for management of applications.
module Keter.AppManager
    ( -- * Types
      AppManager
    , AppId (..)
    , Action (..)
    , AppInput (..)
      -- * Actions
    , perform
    , getAllApps
      -- * Initialize
    , initialize
    ) where

import           Control.Applicative
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import qualified Control.Exception      as E
import           Control.Monad          (void)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Keter.Types

data AppManager = AppManager
    { apps :: TVar (Map AppId (TVar AppState))
    }

data AppId = AIBuiltin | AINamed Appname
    deriving (Eq, Ord)

data AppState = ASRunning RunningApp
              | ASStarting !(Maybe RunningApp) (TVar (Maybe Action)) -- ^ the next one to try
              | ASTerminated

data RunningApp = RunningApp

data AppInput = AIBundle | AIData !BundleConfig

data Action = Reload AppInput | Terminate

initialize :: IO AppManager
initialize = AppManager
    <$> newTVarIO Map.empty

getAllApps :: AppManager -> IO (Set Appname)
getAllApps AppManager {..} = atomically $ do
    m <- readTVar apps
    return $ Set.fromList $ mapMaybe toAppName $ Map.keys m
  where
    toAppName AIBuiltin = Nothing
    toAppName (AINamed x) = Just x

perform :: AppManager -> AppId -> Action -> IO ()
perform am@AppManager {..} aid action = E.mask_ $ do
    launchWorker' <- atomically $ do
        m <- readTVar apps
        case Map.lookup aid m of
            Just tstate -> do
                state <- readTVar tstate
                case state of
                    ASStarting mcurrent tmnext -> do
                        writeTVar tmnext $ Just action
                        -- use the previous worker, so nothing to do
                        return noWorker
                    ASRunning runningApp -> do
                        tmnext <- newTVar Nothing
                        writeTVar tstate $ ASStarting (Just runningApp) tmnext
                        return $ launchWorker am tstate tmnext action
                    ASTerminated -> onNotRunning
            Nothing -> onNotRunning
    launchWorker'
  where
    noWorker = return ()

    onNotRunning =
        case action of
            Reload _ -> do
                tmnext <- newTVar Nothing
                tstate <- newTVar $ ASStarting Nothing tmnext
                modifyTVar apps $ Map.insert aid tstate
                return $ launchWorker am tstate tmnext action
            Terminate -> return noWorker

launchWorker :: AppManager
             -> TVar AppState
             -> TVar (Maybe Action)
             -> Action
             -> IO ()
launchWorker AppManager {..} tstate tmnext action0 = void $ forkIO $ do
    loop action0
  where
    loop action = do
        mRunningApp <- processAction action
        mnext <- atomically $ do
            mnext <- readTVar tmnext
            writeTVar tmnext Nothing
            case mnext of
                Nothing ->
                    case mRunningApp of
                        Nothing -> writeTVar tstate ASTerminated
                        Just runningApp -> writeTVar tstate $ ASRunning runningApp
                Just next -> writeTVar tstate $ ASStarting mRunningApp tmnext
            return mnext
        case mnext of
            Nothing -> return ()
            Just next -> loop action

    processAction _ = return Nothing -- FIXME

{- FIXME
            rest <-
                case Map.lookup appname appMap of
                    Just (app, _time) -> do
                        App.reload app
                        etime <- liftIO $ modificationTime <$> getFileStatus (F.encodeString bundle)
                        let time = either (P.const 0) id etime
                        return (Map.insert appname (app, time) appMap, return ())
                    Nothing -> do
                        mlogger <- do
                            let dirout = kconfigDir </> "log" </> fromText ("app-" ++ appname)
                                direrr = dirout </> "err"
                            erlog <- liftIO $ LogFile.openRotatingLog
                                (F.encodeString dirout)
                                LogFile.defaultMaxTotal
                            case erlog of
                                Left e -> do
                                    $logEx e
                                    return Nothing
                                Right rlog -> return (Just rlog)
                        let logger = fromMaybe LogFile.dummy mlogger
                        (app, rest) <- App.start
                            tf
                            muid
                            processTracker
                            hostman
                            plugins
                            logger
                            appname
                            bundle
                            (removeApp appname)
                        etime <- liftIO $ modificationTime <$> getFileStatus (F.encodeString bundle)
                        let time = either (P.const 0) id etime
                        let appMap' = Map.insert appname (app, time) appMap
                        return (appMap', rest)
            rest
            -}
