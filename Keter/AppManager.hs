{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Keter.Prelude
import           Keter.Types
import           Prelude                (Eq, IO, Ord)
import qualified Control.Exception as E

data AppManager = AppManager
    { apps :: TVar (Map AppId (TVar AppState))
    }

data AppId = AIBuiltin | AINamed Appname
    deriving (Eq, Ord)

data AppState = ASRunning RunningApp
              | ASStarting !(Maybe RunningApp) (TVar (Maybe Request)) -- ^ the next one to try

data RunningApp = RunningApp

data AppInput = AIBundle | AIData !BundleConfig

data Action = Reload AppInput | Terminate

data Request = Request Action (TMVar Result)

data Result = ResultSuccess
            | ResultFailure SomeException
            | ResultCanceled

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
perform AppManager {..} aid action = E.mask_ $ do
    (launchWorker', tresult) <- atomically $ do
        tresult <- newEmptyTMVar
        let request = Request action tresult
        m <- readTVar apps
        launchWorker' <- case Map.lookup aid m of
            Just tstate -> do
                state <- readTVar tstate
                case state of
                    ASStarting mcurrent tmnext -> do
                        mnext <- readTVar tmnext
                        case mnext of
                            Nothing -> return ()
                            Just (Request _ tresultOld) -> void $ tryPutTMVar tresultOld ResultCanceled
                        writeTVar tmnext $ Just request
                        -- use the previous worker, so nothing to do
                        return (return ())
                    ASRunning runningApp -> do
                        tmnext <- newTVar Nothing
                        writeTVar tstate $ ASStarting (Just runningApp) tmnext
                        return launchWorker
            Nothing -> do
                case action of
                    Reload _ -> do
                        tmnext <- newTVar Nothing
                        tstate <- newTVar $ ASStarting Nothing tmnext
                        writeTVar apps $ Map.insert aid tstate m
                        return launchWorker
                    Terminate -> do
                        putTMVar tresult ResultSuccess
                        return (return ())
        return (launchWorker', tresult)
    launchWorker'
    void $ forkIO $ do
        result <- atomically $ takeTMVar tresult
        -- FIXME
        return ()
        {-
        case (aid, result) of
            ResultSuccess -> log $ AppLoadedSuccessfully
            -}
  where
    launchWorker = return () -- FIXME

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
