{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Used for management of applications.
module Keter.AppManager
    ( -- * Types
      AppManager
    , Action (..)
      -- * Actions
    , perform
    , reloadAppList
    , addApp
    , terminateApp
      -- * Initialize
    , initialize
    ) where

import           Control.Applicative
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar, newMVar, withMVar)
import           Control.Concurrent.STM
import qualified Control.Exception         as E
import           Control.Monad             (void)
import qualified Data.Map                  as Map
import           Data.Maybe                (mapMaybe)
import           Data.Maybe                (catMaybes)
import qualified Data.Set                  as Set
import           Keter.App                 (App, AppStartConfig)
import qualified Keter.App                 as App
import           Keter.Types
import           Prelude                   hiding (FilePath, log)
import           System.Posix.Files        (getFileStatus, modificationTime)
import           System.Posix.Types        (EpochTime)

data AppManager = AppManager
    { apps           :: !(TVar (Map AppId (TVar AppState)))
    , appStartConfig :: !AppStartConfig
    , mutex          :: !(MVar ())
    , log            :: !(LogMessage -> IO ())
    }

data AppState = ASRunning App
              | ASStarting
                    !(Maybe App)
                    !(TVar (Maybe EpochTime))
                    !(TVar (Maybe Action)) -- ^ the next one to try
              | ASTerminated

data Action = Reload AppInput | Terminate

initialize :: (LogMessage -> IO ())
           -> AppStartConfig
           -> IO AppManager
initialize log' asc = AppManager
    <$> newTVarIO Map.empty
    <*> return asc
    <*> newMVar ()
    <*> return log'

-- | Reset which apps are running.
--
-- * Any app not listed here that is currently running will be terminated.
--
-- * Any app listed here that is currently running will be reloaded.
--
-- * Any app listed here that is not currently running will be started.
reloadAppList :: AppManager
              -> Map Appname (FilePath, EpochTime)
              -> IO ()
reloadAppList am@AppManager {..} newApps = withMVar mutex $ const $ do
    actions <- atomically $ do
        m <- readTVar apps
        let currentApps = Set.fromList $ mapMaybe toAppName $ Map.keys m
            allApps = Set.toList $ Map.keysSet newApps `Set.union` currentApps
        fmap catMaybes $ mapM (getAction m) allApps
    sequence_ actions
  where
    toAppName AIBuiltin = Nothing
    toAppName (AINamed x) = Just x

    getAction currentApps appname = do
        case Map.lookup (AINamed appname) currentApps of
            Nothing -> return freshLaunch
            Just tstate -> do
                state <- readTVar tstate
                case state of
                    ASTerminated -> return freshLaunch
                    ASRunning app ->
                        case Map.lookup appname newApps of
                            Nothing -> return terminate
                            Just (fp, newTimestamp) -> do
                                moldTimestamp <- App.getTimestamp app
                                return $ if moldTimestamp == Just newTimestamp
                                    then Nothing
                                    else reload fp newTimestamp
                    ASStarting _ tmoldTimestamp tmaction ->
                        case Map.lookup appname newApps of
                            Nothing -> do
                                writeTVar tmaction $ Just Terminate
                                return Nothing
                            Just (fp, newTimestamp) -> do
                                moldTimestamp <- readTVar tmoldTimestamp
                                return $ if moldTimestamp == Just newTimestamp
                                    then Nothing
                                    else reload fp newTimestamp
      where
        freshLaunch =
            case Map.lookup appname newApps of
                Nothing -> E.assert False Nothing
                Just (fp, timestamp) -> reload fp timestamp
        terminate = Just $ performNoLock am (AINamed appname) Terminate
        reload fp timestamp = Just $ performNoLock am (AINamed appname) (Reload $ AIBundle fp timestamp)
        {-
        case (Map.lookup appname currentApps, Map.lookup appname newApps) of
            (Nothing, Nothing) -> E.assert False Nothing
            (Just _, Nothing) -> Just $ perform am (AINamed appname) Terminate
            (Nothing, Just _) -> Just $ perform am (AINamed appname) (Reload AIBundle)
            -}

    {- FIXME
        actions <- do

            current <- getAllApps appMan
            let apps = Set.toList $ Set.fromList (Map.keys newMap) `Set.union` current
            fmap catMaybes $ forM apps $ \appname -> return $
                case (Set.member appname current, Map.lookup appname newMap) of
                    (False, Nothing) -> Nothing -- should never happen
                    (True, Nothing) -> Just $ terminateApp appname
                    (False, Just (bundle, _)) -> Just $ runKIO' $ addApp bundle
                    (Just (_, oldTime), Just (bundle, newTime))
                        | newTime /= oldTime -> Just $ runKIO' $ addApp bundle
                        | otherwise -> Nothing
        P.sequence_ actions

getAllApps :: AppManager -> IO (Set Appname)
getAllApps AppManager {..} = atomically $ do
    m <- readTVar apps
    return $ Set.fromList $ mapMaybe toAppName $ Map.keys m
    -}

perform :: AppManager -> AppId -> Action -> IO ()
perform am appid action = withMVar (mutex am) $ const $ performNoLock am appid action

performNoLock :: AppManager -> AppId -> Action -> IO ()
performNoLock am@AppManager {..} aid action = E.mask_ $ do
    launchWorker' <- atomically $ do
        m <- readTVar apps
        case Map.lookup aid m of
            Just tstate -> do
                state <- readTVar tstate
                case state of
                    ASStarting _mcurrent _tmtimestamp tmnext -> do
                        writeTVar tmnext $ Just action
                        -- use the previous worker, so nothing to do
                        return noWorker
                    ASRunning runningApp -> do
                        tmnext <- newTVar Nothing
                        tmtimestamp <- newTVar $
                            case action of
                                Reload (AIBundle _fp timestamp) -> Just timestamp
                                Reload (AIData _) -> Nothing
                                Terminate -> Nothing
                        writeTVar tstate $ ASStarting (Just runningApp) tmtimestamp tmnext
                        return $ launchWorker am aid tstate tmnext (Just runningApp) action
                    ASTerminated -> onNotRunning
            Nothing -> onNotRunning
    launchWorker'
  where
    noWorker = return ()

    onNotRunning =
        case action of
            Reload input -> do
                tmnext <- newTVar Nothing
                tmtimestamp <- newTVar $
                    case input of
                        AIBundle _fp timestamp -> Just timestamp
                        AIData _ -> Nothing
                tstate <- newTVar $ ASStarting Nothing tmtimestamp tmnext
                modifyTVar apps $ Map.insert aid tstate
                return $ launchWorker am aid tstate tmnext Nothing action
            Terminate -> return noWorker

launchWorker :: AppManager
             -> AppId
             -> TVar AppState
             -> TVar (Maybe Action)
             -> Maybe App
             -> Action
             -> IO ()
launchWorker AppManager {..} appid tstate tmnext mcurrentApp0 action0 = void $ forkIO $ do
    loop mcurrentApp0 action0
  where
    loop mcurrentApp action = do
        mRunningApp <- processAction mcurrentApp action
        mnext <- atomically $ do
            mnext <- readTVar tmnext
            writeTVar tmnext Nothing
            case mnext of
                Nothing ->
                    case mRunningApp of
                        Nothing -> writeTVar tstate ASTerminated
                        Just runningApp -> writeTVar tstate $ ASRunning runningApp
                Just _next -> do
                    tmtimestamp <- newTVar $
                        case action of
                            Reload (AIBundle _fp timestamp) -> Just timestamp
                            Reload (AIData _) -> Nothing
                            Terminate -> Nothing
                    writeTVar tstate $ ASStarting mRunningApp tmtimestamp tmnext
            return mnext
        case mnext of
            Nothing -> return ()
            Just next -> loop mRunningApp next

    processAction Nothing Terminate = return Nothing
    processAction (Just app) Terminate = do
        log $ Terminating $ show app
        App.terminate app
        return Nothing
    processAction Nothing (Reload input) = do
        log $ ReloadFrom Nothing $ show input
        eres <- E.try $ App.start appStartConfig appid input
        case eres of
            Left e -> do
                log $ ErrorStartingBundle name e
                return Nothing
            Right app -> return $ Just app
    processAction (Just app) (Reload input) = do
        log $ ReloadFrom (Just $ show app) (show input)
        eres <- E.try $ App.reload app input
        case eres of
            Left e -> do
                log $ ErrorStartingBundle name e
                -- reloading will /always/ result in a valid app, either the old one
                -- will continue running or the new one will replace it.
                return (Just app)
            Right () -> return $ Just app

    name =
        case appid of
            AIBuiltin -> "<builtin>"
            AINamed x -> x

addApp :: AppManager -> FilePath -> IO ()
addApp appMan bundle = do
    (input, action) <- getInputForBundle bundle
    perform appMan input action

getInputForBundle :: FilePath -> IO (AppId, Action)
getInputForBundle bundle = do
    time <- modificationTime <$> getFileStatus bundle
    return (AINamed $ getAppname bundle, Reload $ AIBundle bundle time)

terminateApp :: AppManager -> Appname -> IO ()
terminateApp appMan appname = perform appMan (AINamed appname) Terminate
