{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
      -- * Show
    , renderApps
    ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as Builder
import Data.Traversable.WithIndex (itraverse)
import Keter.App (App, AppStartConfig, showApp)
import Keter.App qualified as App
import Keter.Common
import Keter.Config
import Keter.Context
import Prelude hiding (FilePath, log)
import System.FilePath (FilePath)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import Text.Printf (printf)

data AppManager = AppManager
    { apps           :: !(TVar (Map AppId (TVar AppState)))
    , appStartConfig :: !AppStartConfig
    , mutex          :: !(MVar ())
    }

data AppState = ASRunning App
              | ASStarting
                    !(Maybe App)
                    !(TVar (Maybe EpochTime))
                    !(TVar (Maybe Action)) -- ^ the next one to try
              | ASTerminated

showAppState :: AppState -> STM Text
showAppState (ASRunning x) = (\x -> "running(" <> x <> ")") <$> showApp x
showAppState (ASStarting mapp tmtime tmaction) = do
  mtime   <- readTVar tmtime
  maction <- readTVar tmaction
  mtext <- traverse showApp mapp
  pure $ pack $ printf "starting app %s, time %s, action %s \n" (unpack $ fold mtext) (show mtime) (show maction)
showAppState ASTerminated = pure "terminated"

renderApps :: AppManager -> STM Text
renderApps mngr = do
  appMap <- readTVar $ apps mngr
  x <- itraverse (\appId tappState -> do
                state <- readTVar tappState
                res <- showAppState state
                pure $ Builder.fromText $ res <> " \n"
               ) appMap
  pure $ LT.toStrict $ Builder.toLazyText $ fold x

data Action = Reload AppInput | Terminate
 deriving Show

initialize :: KeterM AppStartConfig AppManager
initialize = do
  asc <- ask
  liftIO $ AppManager
      <$> newTVarIO Map.empty
      <*> return asc
      <*> newMVar ()

-- | Reset which apps are running.
--
-- * Any app not listed here that is currently running will be terminated.
--
-- * Any app listed here that is currently running will be reloaded.
--
-- * Any app listed here that is not currently running will be started.
reloadAppList :: Map Appname (FilePath, EpochTime)
              -> KeterM AppManager ()
reloadAppList newApps = do
  am@AppManager{..} <- ask
  withRunInIO $ \rio ->
    withMVar mutex $ const $ do
      actions <- atomically $ do
          m <- readTVar apps
          let currentApps = Set.fromList $ mapMaybe toAppName $ Map.keys m
              allApps = Set.toList $ Map.keysSet newApps `Set.union` currentApps
          catMaybes <$> mapM (getAction m) allApps
      mapM_ rio actions
  where
    toAppName AIBuiltin   = Nothing
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
                Nothing              -> E.assert False Nothing
                Just (fp, timestamp) -> reload fp timestamp
        terminate = Just $ performNoLock (AINamed appname) Terminate
        reload fp timestamp = Just $ performNoLock (AINamed appname) (Reload $ AIBundle fp timestamp)
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

perform :: AppId -> Action -> KeterM AppManager ()
perform appid action = do
    am <- ask
    withRunInIO $ \rio ->
      withMVar (mutex am) $ const $ rio $  performNoLock appid action

performNoLock :: AppId -> Action -> KeterM AppManager ()
performNoLock aid action = do
    am@AppManager{..} <- ask
    withRunInIO $ \rio -> E.mask_ $ do
        launchWorker' <- liftIO $ atomically $ do
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
                            return $ launchWorker aid tstate tmnext (Just runningApp) action
                        ASTerminated -> onNotRunning apps
                Nothing -> onNotRunning apps
        rio launchWorker'
  where
    noWorker = return ()

    onNotRunning apps =
        case action of
            Reload input -> do
                tmnext <- newTVar Nothing
                tmtimestamp <- newTVar $
                    case input of
                        AIBundle _fp timestamp -> Just timestamp
                        AIData _               -> Nothing
                tstate <- newTVar $ ASStarting Nothing tmtimestamp tmnext
                modifyTVar apps $ Map.insert aid tstate
                return $ launchWorker aid tstate tmnext Nothing action
            Terminate -> return noWorker

launchWorker ::
     AppId
  -> TVar AppState
  -> TVar (Maybe Action)
  -> Maybe App
  -> Action
  -> KeterM AppManager ()
launchWorker appid tstate tmnext = loop
  where
    loop :: Maybe App -> Action -> KeterM AppManager ()
    loop mcurrentApp action = do
        mRunningApp <- processAction mcurrentApp action
        mnext <- liftIO $ atomically $ do
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
                            Reload (AIData _)               -> Nothing
                            Terminate                       -> Nothing
                    writeTVar tstate $ ASStarting mRunningApp tmtimestamp tmnext
            return mnext
        forM_ mnext (loop mRunningApp)

    reloadMsg :: String -> String -> Text
    reloadMsg app input =
        pack $ "Reloading from: " <> app <> input

    errorStartingBundleMsg :: String -> String -> Text
    errorStartingBundleMsg name e =
        pack $ "Error occured when launching bundle " <> name <> ": " <> e

    processAction :: Maybe App -> Action -> KeterM AppManager (Maybe App)
    processAction Nothing Terminate = return Nothing
    processAction (Just app) Terminate = do
        $logInfo $ pack ("Terminating" <> show app)
        withMappedConfig (const app) App.terminate
        return Nothing
    processAction Nothing (Reload input) = do
        $logInfo (reloadMsg "Nothing" (show input))
        AppManager{..} <- ask
        eres <- withRunInIO $ \rio -> E.try @SomeException $
            rio $ withMappedConfig (const appStartConfig) $ App.start appid input
        case eres of
            Left e -> do
                $logError (errorStartingBundleMsg (show name) (show e))
                return Nothing
            Right app -> return $ Just app
    processAction (Just app) (Reload input) = do
        $logInfo (reloadMsg (show $ Just app) (show input))
        eres <- withRunInIO $ \rio -> E.try @SomeException $
            rio $ withMappedConfig (const app) $ App.reload input
        case eres of
            Left e -> do
                $logError (errorStartingBundleMsg (show name) (show e))
                -- reloading will /always/ result in a valid app, either the old one
                -- will continue running or the new one will replace it.
                return (Just app)
            Right () -> return $ Just app

    name =
        case appid of
            AIBuiltin -> "<builtin>"
            AINamed x -> x

addApp :: FilePath -> KeterM AppManager ()
addApp bundle = do
    (input, action) <- liftIO $ getInputForBundle bundle
    perform input action

getInputForBundle :: FilePath -> IO (AppId, Action)
getInputForBundle bundle = do
    time <- modificationTime <$> getFileStatus bundle
    return (AINamed $ getAppname bundle, Reload $ AIBundle bundle time)

terminateApp :: Appname -> KeterM AppManager ()
terminateApp appname = perform (AINamed appname) Terminate
