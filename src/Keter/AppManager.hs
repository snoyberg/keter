{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Used for management of applications.
module Keter.AppManager
    ( -- * Types
      AppManager
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

import Conduit (sourceFile, (.|), ConduitT, ResourceT, runResourceT, runConduit, awaitForever, sinkNull)
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (forM_, void, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (runStateT, StateT, modify)
import Data.Binary (Word32)
import Data.Binary.Get (getWord32le, runGet)
import Data.Bits (Bits((.|.), shiftR))
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Zlib (ungzip)
import Data.Conduit (yield)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as Builder
import Data.Traversable.WithIndex (itraverse)
import Keter.App qualified as App
import Keter.Common
import Keter.Config
import Keter.Context
import Keter.SharedData.App (App, AppStartConfig (ascKeterConfig))
import Keter.SharedData.AppManager
import Prelude hiding (FilePath, log)
import System.Directory (getFileSize, getModificationTime)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (FilePath)
import System.IO (hGetContents)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import System.Process (shell, CreateProcess (env, std_err), waitForProcess, StdStream (CreatePipe), withCreateProcess)

data AppManager = AppManager
    { apps           :: !(TVar (Map AppId (TVar AppState)))
    , appStartConfig :: !AppStartConfig
    , mutex          :: !(MVar ())
    , loads          :: !(TVar [FilePath])
    }

renderApps :: AppManager -> STM Text
renderApps mngr = do
  appMap <- readTVar $ apps mngr
  x <- itraverse (\_appId tappState -> do
                state <- readTVar tappState
                res <- showAppState state
                pure $ Builder.fromText $ res <> " \n"
               ) appMap
  pure $ LT.toStrict $ Builder.toLazyText $ fold x

initialize :: KeterM AppStartConfig AppManager
initialize = do
  asc <- ask
  liftIO $ AppManager
      <$> newTVarIO Map.empty
      <*> return asc
      <*> newMVar ()
      <*> newTVarIO []

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
  AppManager{..} <- ask
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
    AppManager{..} <- ask
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
launchWorker appid tstate tmnext mcurrentApp' action' =
  void $ withRunInIO $ \rio -> forkIO $ rio $ loop mcurrentApp' action'
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
        pack $ "Reloading from: " <> app <> ", " <> input

    errorStartingBundleMsg :: String -> String -> Text
    errorStartingBundleMsg bundleName e =
        pack $ "Error occured when launching bundle " <> bundleName <> ": " <> e

    processAction :: Maybe App -> Action -> KeterM AppManager (Maybe App)
    processAction Nothing Terminate = return Nothing
    processAction (Just app) Terminate = do
        $logInfo $ pack ("Terminating " <> show app)
        withMappedConfig (const app) App.terminate
        return Nothing
    processAction Nothing (Reload input) = do
        $logInfo (reloadMsg "Nothing" (show input))
        AppManager{..} <- ask
        eres <- withRunInIO $ \rio -> E.try @SomeException $
            rio $ withMappedConfig (const appStartConfig) $ App.start appid input tstate
        case eres of
            Left e -> do
                $logError (errorStartingBundleMsg (show name) (show e))
                callUserHook $ userHookEnvFailed e Nothing
                return Nothing
            Right app -> do
                callUserHook userHookEnvSuccess
                return $ Just app
    processAction (Just app) (Reload input) = do
        $logInfo (reloadMsg (show $ Just app) (show input))
        eres <- withRunInIO $ \rio -> E.try @SomeException $
            rio $ withMappedConfig (const app) $ App.reload input tstate
        case eres of
            Left e -> do
                $logError (errorStartingBundleMsg (show name) (show e))
                callUserHook $ userHookEnvFailed e $ Just app
                -- reloading will /always/ result in a valid app, either the old one
                -- will continue running or the new one will replace it.
                return (Just app)
            Right () -> do
                callUserHook userHookEnvSuccess
                return $ Just app

    name =
        case appid of
            AIBuiltin -> "<builtin>"
            AINamed x -> x

    callUserHook :: [(String, String)] -> KeterM AppManager ()
    callUserHook envVars =
        asks (kconfigUserHook . ascKeterConfig . appStartConfig) >>= mapM_
            (\hook -> withRunInIO $ \rio -> do
                withCreateProcess (shell $ unpack hook)
                                  { env = Just envVars
                                  , std_err = CreatePipe
                                  }
                                  $ \_ _ mbStderr ph -> do
                    exitCode <- waitForProcess ph
                    when (exitCode /= ExitSuccess) $ do
                        err <- case mbStderr of
                                   Just stderr -> (": "<>) <$> hGetContents stderr
                                   Nothing -> return ""
                        rio $ $logError $ "Failed to start user hook '" <> hook <> "'" <> pack err
            )
    userHookEnvSuccess :: [(String, String)]
    userHookEnvSuccess =
        [ ("NAME", unpack name)
        , ("STATUS", "started")
        ]
    userHookEnvFailed :: SomeException -> Maybe App -> [(String, String)]
    userHookEnvFailed err mbApp =
        [ ("NAME", unpack name)
        , ("STATUS", "failure")
        , ("FAILURE", show err)
        , ("FALLBACK", show $ isJust mbApp)
        ] ++ maybe [] (\app -> [("FALLBACK_APP", show app)]) mbApp

addApp :: FilePath -> KeterM AppManager ()
addApp bundle = do
    AppManager {..} <- ask
    withRunInIO $ \rio -> do
        waitAndPerform' <- liftIO $ atomically $ do
            loads' <- readTVar loads
            if bundle `elem` loads'
            then return skipLoading
            else do
                putBundleIntoQueue loads
                return waitAndPerform
        void $ forkIO $ rio waitAndPerform'
    where
    skipLoading = do
        return ()
    waitAndPerform = do
        waitUntilStable bundle
        runable <- isGzip bundle
        AppManager {..} <- ask
        removeBundleFromQueue loads
        when runable $ do
            (input, action) <- liftIO $ getInputForBundle bundle
            perform input action
    putBundleIntoQueue loads = modifyTVar loads (bundle:)
    removeBundleFromQueue loads = liftIO $ atomically $ modifyTVar loads $ filter (/= bundle)

getInputForBundle :: FilePath -> IO (AppId, Action)
getInputForBundle bundle = do
    time <- modificationTime <$> getFileStatus bundle
    return (AINamed $ getAppname bundle, Reload $ AIBundle bundle time)

terminateApp :: Appname -> KeterM AppManager ()
terminateApp appname = perform (AINamed appname) Terminate

waitUntilStable :: FilePath -> KeterM AppManager ()
waitUntilStable path = liftIO $ loop Nothing
    where
    loop old = do
        size <- getFileSize path
        time <- getModificationTime path
        let new = Just (size, time)
        when (new /= old) $ do
            threadDelay 1000000 -- wait 1 sec
            loop new

isGzip :: FilePath -> KeterM AppManager Bool
isGzip bundle = do
    res <- liftIO $ either (const $ return False) checkSizes
        =<< E.try @SomeException
        ( runResourceT . flip runStateT (0, 0) . runConduit
        $ sourceFile bundle
       .| readSize
       .| ungzip
       .| countSize
       .| sinkNull
        )
    unless res $ $logError $ pack $ "File '" <> bundle <> "' is not gzip"
    return res
    where
    checkSizes (_, (size, counter)) = return $ size >0 && size == counter

readSize :: ConduitT C.ByteString C.ByteString (StateT (Word32, Word32) (ResourceT IO)) ()
readSize = awaitForever $ \chunk -> do
    let footer = BL.takeEnd 4 $ BL.fromStrict chunk
        footerLength = BL.length footer
        nullPrefix = BL.take (4 - footerLength) (BL.repeat 0)
        size = runGet getWord32le $ nullPrefix <> footer
    modify $ \(prevSize, counter) ->
        let newSize = shiftR prevSize (8 * fromIntegral footerLength) .|. size
        in (newSize, counter)
    yield chunk

countSize :: ConduitT C.ByteString C.ByteString (StateT (Word32, Word32) (ResourceT IO)) ()
countSize = awaitForever $ \chunk -> do
    modify $ \(size, counter) -> (size, counter + fromIntegral (C.length chunk))
    yield chunk
