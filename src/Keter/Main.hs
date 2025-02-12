{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Keter.Main
    ( keter
    ) where

import Control.Concurrent.Async (waitAny, withAsync)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (LoggingT, logInfo, runLoggingT)
import Control.Monad.Logger qualified as L
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Read qualified
import Data.Time (getCurrentTime)
import Data.Vector qualified as V
import Keter.App (AppStartConfig(..))
import Keter.AppManager qualified as AppMan
import Keter.Cli
import Keter.Common
import Keter.Conduit.Process.Unix (initProcessTracker)
import Keter.Config
import Keter.Config.V10
import Keter.Context
import Keter.HostManager qualified as HostMan
import Keter.Logger qualified as Log
import Keter.PortPool qualified as PortPool
import Keter.Proxy qualified as Proxy
import Keter.TempTarball qualified as TempFolder
import Keter.Yaml.FilePath
import Prelude hiding (FilePath, log)
import System.Directory
       ( createDirectoryIfMissing
       , doesDirectoryExist
       , doesFileExist
       , getDirectoryContents
       )
import System.FilePath (FilePath, takeExtension, (</>))
import System.FSNotify qualified as FSN
import System.Log.FastLogger qualified as FL
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Signals (Handler(Catch), installHandler, sigHUP)
import System.Posix.User
       (getUserEntryForID, getUserEntryForName, userGroupID, userID, userName)

#ifdef SYSTEM_FILEPATH
import Filesystem.Path qualified as FP (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
#endif

keter :: FilePath -- ^ root directory or config file
      -> [FilePath -> IO Plugin]
      -> IO ()
keter input mkPlugins =
    runKeterConfigReader input . runKeterLogger . runKeterM $
        withManagers mkPlugins $ \hostman appMan -> do
            KeterConfig{..} <- ask
            $logInfo "Launching cli"
            forM_ kconfigCliPort $ \port ->
              withMappedConfig
                  (const $ MkCliStates
                      { csAppManager = appMan
                      , csPort       = port
                      })
                  launchCli
            $logInfo "Launching initial"
            launchInitial appMan
            $logInfo "Started watching"
            startWatching appMan
            $logInfo "Started listening"
            startListening hostman

-- | Load up Keter config and evaluate a ReaderT context with it
runKeterConfigReader :: MonadIO m
                     => FilePath
                     -> ReaderT KeterConfig m a
                     -> m a
runKeterConfigReader input ctx = do
    exists <- liftIO $ doesFileExist input
    config <- liftIO $
        if exists
            then do
                eres <- decodeFileRelative input
                case eres of
                    Left e -> throwIO $ InvalidKeterConfigFile input e
                    Right x -> return x
            else return defaultKeterConfig { kconfigDir = input }
    runReaderT ctx config

-- | Running the Keter logger requires a context with access to a KeterConfig, hence the
-- MonadReader constraint. This is versatile: 'runKeterConfigReader', or use the free
-- ((->) KeterConfig) instance.
runKeterLogger :: (MonadReader KeterConfig m, MonadIO m, MonadUnliftIO m)
               => LoggingT m a
               -> m a
runKeterLogger ctx = do
    cfg <- ask
    withRunInIO $ \rio -> bracket (Log.createLoggerViaConfig cfg "keter") Log.loggerClose $
        rio . runLoggingT ctx . formatLog
    where
        formatLog logger loc _ lvl msg = do
            now <- liftIO getCurrentTime
            -- Format: "{keter|}$time|$module$:$line_num|$log_level> $msg"
            let tag = case Log.loggerType logger of { FL.LogStderr _ -> "keter|"; _ -> mempty }
            let bs = mconcat
                    [ tag
                    , L.toLogStr $ take 22 $ show now
                    , "|"
                    , L.toLogStr (L.loc_module loc)
                    , ":"
                    , L.toLogStr (fst $ L.loc_start loc)
                    , "|"
                    , L.toLogStr $ drop 5 $ show lvl
                    , "> "
                    , msg
                    , "\n"
                    ]
            Log.loggerLog logger bs

withManagers :: [FilePath -> IO Plugin]
             -> (HostMan.HostManager -> AppMan.AppManager -> KeterM KeterConfig a)
             -> KeterM KeterConfig a
withManagers mkPlugins f = do
    cfg@KeterConfig{..} <- ask
    processTracker <- liftIO initProcessTracker
    hostman <- liftIO HostMan.start
    portpool <- liftIO $ PortPool.start kconfigPortPool
    tf <- liftIO $ TempFolder.setup $ kconfigDir </> "temp"
    plugins <- mapM (liftIO . ($ kconfigDir)) mkPlugins
    muid <-
        case kconfigSetuid of
            Nothing -> return Nothing
            Just t -> do
                x <- liftIO $ try $
                    case Data.Text.Read.decimal t of
                        Right (i, "") -> getUserEntryForID i
                        _ -> getUserEntryForName $ T.unpack t
                case x of
                    Left (_ :: SomeException) -> error $ "Invalid user ID: " ++ T.unpack t
                    Right ue -> return $ Just (T.pack $ userName ue, (userID ue, userGroupID ue))

    let appStartConfig = AppStartConfig
            { ascTempFolder = tf
            , ascSetuid = muid
            , ascProcessTracker = processTracker
            , ascHostManager = hostman
            , ascPortPool = portpool
            , ascPlugins = plugins
            , ascKeterConfig = cfg
            }
    appMan <- withMappedConfig (const appStartConfig) AppMan.initialize
    f hostman appMan

launchInitial :: AppMan.AppManager -> KeterM KeterConfig ()
launchInitial appMan = do
    kc@KeterConfig{..} <- ask
    let incoming = getIncoming kc
    liftIO $ createDirectoryIfMissing True incoming
    bundles0 <- liftIO $ filter isKeter <$> listDirectoryTree incoming
    withMappedConfig (const appMan) $ do
        mapM_ AppMan.addApp bundles0
        unless (V.null kconfigBuiltinStanzas) $ AppMan.perform
            AIBuiltin
            (AppMan.Reload $ AIData $ BundleConfig kconfigBuiltinStanzas mempty)

getIncoming :: KeterConfig -> FilePath
getIncoming kc = kconfigDir kc </> "incoming"

isKeter :: FilePath -> Bool
isKeter fp = takeExtension fp == ".keter"

startWatching :: AppMan.AppManager -> KeterM KeterConfig ()
startWatching appMan = do
    incoming <- asks getIncoming
    -- File system watching
    wm <- liftIO FSN.startManager
    withMappedConfig (const appMan) $ withRunInIO $ \rio -> do
        _ <- FSN.watchTree wm (fromString incoming) (const True) $ \e -> do
                e' <-
                    case e of
                        FSN.Removed fp _ _ -> do
                            rio $ $logInfo $ "Watched file removed: " <> T.pack (fromFilePath fp)
                            return $ Left $ fromFilePath fp
                        FSN.Added fp _ _ -> do
                            rio $ $logInfo $ "Watched file added: " <> T.pack (fromFilePath fp)
                            return $ Right $ fromFilePath fp
                        FSN.Modified fp _ _ -> do
                            rio $ $logInfo $ "Watched file modified: " <> T.pack (fromFilePath fp)
                            return $ Right $ fromFilePath fp
                        _ -> do
                            rio $ $logInfo $ "Watched file unknown" <> T.pack mempty
                            return $ Left []
                rio $ case e' of
                    Left fp -> when (isKeter fp) $ AppMan.terminateApp $ getAppname fp
                    Right fp -> when (isKeter fp) $ AppMan.addApp $ incoming </> fp
        -- Install HUP handler for cases when inotify cannot be used.
        void $ flip (installHandler sigHUP) Nothing $ Catch $ do
            bundles <- filter isKeter <$> listDirectoryTree incoming
            newMap <- fmap Map.fromList $ forM bundles $ \bundle -> do
                time <- modificationTime <$> getFileStatus bundle
                return (getAppname bundle, (bundle, time))
            rio $ AppMan.reloadAppList newMap


-- compatibility with older versions of fsnotify which used
-- 'Filesystem.Path'
#ifdef SYSTEM_FILEPATH
fromFilePath :: FP.FilePath -> String
fromFilePath = encodeString
#else
fromFilePath :: forall a. a -> a
fromFilePath = id
#endif

listDirectoryTree :: FilePath -> IO [FilePath]
listDirectoryTree fp = do
       dir <- getDirectoryContents fp
       concat <$> mapM (\fpRel -> do
          let fp1 = fp </> fpRel
          isDir <- doesDirectoryExist fp1
          if isDir
           then
             listDirectoryTree fp1
           else
             return [fp1]
           ) (filter (\x -> x /= "." && x /= "..") dir)

startListening :: HostMan.HostManager -> KeterM KeterConfig ()
startListening hostman = do
    KeterConfig{..} <- ask
    settings <- Proxy.makeSettings hostman
    withMappedConfig (const settings) $ withRunInIO $ \rio ->
        liftIO $ runAndBlock kconfigListeners $ \ls ->
            rio $ Proxy.reverseProxy ls

runAndBlock :: NonEmptyVector a
            -> (a -> IO ())
            -> IO ()
runAndBlock (NonEmptyVector x0 v) f =
    loop l0 []
  where
    l0 = x0 : V.toList v

    loop (x:xs) asyncs = withAsync (f x) $ \async -> loop xs $ async : asyncs
    -- Once we have all of our asyncs, we wait for /any/ of them to exit. If
    -- any listener thread exits, we kill the whole process.
    loop [] asyncs = void $ waitAny asyncs
