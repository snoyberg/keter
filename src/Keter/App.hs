{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Keter.App
    ( start
    , reload
    , getTimestamp
    , Keter.App.terminate
    ) where

import Control.Arrow ((***))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception
       (IOException, SomeException, bracketOnError, catch, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask)
import Data.CaseInsensitive qualified as CI
import Data.Foldable (for_)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import Data.Yaml
import Keter.Common
import Keter.Conduit.Process.Unix
       ( monitorProcess
       , printStatus
       , terminateMonitoredProcess
       )
import Keter.Config
import Keter.Context
import Keter.HostManager hiding (start)
import Keter.Logger (Logger)
import Keter.Logger qualified as Log
import Keter.PortPool (getPort, releasePort)
import Keter.Rewrite (ReverseProxyConfig(..))
import Keter.SharedData.App
import Keter.SharedData.AppManager (AppState(..))
import Keter.TempTarball
import Keter.Yaml.FilePath
import Network.Socket
import Network.TLS qualified as TLS
import Prelude hiding (FilePath)
import System.Directory
       (canonicalizePath, doesFileExist, removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.FilePath (FilePath, (</>))
import System.IO (IOMode(..), hClose)
import System.Log.FastLogger qualified as FL
import System.Posix.Files (fileAccess)
import System.Posix.Types (EpochTime)
import System.Timeout (timeout)

unpackBundle :: FilePath
             -> AppId
             -> KeterM AppStartConfig (FilePath, BundleConfig)
unpackBundle bundle aid = do
    AppStartConfig{..} <- ask
    $logInfo $ pack $ "Unpacking bundle '" <> show bundle <> "'"
    liftIO $ unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
        -- Get the FilePath for the keter yaml configuration. Tests for
        -- keter.yml and defaults to keter.yaml.
        configFP <- do
            let yml = dir </> "config" </> "keter.yml"
            exists <- doesFileExist yml
            return $ if exists then yml
                               else dir </> "config" </> "keter.yaml"

        mconfig <- decodeFileRelative configFP
        config <-
            case mconfig of
                Right config -> return config
                Left e -> throwIO $ InvalidConfigFile e
        return (dir, config)
  where
    folderName =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> x

withConfig :: AppId
           -> AppInput
           -> (Maybe FilePath -> BundleConfig -> Maybe EpochTime -> KeterM AppStartConfig a)
           -> KeterM AppStartConfig a
withConfig _aid (AIData bconfig) f = f Nothing bconfig Nothing
withConfig aid (AIBundle fp modtime) f = do
    withRunInIO $ \rio ->
        bracketOnError (rio $ unpackBundle fp aid) (\(newdir, _) -> removeDirectoryRecursive newdir) $ \(newdir, bconfig) ->
            rio $ f (Just newdir) bconfig (Just modtime)

withReservations :: AppId
                 -> BundleConfig
                 -> ([WebAppConfig Port] -> [BackgroundConfig] -> Map Host (ProxyAction, TLS.Credentials) -> KeterM AppStartConfig a)
                 -> KeterM AppStartConfig a
withReservations aid bconfig f = do
    AppStartConfig{..} <- ask
    withActions bconfig $ \wacs backs actions ->
        withRunInIO $ \rio ->
            bracketOnError
              (rio $ withMappedConfig (const ascHostManager) $ reserveHosts aid $ Map.keysSet actions)
              (rio . withMappedConfig (const ascHostManager) . forgetReservations aid)
              (\_ -> rio $ f wacs backs actions)

withActions :: BundleConfig
            -> ([ WebAppConfig Port] -> [BackgroundConfig] -> Map Host (ProxyAction, TLS.Credentials) -> KeterM AppStartConfig a)
            -> KeterM AppStartConfig a
withActions bconfig f =
    loop (V.toList $ bconfigStanzas bconfig) [] [] Map.empty
  where
    -- todo: add loading from relative location
    loadCert (SSL certFile chainCertFiles keyFile) =
         either (const mempty) (TLS.Credentials . (:[]))
            <$> TLS.credentialLoadX509Chain certFile (V.toList chainCertFiles) keyFile
    loadCert _ = return mempty

    loop [] wacs backs actions = f wacs backs actions
    loop (Stanza (StanzaWebApp wac) rs:stanzas) wacs backs actions = do
      AppStartConfig{..} <- ask
      withRunInIO $ \rio ->
        liftIO $ bracketOnError
          (rio (getPort ascPortPool) >>= either throwIO
               (\p -> fmap (p,) <$> loadCert $ waconfigSsl wac)
          )
          (\(port, _)    -> releasePort ascPortPool port)
          (\(port, cert) -> rio $ loop
              stanzas
              (wac { waconfigPort = port } : wacs)
              backs
              (Map.unions $ actions : map (\host -> Map.singleton host ((PAPort port (waconfigTimeout wac), rs), cert)) hosts))
      where
        hosts = Set.toList $ Set.insert (waconfigApprootHost wac) (waconfigHosts wac)
    loop (Stanza (StanzaStaticFiles sfc) rs:stanzas) wacs backs actions0 = do
        cert <- liftIO $ loadCert $ sfconfigSsl sfc
        loop stanzas wacs backs (actions cert)
      where
        actions cert = Map.unions
                $ actions0
                : map (\host -> Map.singleton host ((PAStatic sfc, rs), cert))
                  (Set.toList (sfconfigHosts sfc))
    loop (Stanza (StanzaRedirect red) rs:stanzas) wacs backs actions0 = do
        cert <- liftIO $ loadCert $ redirconfigSsl red
        loop stanzas wacs backs (actions cert)
      where
        actions cert = Map.unions
                $ actions0
                : map (\host -> Map.singleton host ((PARedirect red, rs), cert))
                  (Set.toList (redirconfigHosts red))
    loop (Stanza (StanzaReverseProxy rev mid to) rs:stanzas) wacs backs actions0 = do
        cert <- liftIO $ loadCert $ reversingUseSSL rev
        loop stanzas wacs backs (actions cert)
      where
        actions cert = Map.insert (CI.mk $ reversingHost rev) ((PAReverseProxy rev mid to, rs), cert) actions0
    loop (Stanza (StanzaBackground back) _:stanzas) wacs backs actions =
        loop stanzas wacs (back:backs) actions

-- | Gives the log file or log tag name for a given 'AppId'
appLogName :: AppId -> String
appLogName AIBuiltin = "__builtin__"
appLogName (AINamed x) = "app-" <> unpack x

withLogger :: AppId
           -> Maybe (TVar (Maybe Logger))
           -> (TVar (Maybe Logger) -> Logger -> KeterM AppStartConfig a)
           -> KeterM AppStartConfig a
withLogger aid Nothing f = do
    var <- liftIO $ newTVarIO Nothing
    withLogger aid (Just var) f
withLogger aid (Just var) f = do
    AppStartConfig{..} <- ask
    mappLogger <- liftIO $ readTVarIO var
    case mappLogger of
        Nothing -> withRunInIO $ \rio ->
          bracketOnError (Log.createLoggerViaConfig ascKeterConfig (appLogName aid))
                         Log.loggerClose
                       $ \appLogger -> do
                           atomically $ writeTVar var $ Just appLogger
                           rio $ f var appLogger
        Just appLogger ->  f var appLogger

withSanityChecks :: BundleConfig -> KeterM AppStartConfig a -> KeterM AppStartConfig a
withSanityChecks BundleConfig{..} f = do
    liftIO $ V.mapM_ go bconfigStanzas
    $logInfo "Sanity checks passed"
    f
  where
    go (Stanza (StanzaWebApp WebAppConfig {..}) _) = do
      isExec waconfigExec
      for_ waconfigEnsureAliveTimeout
        $ \x -> when (x < 1) $ throwIO $ EnsureAliveShouldBeBiggerThenZero x
    go (Stanza (StanzaBackground BackgroundConfig {..}) _) = isExec bgconfigExec
    go _ = return ()

    isExec fp = do
        exists <- doesFileExist fp
        if exists
            then do
                canExec <- fileAccess fp True False True
                if canExec
                    then return ()
                    else throwIO $ FileNotExecutable fp
            else throwIO $ ExecutableNotFound fp

start :: AppId
      -> AppInput
      -> TVar AppState
      -> KeterM AppStartConfig App
start aid input tstate =
    withLogger aid Nothing $ \tAppLogger appLogger ->
    withConfig aid input $ \newdir bconfig mmodtime ->
    withSanityChecks bconfig $
    withReservations aid bconfig $ \webapps backs actions ->
    withBackgroundApps aid bconfig newdir appLogger backs $ \runningBacks ->
    withWebApps aid bconfig newdir appLogger webapps $ \runningWebapps -> do
        asc@AppStartConfig{..} <- ask
        liftIO $ mapM_ (ensureAlive tstate) runningWebapps
        withMappedConfig (const ascHostManager) $ activateApp aid actions
        liftIO $
          App
            <$> newTVarIO mmodtime
            <*> newTVarIO runningWebapps
            <*> newTVarIO runningBacks
            <*> return aid
            <*> newTVarIO (Map.keysSet actions)
            <*> newTVarIO newdir
            <*> return asc
            <*> return tAppLogger

bracketedMap :: (a -> (b -> IO c) -> IO c)
             -> ([b] -> IO c)
             -> [a]
             -> IO c
bracketedMap with inside =
    loop id
  where
    loop front [] = inside $ front []
    loop front (c:cs) = with c $ \x -> loop (front . (x:)) cs

withWebApps :: AppId
            -> BundleConfig
            -> Maybe FilePath
            -> Logger
            -> [WebAppConfig Port]
            -> ([RunningWebApp] -> KeterM AppStartConfig a)
            -> KeterM AppStartConfig a
withWebApps aid bconfig mdir appLogger configs0 f =
    withRunInIO $ \rio ->
      bracketedMap (\wac f' -> rio $ alloc wac (liftIO <$> f')) (rio . f) configs0
  where
    alloc = launchWebApp aid bconfig mdir appLogger

-- | Format a log message for an app by tagging it with 'app-$name>' (only when it is being logged to stderr)
formatAppLog :: AppId -> FL.LogType -> LogStr -> LogStr
formatAppLog aid (FL.LogStderr _) msg = toLogStr (appLogName aid) <> "> " <> msg
formatAppLog _ _ msg = msg

launchWebApp :: AppId
             -> BundleConfig
             -> Maybe FilePath
             -> Logger
             -> WebAppConfig Port
             -> (RunningWebApp -> KeterM AppStartConfig a)
             -> KeterM AppStartConfig a
launchWebApp aid BundleConfig {..} mdir appLogger WebAppConfig {..} f = do
    AppStartConfig{..} <- ask
    otherEnv <- liftIO $ pluginsGetEnv ascPlugins name bconfigPlugins
    forwardedEnv <- liftIO $ getForwardedEnv waconfigForwardEnv
    let httpPort  = kconfigExternalHttpPort  ascKeterConfig
        httpsPort = kconfigExternalHttpsPort ascKeterConfig
        (scheme, extport) =
            if waconfigSsl == SSLFalse
                then ("http://",  if httpPort  ==  80 then "" else ':' : show httpPort)
                else ("https://", if httpsPort == 443 then "" else ':' : show httpsPort)
        env = Map.toList $ Map.unions
            -- Ordering chosen specifically to precedence rules: app specific,
            -- plugins, global, and then auto-set Keter variables.
            [ waconfigEnvironment
            , forwardedEnv
            , Map.fromList otherEnv
            , kconfigEnvironment ascKeterConfig
            , Map.singleton "PORT" $ pack $ show waconfigPort
            , Map.singleton "APPROOT" $ scheme <> CI.original waconfigApprootHost <> pack extport
            ]
    exec <- liftIO $ canonicalizePath waconfigExec
    withRunInIO $ \rio -> bracketOnError
        (rio $ monitorProcess
            ascProcessTracker
            (encodeUtf8 . fst <$> ascSetuid)
            (encodeUtf8 $ pack exec)
            (maybe "/tmp" (encodeUtf8 . pack) mdir)
            (map encodeUtf8 $ V.toList waconfigArgs)
            (map (encodeUtf8 *** encodeUtf8) env)
            (Log.loggerLog appLogger . formatAppLog aid (Log.loggerType appLogger) . toLogStr)
            (const $ return True))
        terminateMonitoredProcess
        $ \mp -> rio $ f RunningWebApp
            { rwaProcess = mp
            , rwaPort = waconfigPort
            , rwaEnsureAliveTimeOut = fromMaybe (90 * 1000 * 1000) waconfigEnsureAliveTimeout
            }
  where
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> x

killWebApp :: RunningWebApp -> KeterM cfg ()
killWebApp RunningWebApp {..} = do
    status <- liftIO $ printStatus rwaProcess
    $logInfo $ pack $ "Killing " <> unpack status <> " running on port: "  <> show rwaPort
    liftIO $ terminateMonitoredProcess rwaProcess

ensureAlive :: TVar AppState -> RunningWebApp -> IO ()
ensureAlive tstate RunningWebApp {..} = do
    didAnswer <- testApp rwaPort
    if didAnswer
        then return ()
        else error $ "ensureAlive failed, this means keter couldn't " <>
                      "detect your app at port " <> show rwaPort <>
                      ", check your app logs detailed errors. " <>
                      " Also make sure your app binds to the PORT environment variable (not YESOD_PORT for example)." -- TODO domain name would be good to add as well
  where
    testApp :: Port -> IO Bool
    testApp port = do
        res <- timeout rwaEnsureAliveTimeOut testApp'
        return $ fromMaybe False res
      where
        testApp' = do
            threadDelay $ 2 * 1000 * 1000
            eres <- try $ connectTo "127.0.0.1" $ show port
            case eres of
                Left (_ :: IOException) -> do
                    testApp'required <- not <$> hasNextStartingAction
                    if testApp'required then testApp' else return False
                Right handle -> do
                    hClose handle
                    return True
        hasNextStartingAction :: IO Bool
        hasNextStartingAction = do
            state <- readTVarIO tstate
            case state of
                ASStarting _app _time tmaction -> isJust <$> readTVarIO tmaction
                _ -> return False
        connectTo host serv = do
            let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                                     , addrSocketType = Stream }
            addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
            firstSuccessful $ map tryToConnect addrs
            where
              tryToConnect addr =
                bracketOnError
                  (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
                  close  -- only done if there's an error
                  (\sock -> do
                    connect sock (addrAddress addr)
                    socketToHandle sock ReadWriteMode
                  )
              firstSuccessful = go Nothing
                where
                  go _ (p:ps) = do
                    r <- tryIO p
                    case r of
                          Right x -> return x
                          Left  e -> go (Just e) ps
                 -- All operations failed, throw error if one exists
                  go Nothing  [] = ioError $ userError "connectTo firstSuccessful: empty list"
                  go (Just e) [] = throwIO e
                  tryIO :: IO a -> IO (Either IOException a)
                  tryIO m = catch (fmap Right m) (return . Left)


withBackgroundApps :: AppId
                   -> BundleConfig
                   -> Maybe FilePath
                   -> Logger
                   -> [BackgroundConfig]
                   -> ([RunningBackgroundApp] -> KeterM AppStartConfig a)
                   -> KeterM AppStartConfig a
withBackgroundApps aid bconfig mdir appLogger configs f =
    withRunInIO $ \rio -> bracketedMap (\cfg f' -> rio $ alloc cfg (liftIO <$> f')) (rio . f) configs
  where
    alloc = launchBackgroundApp aid bconfig mdir appLogger

launchBackgroundApp :: AppId
                    -> BundleConfig
                    -> Maybe FilePath
                    -> Logger
                    -> BackgroundConfig
                    -> (RunningBackgroundApp -> IO a)
                    -> KeterM AppStartConfig a
launchBackgroundApp aid BundleConfig {..} mdir appLogger BackgroundConfig {..} f = do
    AppStartConfig{..} <- ask
    otherEnv <- liftIO $ pluginsGetEnv ascPlugins name bconfigPlugins
    forwardedEnv <- liftIO $ getForwardedEnv bgconfigForwardEnv
    let env = Map.toList $ Map.unions
            -- Order matters as in launchWebApp
            [ bgconfigEnvironment
            , forwardedEnv
            , Map.fromList otherEnv
            , kconfigEnvironment ascKeterConfig
            ]
    exec <- liftIO $ canonicalizePath bgconfigExec

    let delay = threadDelay $ fromIntegral $ bgconfigRestartDelaySeconds * 1000 * 1000
    shouldRestart <-
        case bgconfigRestartCount of
            UnlimitedRestarts -> return $ do
                delay
                return True
            LimitedRestarts maxCount -> do
                icount <- liftIO $ newIORef 0
                return $ do
                    res <- atomicModifyIORef icount $ \count ->
                        (count + 1, count < maxCount)
                    when res delay
                    return res
    withRunInIO $ \rio -> bracketOnError
        (rio $ monitorProcess
            ascProcessTracker
            (encodeUtf8 . fst <$> ascSetuid)
            (encodeUtf8 $ pack exec)
            (maybe "/tmp" (encodeUtf8 . pack) mdir)
            (map encodeUtf8 $ V.toList bgconfigArgs)
            (map (encodeUtf8 *** encodeUtf8) env)
            (Log.loggerLog appLogger . formatAppLog aid (Log.loggerType appLogger) . toLogStr)
            (const shouldRestart))
        terminateMonitoredProcess
        (f . RunningBackgroundApp)
  where
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> x

killBackgroundApp :: RunningBackgroundApp -> IO ()
killBackgroundApp RunningBackgroundApp {..} = do
    terminateMonitoredProcess rbaProcess

    {-
start :: TempFolder
      -> Maybe (Text, (UserID, GroupID))
      -> ProcessTracker
      -> HostManager
      -> Plugins
      -> RotatingLog
      -> Appname
      -> (Maybe BundleConfig)
      -> KIO () -- ^ action to perform to remove this App from list of actives
      -> KIO (App, KIO ())
start tf muid processTracker portman plugins appLogger appname bundle removeFromList = do
    Prelude.error "FIXME Keter.App.start"
    chan <- newChan
    return (App $ writeChan chan, rest chan)
  where

    rest chan = forkKIO $ do
        mres <- unpackBundle tf (snd <$> muid) bundle appname
        case mres of
            Left e -> do
                $logEx e
                removeFromList
            Right (dir, config) -> do
                let common = do
                        mapM_ (\StaticHost{..} -> addEntry portman shHost (PEStatic shRoot)) $ Set.toList $ bconfigStaticHosts config
                        mapM_ (\Redirect{..} -> addEntry portman redFrom (PERedirect $ encodeUtf8 redTo)) $ Set.toList $ bconfigRedirects config
                case bconfigApp config of
                    Nothing -> do
                        common
                        loop chan dir config Nothing
                    Just appconfig -> do
                        eport <- getPort portman
                        case eport of
                            Left e -> do
                                $logEx e
                                removeFromList
                            Right port -> do
                                eprocess <- runApp port dir appconfig
                                case eprocess of
                                    Left e -> do
                                        $logEx e
                                        removeFromList
                                    Right process -> do
                                        b <- testApp port
                                        if b
                                            then do
                                                addEntry portman (aconfigHost appconfig) $ PEPort port
                                                mapM_ (flip (addEntry portman) $ PEPort port) $ Set.toList $ aconfigExtraHosts appconfig
                                                common
                                                loop chan dir config $ Just (process, port)
                                            else do
                                                removeFromList
                                                releasePort portman port
                                                void $ liftIO $ terminateMonitoredProcess process

    loop chan dirOld configOld mprocPortOld = do
        command <- readChan chan
        case command of
            Terminate -> do
                removeFromList
                case bconfigApp configOld of
                    Nothing -> return ()
                    Just appconfig -> do
                        removeEntry portman $ aconfigHost appconfig
                        mapM_ (removeEntry portman) $ Set.toList $ aconfigExtraHosts appconfig
                mapM_ (removeEntry portman) $ map shHost $ Set.toList $ bconfigStaticHosts configOld
                mapM_ (removeEntry portman) $ map redFrom $ Set.toList $ bconfigRedirects configOld
                log $ TerminatingApp appname
                terminateOld
            Reload -> do
                mres <- unpackBundle tf (snd <$> muid) bundle appname
                case mres of
                    Left e -> do
                        log $ InvalidBundle bundle e
                        loop chan dirOld configOld mprocPortOld
                    Right (dir, config) -> do
                        eport <- getPort portman
                        case eport of
                            Left e -> $logEx e
                            Right port -> do
                                let common = do
                                        mapM_ (\StaticHost{..} -> addEntry portman shHost (PEStatic shRoot)) $ Set.toList $ bconfigStaticHosts config
                                        mapM_ (\Redirect{..} -> addEntry portman redFrom (PERedirect $ encodeUtf8 redTo)) $ Set.toList $ bconfigRedirects config
                                case bconfigApp config of
                                    Nothing -> do
                                        common
                                        loop chan dir config Nothing
                                    Just appconfig -> do
                                        eprocess <- runApp port dir appconfig
                                        mprocess <-
                                            case eprocess of
                                                Left _ -> return Nothing
                                                Right process -> do
                                                    b <- testApp port
                                                    return $ if b
                                                        then Just process
                                                        else Nothing
                                        case mprocess of
                                            Just process -> do
                                                addEntry portman (aconfigHost appconfig) $ PEPort port
                                                mapM_ (flip (addEntry portman) $ PEPort port) $ Set.toList $ aconfigExtraHosts appconfig
                                                common
                                                case bconfigApp configOld of
                                                    Just appconfigOld | aconfigHost appconfig /= aconfigHost appconfigOld ->
                                                        removeEntry portman $ aconfigHost appconfigOld
                                                    _ -> return ()
                                                log $ FinishedReloading appname
                                                terminateOld
                                                loop chan dir config $ Just (process, port)
                                            Nothing -> do
                                                releasePort portman port
                                                case eprocess of
                                                    Left _ -> return ()
                                                    Right process -> void $ liftIO $ terminateMonitoredProcess process
                                                log $ ProcessDidNotStart bundle
                                                loop chan dirOld configOld mprocPortOld
      where
        terminateOld = forkKIO $ do
    -}

reload :: AppInput -> TVar AppState -> KeterM App ()
reload input tstate = do
    App{..} <- ask
    withMappedConfig (const appAsc) $
      withLogger appId (Just appLog) $ \_ appLogger ->
      withConfig appId input $ \newdir bconfig mmodtime ->
      withSanityChecks bconfig $
      withReservations appId bconfig $ \webapps backs actions ->
      withBackgroundApps appId bconfig newdir appLogger backs $ \runningBacks ->
      withWebApps appId bconfig newdir appLogger webapps $ \runningWebapps -> do
          liftIO $ mapM_ (ensureAlive tstate) runningWebapps
          liftIO (readTVarIO appHosts) >>= \hosts ->
            withMappedConfig (const $ ascHostManager appAsc) $
              reactivateApp appId actions hosts
          (oldApps, oldBacks, oldDir, oldRlog) <- liftIO $ atomically $ do
              oldApps <- readTVar appRunningWebApps
              oldBacks <- readTVar appBackgroundApps
              oldDir <- readTVar appDir
              oldRlog <- readTVar appLog

              writeTVar appModTime mmodtime
              writeTVar appRunningWebApps runningWebapps
              writeTVar appBackgroundApps runningBacks
              writeTVar appHosts $ Map.keysSet actions
              writeTVar appDir newdir
              return (oldApps, oldBacks, oldDir, oldRlog)
          void $ withRunInIO $ \rio ->
            forkIO $ rio $ terminateHelper appId oldApps oldBacks oldDir oldRlog

terminate :: KeterM App ()
terminate = do
    App{..} <- ask
    let AppStartConfig {..} = appAsc
    (hosts, apps, backs, mdir, appLogger) <- liftIO $ atomically $ do
        hosts <- readTVar appHosts
        apps <- readTVar appRunningWebApps
        backs <- readTVar appBackgroundApps
        mdir <- readTVar appDir
        appLogger <- readTVar appLog

        writeTVar appModTime Nothing
        writeTVar appRunningWebApps []
        writeTVar appBackgroundApps []
        writeTVar appHosts Set.empty
        writeTVar appDir Nothing
        writeTVar appLog Nothing

        return (hosts, apps, backs, mdir, appLogger)

    withMappedConfig (const ascHostManager) $
        deactivateApp appId hosts

    void $ withRunInIO $ \rio ->
      forkIO $ rio $ withMappedConfig (const appAsc) $
        terminateHelper appId apps backs mdir appLogger
    liftIO $ maybe (return ()) Log.loggerClose appLogger

terminateHelper :: AppId
                -> [RunningWebApp]
                -> [RunningBackgroundApp]
                -> Maybe FilePath
                -> Maybe Logger
                -> KeterM AppStartConfig ()
terminateHelper aid apps backs mdir _appLogger = do
    liftIO $ threadDelay $ 20 * 1000 * 1000
    $logInfo $ pack $
        "Sending old process TERM signal: "
          ++ case aid of { AINamed t -> unpack t; AIBuiltin -> "builtin" }
    mapM_ killWebApp apps
    liftIO $ do
        mapM_ killBackgroundApp backs
        threadDelay $ 60 * 1000 * 1000
    case mdir of
        Nothing -> return ()
        Just dir -> do
            $logInfo $ pack $ "Removing unneeded folder: " ++ dir
            res <- liftIO $ try @SomeException $ removeDirectoryRecursive dir
            case res of
                Left e -> $logError $ pack $ show e
                Right () -> return ()

-- | Get the modification time of the bundle file this app was launched from,
-- if relevant.
getTimestamp :: App -> STM (Maybe EpochTime)
getTimestamp = readTVar . appModTime

pluginsGetEnv :: Plugins -> Appname -> Object -> IO [(Text, Text)]
pluginsGetEnv ps app o = concat <$> mapM (\p -> pluginGetEnv p app o) ps

-- | For the forward-env option. From a Set of desired variables, create a
-- Map pulled from the system environment.
getForwardedEnv :: Set Text -> IO (Map Text Text)
getForwardedEnv vars = filterEnv <$> getEnvironment
  where
    filterEnv = Map.filterWithKey (\k _ -> Set.member k vars)
              . Map.fromList
              . map (pack *** pack)


    {- FIXME handle static stanzas
    let staticReverse r = do
            HostMan.addEntry hostman (ReverseProxy.reversingHost r)
                $ HostMan.PEReverseProxy
                $ ReverseProxy.RPEntry r manager
    runKIO' $ mapM_ staticReverse (Set.toList kconfigReverseProxy)
    -}

{- FIXME
            rest <-
                case Map.lookup appname appMap of
                    Just (app, _time) -> do
                        App.reload app
                        etime <- liftIO $ modificationTime <$> getFileStatus (F.encodeString bundle)
                        let time = either (P.const 0) id etime
                        return (Map.insert appname (app, time) appMap, return ())
                    Nothing -> do
                        mappLogger <- do
                            let dirout = kconfigDir </> "log" </> fromText ("app-" ++ appname)
                                direrr = dirout </> "err"
                            eappLogger <- liftIO $ Log.openRotatingLog
                                (F.encodeString dirout)
                                Log.defaultMaxTotal
                            case eappLogger of
                                Left e -> do
                                    $logEx e
                                    return Nothing
                                Right appLogger -> return (Just appLogger)
                        let appLogger = fromMaybe Log.dummy mappLogger
                        (app, rest) <- App.start
                            tf
                            muid
                            processTracker
                            hostman
                            plugins
                            appLogger
                            appname
                            bundle
                            (removeApp appname)
                        etime <- liftIO $ modificationTime <$> getFileStatus (F.encodeString bundle)
                        let time = either (P.const 0) id etime
                        let appMap' = Map.insert appname (app, time) appMap
                        return (appMap', rest)
            rest
            -}
