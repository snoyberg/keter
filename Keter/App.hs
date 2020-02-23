{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Keter.App
    ( App
    , AppStartConfig (..)
    , start
    , reload
    , getTimestamp
    , Keter.App.terminate
    ) where

import           Codec.Archive.TempTarball
import           Control.Applicative       ((<$>), (<*>))
import           Control.Arrow             ((***))
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception         (IOException, bracketOnError,
                                            throwIO, try)
import           Control.Monad             (void, when)
import qualified Data.CaseInsensitive      as CI
import           Data.Conduit.LogFile      (RotatingLog)
import qualified Data.Conduit.LogFile      as LogFile
import           Data.Conduit.Process.Unix (MonitoredProcess, ProcessTracker,
                                            monitorProcess,
                                            terminateMonitoredProcess)
import           Data.IORef
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>), mempty)
import qualified Data.Set                  as Set
import           Data.Text                 (pack, unpack)
import           Data.Text.Encoding        (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error  (lenientDecode)
import qualified Data.Vector               as V
import           Data.Yaml
import           Data.Yaml.FilePath
import System.FilePath ((</>))
import           System.Directory          (canonicalizePath, doesFileExist,
                                            removeDirectoryRecursive)
import           Keter.HostManager         hiding (start)
import           Keter.PortPool            (PortPool, getPort, releasePort)
import           Keter.Types
import qualified Network
import           Prelude                   hiding (FilePath)
import           System.Environment        (getEnvironment)
import           System.IO                 (hClose)
import           System.Posix.Files        (fileAccess)
import           System.Posix.Types        (EpochTime, GroupID, UserID)
import           System.Timeout            (timeout)
import qualified Network.TLS as TLS

data App = App
    { appModTime        :: !(TVar (Maybe EpochTime))
    , appRunningWebApps :: !(TVar [RunningWebApp])
    , appBackgroundApps :: !(TVar [RunningBackgroundApp])
    , appId             :: !AppId
    , appHosts          :: !(TVar (Set Host))
    , appDir            :: !(TVar (Maybe FilePath))
    , appAsc            :: !AppStartConfig
    , appRlog           :: !(TVar (Maybe RotatingLog))
    }

data RunningWebApp = RunningWebApp
    { rwaProcess :: !MonitoredProcess
    , rwaPort    :: !Port
    }

newtype RunningBackgroundApp = RunningBackgroundApp
    { rbaProcess :: MonitoredProcess
    }

unpackBundle :: AppStartConfig
             -> FilePath
             -> AppId
             -> IO (FilePath, BundleConfig)
unpackBundle AppStartConfig {..} bundle aid = do
    ascLog $ UnpackingBundle bundle
    unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
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

data AppStartConfig = AppStartConfig
    { ascTempFolder     :: !TempFolder
    , ascSetuid         :: !(Maybe (Text, (UserID, GroupID)))
    , ascProcessTracker :: !ProcessTracker
    , ascHostManager    :: !HostManager
    , ascPortPool       :: !PortPool
    , ascPlugins        :: !Plugins
    , ascLog            :: !(LogMessage -> IO ())
    , ascKeterConfig    :: !KeterConfig
    }

withConfig :: AppStartConfig
           -> AppId
           -> AppInput
           -> (Maybe FilePath -> BundleConfig -> Maybe EpochTime -> IO a)
           -> IO a
withConfig _asc _aid (AIData bconfig) f = f Nothing bconfig Nothing
withConfig asc aid (AIBundle fp modtime) f = bracketOnError
    (unpackBundle asc fp aid)
    (\(newdir, _) -> removeDirectoryRecursive newdir)
    $ \(newdir, bconfig) -> f (Just newdir) bconfig (Just modtime)

withReservations :: AppStartConfig
                 -> AppId
                 -> BundleConfig
                 -> ([WebAppConfig Port] -> [BackgroundConfig] -> Map Host (ProxyAction, TLS.Credentials) -> IO a)
                 -> IO a
withReservations asc aid bconfig f = withActions asc bconfig $ \wacs backs actions -> bracketOnError
    (reserveHosts (ascLog asc) (ascHostManager asc) aid $ Map.keysSet actions)
    (forgetReservations (ascLog asc) (ascHostManager asc) aid)
    (const $ f wacs backs actions)

withActions :: AppStartConfig
            -> BundleConfig
            -> ([WebAppConfig Port] -> [BackgroundConfig] -> Map Host (ProxyAction, TLS.Credentials) -> IO a)
            -> IO a
withActions asc bconfig f =
    loop (V.toList $ bconfigStanzas bconfig) [] [] Map.empty
  where
    loop [] wacs backs actions = f wacs backs actions
    loop (Stanza (StanzaWebApp wac) rs:stanzas) wacs backs actions = bracketOnError
        (
           getPort (ascLog asc) (ascPortPool asc) >>= either throwIO
             (\p -> do
                c <- case waconfigSsl wac of
                       -- todo: add loading from relative location
                       SSL certFile chainCertFiles keyFile ->
                              either (const mempty) (TLS.Credentials . (:[])) <$>
                                  TLS.credentialLoadX509Chain certFile (V.toList chainCertFiles) keyFile
                       _ -> return mempty
                return (p, c)
             )
        )
        (\(port, _) -> releasePort (ascPortPool asc) port)
        (\(port, cert) -> loop
            stanzas
            (wac { waconfigPort = port } : wacs)
            backs
            (Map.unions $ actions : map (\host -> Map.singleton host ((PAPort port (waconfigTimeout wac), rs), cert)) hosts))
      where
        hosts = Set.toList $ Set.insert (waconfigApprootHost wac) (waconfigHosts wac)
    loop (Stanza (StanzaStaticFiles sfc) rs:stanzas) wacs backs actions0 =
        loop stanzas wacs backs actions
      where
        actions = Map.unions
                $ actions0
                : map (\host -> Map.singleton host ((PAStatic sfc, rs), mempty))
                  (Set.toList (sfconfigHosts sfc))
    loop (Stanza (StanzaRedirect red) rs:stanzas) wacs backs actions0 =
        loop stanzas wacs backs actions
      where
        actions = Map.unions
                $ actions0
                : map (\host -> Map.singleton host ((PARedirect red, rs), mempty))
                  (Set.toList (redirconfigHosts red))
    loop (Stanza (StanzaReverseProxy rev mid to) rs:stanzas) wacs backs actions0 =
        loop stanzas wacs backs actions
      where
        actions = Map.insert (CI.mk $ reversingHost rev) ((PAReverseProxy rev mid to, rs), mempty) actions0
    loop (Stanza (StanzaBackground back) _:stanzas) wacs backs actions =
        loop stanzas wacs (back:backs) actions

withRotatingLog :: AppStartConfig
                -> AppId
                -> Maybe (TVar (Maybe RotatingLog))
                -> ((TVar (Maybe RotatingLog)) -> RotatingLog -> IO a)
                -> IO a
withRotatingLog asc aid Nothing f = do
    var <- newTVarIO Nothing
    withRotatingLog asc aid (Just var) f
withRotatingLog AppStartConfig {..} aid (Just var) f = do
    mrlog <- readTVarIO var
    case mrlog of
        Nothing -> bracketOnError
            (LogFile.openRotatingLog dir LogFile.defaultMaxTotal)
            LogFile.close
            (f var)
        Just rlog ->  f var rlog
  where
    dir = kconfigDir ascKeterConfig </> "log" </> name
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> unpack $ "app-" <> x

withSanityChecks :: AppStartConfig -> BundleConfig -> IO a -> IO a
withSanityChecks AppStartConfig {..} BundleConfig {..} f = do
    V.mapM_ go bconfigStanzas
    ascLog SanityChecksPassed
    f
  where
    go (Stanza (StanzaWebApp WebAppConfig {..}) _) = isExec waconfigExec
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

start :: AppStartConfig
      -> AppId
      -> AppInput
      -> IO App
start asc aid input =
    withRotatingLog asc aid Nothing $ \trlog rlog ->
    withConfig asc aid input $ \newdir bconfig mmodtime ->
    withSanityChecks asc bconfig $
    withReservations asc aid bconfig $ \webapps backs actions ->
    withBackgroundApps asc aid bconfig newdir rlog backs $ \runningBacks ->
    withWebApps asc aid bconfig newdir rlog webapps $ \runningWebapps -> do
        mapM_ ensureAlive runningWebapps
        activateApp (ascLog asc) (ascHostManager asc) aid actions
        App
            <$> newTVarIO mmodtime
            <*> newTVarIO runningWebapps
            <*> newTVarIO runningBacks
            <*> return aid
            <*> newTVarIO (Map.keysSet actions)
            <*> newTVarIO newdir
            <*> return asc
            <*> return trlog

bracketedMap :: (a -> (b -> IO c) -> IO c)
             -> ([b] -> IO c)
             -> [a]
             -> IO c
bracketedMap with inside =
    loop id
  where
    loop front [] = inside $ front []
    loop front (c:cs) = with c $ \x -> loop (front . (x:)) cs

withWebApps :: AppStartConfig
            -> AppId
            -> BundleConfig
            -> Maybe FilePath
            -> RotatingLog
            -> [WebAppConfig Port]
            -> ([RunningWebApp] -> IO a)
            -> IO a
withWebApps asc aid bconfig mdir rlog configs0 f =
    bracketedMap alloc f configs0
  where
    alloc = launchWebApp asc aid bconfig mdir rlog

launchWebApp :: AppStartConfig
             -> AppId
             -> BundleConfig
             -> Maybe FilePath
             -> RotatingLog
             -> WebAppConfig Port
             -> (RunningWebApp -> IO a)
             -> IO a
launchWebApp AppStartConfig {..} aid BundleConfig {..} mdir rlog WebAppConfig {..} f = do
    otherEnv <- pluginsGetEnv ascPlugins name bconfigPlugins
    forwardedEnv <- getForwardedEnv waconfigForwardEnv
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
    exec <- canonicalizePath waconfigExec
    bracketOnError
        (monitorProcess
            (ascLog . OtherMessage . decodeUtf8With lenientDecode)
            ascProcessTracker
            (encodeUtf8 . fst <$> ascSetuid)
            (encodeUtf8 $ pack exec)
            (maybe "/tmp" (encodeUtf8 . pack) mdir)
            (map encodeUtf8 $ V.toList waconfigArgs)
            (map (encodeUtf8 *** encodeUtf8) env)
            (LogFile.addChunk rlog)
            (const $ return True))
        terminateMonitoredProcess
        $ \mp -> f RunningWebApp
            { rwaProcess = mp
            , rwaPort = waconfigPort
            }
  where
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> x

killWebApp :: RunningWebApp -> IO ()
killWebApp RunningWebApp {..} = do
    terminateMonitoredProcess rwaProcess

ensureAlive :: RunningWebApp -> IO ()
ensureAlive RunningWebApp {..} = do
    didAnswer <- testApp rwaPort
    if didAnswer
        then return ()
        else error "ensureAlive failed"
  where
    testApp :: Port -> IO Bool
    testApp port = do
        res <- timeout (90 * 1000 * 1000) testApp'
        return $ fromMaybe False res
      where
        testApp' = do
            threadDelay $ 2 * 1000 * 1000
            eres <- try $ Network.connectTo "127.0.0.1" $ Network.PortNumber $ fromIntegral port
            case eres of
                Left (_ :: IOException) -> testApp'
                Right handle -> do
                    hClose handle
                    return True

withBackgroundApps :: AppStartConfig
                   -> AppId
                   -> BundleConfig
                   -> Maybe FilePath
                   -> RotatingLog
                   -> [BackgroundConfig]
                   -> ([RunningBackgroundApp] -> IO a)
                   -> IO a
withBackgroundApps asc aid bconfig mdir rlog configs f =
    bracketedMap alloc f configs
  where
    alloc = launchBackgroundApp asc aid bconfig mdir rlog

launchBackgroundApp :: AppStartConfig
                    -> AppId
                    -> BundleConfig
                    -> Maybe FilePath
                    -> RotatingLog
                    -> BackgroundConfig
                    -> (RunningBackgroundApp -> IO a)
                    -> IO a
launchBackgroundApp AppStartConfig {..} aid BundleConfig {..} mdir rlog BackgroundConfig {..} f = do
    otherEnv <- pluginsGetEnv ascPlugins name bconfigPlugins
    forwardedEnv <- getForwardedEnv bgconfigForwardEnv
    let env = Map.toList $ Map.unions
            -- Order matters as in launchWebApp
            [ bgconfigEnvironment
            , forwardedEnv
            , Map.fromList otherEnv
            , kconfigEnvironment ascKeterConfig
            ]
    exec <- canonicalizePath bgconfigExec

    let delay = threadDelay $ fromIntegral $ bgconfigRestartDelaySeconds * 1000 * 1000
    shouldRestart <-
        case bgconfigRestartCount of
            UnlimitedRestarts -> return $ do
                delay
                return True
            LimitedRestarts maxCount -> do
                icount <- newIORef 0
                return $ do
                    res <- atomicModifyIORef icount $ \count ->
                        (count + 1, count < maxCount)
                    when res delay
                    return res

    bracketOnError
        (monitorProcess
            (ascLog . OtherMessage . decodeUtf8With lenientDecode)
            ascProcessTracker
            (encodeUtf8 . fst <$> ascSetuid)
            (encodeUtf8 $ pack exec)
            (maybe "/tmp" (encodeUtf8 . pack) mdir)
            (map encodeUtf8 $ V.toList bgconfigArgs)
            (map (encodeUtf8 *** encodeUtf8) env)
            (LogFile.addChunk rlog)
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
start tf muid processTracker portman plugins rlog appname bundle removeFromList = do
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

reload :: App -> AppInput -> IO ()
reload App {..} input =
    withRotatingLog appAsc appId (Just appRlog) $ \_ rlog ->
    withConfig appAsc appId input $ \newdir bconfig mmodtime ->
    withSanityChecks appAsc bconfig $
    withReservations appAsc appId bconfig $ \webapps backs actions ->
    withBackgroundApps appAsc appId bconfig newdir rlog backs $ \runningBacks ->
    withWebApps appAsc appId bconfig newdir rlog webapps $ \runningWebapps -> do
        mapM_ ensureAlive runningWebapps
        readTVarIO appHosts >>= reactivateApp (ascLog appAsc) (ascHostManager appAsc) appId actions
        (oldApps, oldBacks, oldDir) <- atomically $ do
            oldApps <- readTVar appRunningWebApps
            oldBacks <- readTVar appBackgroundApps
            oldDir <- readTVar appDir

            writeTVar appModTime mmodtime
            writeTVar appRunningWebApps runningWebapps
            writeTVar appBackgroundApps runningBacks
            writeTVar appHosts $ Map.keysSet actions
            writeTVar appDir newdir
            return (oldApps, oldBacks, oldDir)
        void $ forkIO $ terminateHelper appAsc appId oldApps oldBacks oldDir

terminate :: App -> IO ()
terminate App {..} = do
    (hosts, apps, backs, mdir, rlog) <- atomically $ do
        hosts <- readTVar appHosts
        apps <- readTVar appRunningWebApps
        backs <- readTVar appBackgroundApps
        mdir <- readTVar appDir
        rlog <- readTVar appRlog

        writeTVar appModTime Nothing
        writeTVar appRunningWebApps []
        writeTVar appBackgroundApps []
        writeTVar appHosts Set.empty
        writeTVar appDir Nothing
        writeTVar appRlog Nothing

        return (hosts, apps, backs, mdir, rlog)

    deactivateApp ascLog ascHostManager appId hosts
    void $ forkIO $ terminateHelper appAsc appId apps backs mdir
    maybe (return ()) LogFile.close rlog
  where
    AppStartConfig {..} = appAsc

terminateHelper :: AppStartConfig
                -> AppId
                -> [RunningWebApp]
                -> [RunningBackgroundApp]
                -> Maybe FilePath
                -> IO ()
terminateHelper AppStartConfig {..} aid apps backs mdir = do
    threadDelay $ 20 * 1000 * 1000
    ascLog $ TerminatingOldProcess aid
    mapM_ killWebApp apps
    mapM_ killBackgroundApp backs
    threadDelay $ 60 * 1000 * 1000
    case mdir of
        Nothing -> return ()
        Just dir -> do
            ascLog $ RemovingOldFolder dir
            res <- try $ removeDirectoryRecursive dir
            case res of
                Left e -> $logEx ascLog e
                Right () -> return ()

-- | Get the modification time of the bundle file this app was launched from,
-- if relevant.
getTimestamp :: App -> STM (Maybe EpochTime)
getTimestamp = readTVar . appModTime

pluginsGetEnv :: Plugins -> Appname -> Object -> IO [(Text, Text)]
pluginsGetEnv ps app o = fmap concat $ mapM (\p -> pluginGetEnv p app o) ps

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
