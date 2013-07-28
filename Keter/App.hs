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
import           Control.Applicative       ((<$>))
import           Control.Arrow             ((***))
import           Control.Concurrent.STM
import           Control.Exception         (bracketOnError, throwIO)
import qualified Data.Conduit.LogFile      as LogFile
import           Data.Conduit.Process.Unix (MonitoredProcess, ProcessTracker,
                                            RotatingLog, monitorProcess,
                                            terminateMonitoredProcess)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import qualified Data.Set                  as Set
import           Data.Text                 (pack)
import           Data.Text.Encoding        (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error  (lenientDecode)
import qualified Data.Vector               as V
import           Data.Yaml
import           Data.Yaml.FilePath
import           Filesystem                (removeTree, isFile, canonicalizePath)
import qualified Filesystem.Path.CurrentOS as F
import           Keter.HostManager         hiding (start)
import           Keter.PortPool            (PortPool, getPort, releasePort)
import           Keter.Types
import           Prelude                   hiding (FilePath)
import           System.Posix.Types        (EpochTime)
import           System.Posix.Types        (GroupID, UserID)
import           System.Posix.Files (fileAccess)
import System.IO (hClose)
import qualified Network
import Control.Exception (try, IOException)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)
import System.Timeout (timeout)

data App = App
    { appModTime        :: !(TVar (Maybe EpochTime))
    , appRunningWebApps :: ![RunningWebApp]
    }

data RunningWebApp = RunningWebApp
    { rwaProcess :: !MonitoredProcess
    , rwaPort :: !Port
    }

unpackBundle :: AppStartConfig
             -> FilePath
             -> AppId
             -> IO (FilePath, BundleConfig)
unpackBundle AppStartConfig {..} bundle aid = do
    ascLog $ UnpackingBundle bundle
    unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
        let configFP = dir F.</> "config" F.</> "keter.yaml"
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
           -> (FilePath -> BundleConfig -> Maybe EpochTime -> IO a)
           -> IO a
withConfig _asc _aid (AIData bconfig) f = f "/tmp" bconfig Nothing
withConfig asc aid (AIBundle fp modtime) f = bracketOnError
    (unpackBundle asc fp aid)
    (\(newdir, _) -> removeTree newdir)
    $ \(newdir, bconfig) -> f newdir bconfig (Just modtime)

withReservations :: AppStartConfig
                 -> AppId
                 -> BundleConfig
                 -> ([WebAppConfig Port] -> Map Host ProxyAction -> IO a)
                 -> IO a
withReservations asc aid bconfig f = withActions asc bconfig $ \wacs actions -> bracketOnError
    (reserveHosts (ascHostManager asc) aid $ Map.keysSet actions)
    (forgetReservations (ascHostManager asc) aid)
    (const $ f wacs actions)

withActions :: AppStartConfig
            -> BundleConfig
            -> ([WebAppConfig Port] -> Map Host ProxyAction -> IO a)
            -> IO a
withActions asc bconfig f =
    loop (V.toList $ bconfigStanzas bconfig) [] Map.empty
  where
    loop [] wacs actions = f wacs actions
    loop (StanzaWebApp wac:stanzas) wacs actions = bracketOnError
        (getPort (ascLog asc) (ascPortPool asc) >>= either throwIO return)
        (releasePort (ascPortPool asc))
        (\port -> loop
            stanzas
            (wac { waconfigPort = port } : wacs)
            (Map.unions $ actions : map (\host -> Map.singleton host $ PAPort port) hosts))
      where
        hosts = Set.toList $ Set.insert (waconfigApprootHost wac) (waconfigHosts wac)
    loop (StanzaStaticFiles sfc:stanzas) wacs actions0 =
        loop stanzas wacs actions
      where
        actions = Map.unions
                $ actions0
                : map (\host -> Map.singleton host $ PAStatic sfc)
                  (Set.toList (sfconfigHosts sfc))
    loop (StanzaRedirect red:stanzas) wacs actions0 =
        loop stanzas wacs actions
      where
        actions = Map.unions
                $ actions0
                : map (\host -> Map.singleton host $ PARedirect red)
                  (Set.toList (redirconfigHosts red))
    loop (StanzaReverseProxy rev:stanzas) wacs actions0 =
        loop stanzas wacs actions
      where
        actions = Map.insert (reversingHost rev) (PAReverseProxy rev) actions0

withRotatingLog :: AppStartConfig
                -> AppId
                -> (RotatingLog -> IO a)
                -> IO a
withRotatingLog AppStartConfig {..} aid = bracketOnError
    (LogFile.openRotatingLog (F.encodeString dir) LogFile.defaultMaxTotal)
    LogFile.close
  where
    dir = kconfigDir ascKeterConfig F.</> "log" F.</> name
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> F.fromText $ "app-" <> x

withSanityChecks :: AppStartConfig -> BundleConfig -> IO a -> IO a
withSanityChecks AppStartConfig {..} BundleConfig {..} f = do
    V.mapM_ go bconfigStanzas
    ascLog SanityChecksPassed
    f
  where
    go (StanzaWebApp WebAppConfig {..}) = do
        exists <- isFile waconfigExec
        if exists
            then do
                canExec <- fileAccess (F.encodeString waconfigExec) True False True
                if canExec
                    then return ()
                    else throwIO $ FileNotExecutable waconfigExec
            else throwIO $ ExecutableNotFound waconfigExec
    go _ = return ()

start :: AppStartConfig
      -> AppId
      -> AppInput
      -> IO App
start asc aid input =
    withConfig asc aid input $ \newdir bconfig mmodtime ->
    withSanityChecks asc bconfig $
    withReservations asc aid bconfig $ \webapps actions ->
    withRotatingLog asc aid $ \rlog ->
    withWebApps asc aid bconfig newdir rlog webapps $ \runningWebapps -> do
        mapM_ ensureAlive runningWebapps
        activateApp (ascHostManager asc) aid actions
        tmodtime <- newTVarIO mmodtime
        return App
            { appModTime = tmodtime
            , appRunningWebApps = runningWebapps
            }

withWebApps :: AppStartConfig
            -> AppId
            -> BundleConfig
            -> FilePath
            -> RotatingLog
            -> [WebAppConfig Port]
            -> ([RunningWebApp] -> IO a)
            -> IO a
withWebApps asc aid bconfig dir rlog configs0 f =
    loop configs0 id
  where
    loop [] front = f $ front []
    loop (c:cs) front = bracketOnError
        (launchWebApp asc aid bconfig dir rlog c)
        killWebApp
        (\rwa -> loop cs (front . (rwa:)))

launchWebApp :: AppStartConfig
             -> AppId
             -> BundleConfig
             -> FilePath
             -> RotatingLog
             -> WebAppConfig Port
             -> IO RunningWebApp
launchWebApp AppStartConfig {..} aid BundleConfig {..} dir rlog WebAppConfig {..} = do
    otherEnv <- pluginsGetEnv ascPlugins name bconfigRaw
    let env = ("PORT", pack $ show waconfigPort)
            : ("APPROOT", (if waconfigSsl then "https://" else "http://") <> waconfigApprootHost)
            : otherEnv
    exec <- canonicalizePath waconfigExec
    bracketOnError
        (monitorProcess
            (ascLog . OtherMessage . decodeUtf8With lenientDecode)
            ascProcessTracker
            (encodeUtf8 . fst <$> ascSetuid)
            (encodeUtf8 $ either id id $ F.toText exec)
            (encodeUtf8 $ either id id $ F.toText dir)
            (map encodeUtf8 $ V.toList waconfigArgs)
            (map (encodeUtf8 *** encodeUtf8) env)
            rlog)
        terminateMonitoredProcess
        $ \mp -> do
            return RunningWebApp
                { rwaProcess = mp
                , rwaPort = waconfigPort
                }
  where
    name =
        case aid of
            AIBuiltin -> "__builtin__"
            AINamed x -> x

killWebApp :: RunningWebApp -> IO ()
killWebApp = error "killWebApp"

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
            threadDelay $ 20 * 1000 * 1000
            log $ TerminatingOldProcess appname
            case mprocPortOld of
                Nothing -> return ()
                Just (processOld, _) -> void $ liftIO $ terminateMonitoredProcess processOld
            threadDelay $ 60 * 1000 * 1000
            log $ RemovingOldFolder dirOld
            res <- liftIO $ removeTree dirOld
            case res of
                Left e -> $logEx e
                Right () -> return ()
    -}

reload :: App -> AppInput -> IO ()
reload = error "FIXME"

terminate :: App -> IO ()
terminate = error "FIXME"

-- | Get the modification time of the bundle file this app was launched from,
-- if relevant.
getTimestamp :: App -> STM (Maybe EpochTime)
getTimestamp _ = return Nothing -- FIXME

pluginsGetEnv :: Plugins -> Appname -> Object -> IO [(Text, Text)]
pluginsGetEnv ps app o = fmap concat $ mapM (\p -> pluginGetEnv p app o) ps

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
