{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Keter.Main
    ( keter
    ) where

import qualified Codec.Archive.TempTarball as TempFolder
import           Control.Concurrent.Async  (waitAny, withAsync)
import           Control.Monad             (unless)
import qualified Data.Conduit.LogFile      as LogFile
import           Data.Monoid               (mempty)
import qualified Data.Vector               as V
import           Keter.App                 (AppStartConfig (..))
import qualified Keter.AppManager          as AppMan
import qualified Keter.HostManager         as HostMan
import qualified Keter.PortPool            as PortPool
import           Keter.Prelude             hiding (getCurrentTime, runKIO)
import qualified Keter.Proxy               as Proxy
import           Keter.Types
import           System.Posix.Files        (getFileStatus, modificationTime)
import           System.Posix.Signals      (Handler (Catch), installHandler,
                                            sigHUP)

import           Control.Applicative       ((<$>))
import           Control.Exception         (throwIO, try)
import           Control.Exception         (Exception)
import           Control.Monad             (forM)
import           Data.Conduit.Process.Unix (initProcessTracker)
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Text.Read
import           Data.Time                 (getCurrentTime)
import           Data.Typeable             (Typeable)
import           Data.Yaml                 (ParseException)
import           Data.Yaml.FilePath
import qualified Filesystem                as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Keter.Prelude
import qualified Network.HTTP.Conduit      as HTTP (newManager)
import           Prelude                   (IO)
import qualified Prelude                   as P
import qualified System.FSNotify           as FSN
import           System.Posix.User         (getUserEntryForID,
                                            getUserEntryForName, userGroupID,
                                            userID, userName)

keter :: F.FilePath -- ^ root directory or config file
      -> [F.FilePath -> P.IO Plugin]
      -> P.IO ()
keter input mkPlugins = withManagers input mkPlugins $ \kc hostman appMan -> do
    launchInitial kc appMan
    startWatching kc appMan
    startListening kc hostman

-- | Load up Keter config.
withConfig :: FilePath
           -> (KeterConfig -> IO a)
           -> IO a
withConfig input f = do
    exists <- F.isFile input
    config <-
        if exists
            then do
                eres <- decodeFileRelative input
                case eres of
                    Left e -> throwIO $ InvalidKeterConfigFile input e
                    Right x -> return x
            else return def { kconfigDir = input }
    f config

withRunner :: FilePath
           -> (KeterConfig -> (forall a. KIO a -> IO a) -> IO b)
           -> IO b
withRunner fp f = withConfig fp $ \config -> do
    mainlog <- LogFile.openRotatingLog
        (F.encodeString $ (kconfigDir config) </> "log" </> "keter")
        LogFile.defaultMaxTotal

    f config $ Keter.Prelude.runKIO $ \ml -> do
            now <- getCurrentTime
            let bs = encodeUtf8 $ T.concat
                    [ T.take 22 $ show now
                    , ": "
                    , show ml
                    , "\n"
                    ]
            LogFile.addChunk mainlog bs

withManagers :: FilePath
             -> [FilePath -> IO Plugin]
             -> (KeterConfig -> HostMan.HostManager -> AppMan.AppManager -> IO a)
             -> IO a
withManagers input mkPlugins f = withRunner input $ \kc@KeterConfig {..} runKIO -> do
    processTracker <- initProcessTracker
    hostman <- HostMan.start
    portpool <- PortPool.start kconfigPortPool
    tf <- TempFolder.setup $ kconfigDir </> "temp"
    plugins <- P.sequence $ map ($ kconfigDir) mkPlugins
    muid <-
        case kconfigSetuid of
            Nothing -> return Nothing
            Just t -> do
                x <- try $
                    case Data.Text.Read.decimal t of
                        Right (i, "") -> getUserEntryForID i
                        _ -> getUserEntryForName $ T.unpack t
                case x of
                    Left (_ :: SomeException) -> P.error $ T.unpack $ "Invalid user ID: " ++ t
                    Right ue -> return $ Just (T.pack $ userName ue, (userID ue, userGroupID ue))

    let appStartConfig = AppStartConfig
            { ascTempFolder = tf
            , ascSetuid = muid
            , ascProcessTracker = processTracker
            , ascHostManager = hostman
            , ascPortPool = portpool
            , ascPlugins = plugins
            }
    appMan <- AppMan.initialize (AppMan.RunKIO runKIO) appStartConfig
    f kc hostman appMan

data InvalidKeterConfigFile = InvalidKeterConfigFile !FilePath !ParseException
    deriving (Show, Typeable)
instance Exception InvalidKeterConfigFile

launchInitial :: KeterConfig -> AppMan.AppManager -> IO ()
launchInitial kc@KeterConfig {..} appMan = do
    createTree incoming
    bundles0 <- fmap (filter isKeter) $ listDirectory incoming
    mapM_ (AppMan.addApp appMan) bundles0

    unless (V.null kconfigBuiltinStanzas) $ AppMan.perform
        appMan
        AppMan.AIBuiltin
        (AppMan.Reload $ AppMan.AIData $ BundleConfig kconfigBuiltinStanzas mempty)
  where
    incoming = getIncoming kc

getIncoming :: KeterConfig -> FilePath
getIncoming kc = kconfigDir kc </> "incoming"

isKeter :: FilePath -> Bool
isKeter fp = hasExtension fp "keter"

startWatching :: KeterConfig -> AppMan.AppManager -> IO ()
startWatching kc@KeterConfig {..} appMan = do
    -- File system watching
    wm <- FSN.startManager
    FSN.watchDir wm incoming (P.const True) $ \e ->
        let e' =
                case e of
                    FSN.Removed fp _ -> Left fp
                    FSN.Added fp _ -> Right fp
                    FSN.Modified fp _ -> Right fp
         in case e' of
            Left fp -> when (isKeter fp) $ AppMan.terminateApp appMan $ getAppname fp
            Right fp -> when (isKeter fp) $ AppMan.addApp appMan $ incoming </> fp

    -- Install HUP handler for cases when inotify cannot be used.
    void $ flip (installHandler sigHUP) Nothing $ Catch $ do
        bundles <- fmap (filter isKeter) $ F.listDirectory incoming
        newMap <- fmap Map.fromList $ forM bundles $ \bundle -> do
            time <- modificationTime <$> getFileStatus (F.encodeString bundle)
            return (getAppname bundle, (bundle, time))
        AppMan.reloadAppList appMan newMap
  where
    incoming = getIncoming kc

startListening :: KeterConfig -> HostMan.HostManager -> IO ()
startListening KeterConfig {..} hostman = do
    manager <- HTTP.newManager def
    runAndBlock kconfigListeners $ Proxy.reverseProxy
        kconfigIpFromHeader
        manager
        (HostMan.lookupAction hostman)

runAndBlock :: NonEmptyVector a
            -> (a -> P.IO ())
            -> P.IO ()
runAndBlock (NonEmptyVector x0 v) f =
    loop l0 []
  where
    l0 = x0 : V.toList v

    loop (x:xs) asyncs = withAsync (f x) $ \async -> loop xs $ async : asyncs
    -- Once we have all of our asyncs, we wait for /any/ of them to exit. If
    -- any listener thread exits, we kill the whole process.
    loop [] asyncs = void $ waitAny asyncs
