{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keter.Main
    ( keter
    ) where

import Keter.Prelude hiding (getCurrentTime)
import qualified Codec.Archive.TempTarball as TempFolder
import qualified Keter.App as App
import Keter.Types
import qualified Keter.PortManager as PortMan
import qualified Keter.Proxy as Proxy
import qualified Keter.ReverseProxy as ReverseProxy
import System.Posix.Files (modificationTime, getFileStatus)
import System.Posix.Signals (sigHUP, installHandler, Handler (Catch))
import qualified Data.Conduit.LogFile as LogFile

import Data.Yaml.FilePath
import Data.Aeson (withObject)
import Data.Conduit.Network (HostPreference)
import qualified Control.Concurrent.MVar as M
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import qualified System.FSNotify as FSN
import Control.Monad (forever, forM)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Control.Exception (throwIO, try)
import qualified Prelude as P
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Yaml ((.:?), (.!=))
import Control.Applicative ((<$>), (<*>))
import Data.String (fromString)
import System.Posix.User (userID, userGroupID, getUserEntryForName, getUserEntryForID, userName)
import qualified Data.Text.Read
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Network.HTTP.Conduit as HTTP (newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Conduit.Process.Unix (initProcessTracker)

data Config = Config
    { configDir :: F.FilePath
    , configPortMan :: PortMan.Settings
    , configHost :: HostPreference
    , configPort :: PortMan.Port
    , configSsl :: Maybe Proxy.TLSConfig
    , configSetuid :: Maybe Text
    , configReverseProxy :: Set ReverseProxy.ReverseProxyConfig
    , configIpFromHeader :: Bool
    }

instance Default Config where
    def = Config
        { configDir = "."
        , configPortMan = def
        , configHost = "*"
        , configPort = 80
        , configSsl = Nothing
        , configSetuid = Nothing
        , configReverseProxy = Set.empty
        , configIpFromHeader = False
        }

instance ParseYamlFile Config where
    parseYamlFile basedir = withObject "Config" $ \o -> Config
        <$> lookupBase basedir o "root"
        <*> o .:? "port-manager" .!= def
        <*> (fmap fromString <$> o .:? "host") .!= configHost def
        <*> o .:? "port" .!= configPort def
        <*> (o .:? "ssl" >>= maybe (return Nothing) (fmap Just . parseYamlFile basedir))
        <*> o .:? "setuid"
        <*> o .:? "reverse-proxy" .!= Set.empty
        <*> o .:? "ip-from-header" .!= False

keter :: P.FilePath -- ^ root directory or config file
      -> [F.FilePath -> KIO (Either SomeException Plugin)]
      -> P.IO ()
keter (F.decodeString -> input) mkPlugins = do
    exists <- F.isFile input
    Config{..} <-
        if exists
            then decodeFileRelative input >>= either
                    (\e -> P.error $ "Invalid config file: " ++ P.show e)
                    return
            else return def { configDir = input }

    muid <-
        case configSetuid of
            Nothing -> return Nothing
            Just t -> do
                x <- try $
                    case Data.Text.Read.decimal t of
                        Right (i, "") -> getUserEntryForID i
                        _ -> getUserEntryForName $ T.unpack t
                case x of
                    Left (_ :: SomeException) -> P.error $ T.unpack $ "Invalid user ID: " ++ t
                    Right ue -> return $ Just (T.pack $ userName ue, (userID ue, userGroupID ue))

    processTracker <- initProcessTracker
    portman <- runThrow $ PortMan.start configPortMan
    tf <- runThrow $ liftIO $ TempFolder.setup $ configDir </> "temp"
    plugins <- runThrow $ loadPlugins $ map ($ configDir) mkPlugins
    mainlog <- runThrow $ liftIO $ LogFile.openRotatingLog
        (F.encodeString $ configDir </> "log" </> "keter")
        LogFile.defaultMaxTotal

    let runKIO' = runKIO $ \ml -> do
            now <- getCurrentTime
            let bs = encodeUtf8 $ T.concat
                    [ T.take 22 $ show now
                    , ": "
                    , show ml
                    , "\n"
                    ]
            LogFile.addChunk mainlog bs
        runKIOPrint = runKIO P.print

    manager <- HTTP.newManager def
    _ <- forkIO $ Proxy.reverseProxy
            configIpFromHeader
            manager
            Warp.defaultSettings
                { Warp.settingsPort = configPort
                , Warp.settingsHost = configHost
                }
            (runKIOPrint . PortMan.lookupPort portman)
    case configSsl of
        Nothing -> return ()
        Just (Proxy.TLSConfig s ts) -> do
            _ <- forkIO $ Proxy.reverseProxySsl
                    configIpFromHeader
                    manager
                    ts
                    s
                    (runKIOPrint . PortMan.lookupPort portman)
            return ()

    mappMap <- M.newMVar Map.empty
    let removeApp appname = Keter.Prelude.modifyMVar_ mappMap $ return . Map.delete appname
        addApp bundle = do
            let appname = getAppname bundle
            rest <- modifyMVar mappMap $ \appMap ->
                case Map.lookup appname appMap of
                    Just (app, _time) -> do
                        App.reload app
                        etime <- liftIO $ modificationTime <$> getFileStatus (F.encodeString bundle)
                        let time = either (P.const 0) id etime
                        return (Map.insert appname (app, time) appMap, return ())
                    Nothing -> do
                        mlogger <- do
                            let dirout = configDir </> "log" </> fromText ("app-" ++ appname)
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
                            portman
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
        terminateApp appname = do
            -- FIXME why not remove it from the map?
            appMap <- M.readMVar mappMap
            case Map.lookup appname appMap of
                Nothing -> return ()
                Just (app, _) -> runKIO' $ App.terminate app

    let incoming = configDir </> "incoming"
        isKeter fp = hasExtension fp "keter"
    createTree incoming
    bundles0 <- fmap (filter isKeter) $ listDirectory incoming
    runKIO' $ mapM_ addApp bundles0

    let staticReverse r = do
            PortMan.addEntry portman (ReverseProxy.reversingHost r)
                $ PortMan.PEReverseProxy
                $ ReverseProxy.RPEntry r manager
    runKIO' $ mapM_ staticReverse (Set.toList configReverseProxy)

    -- File system watching
    wm <- FSN.startManager
    FSN.watchDir wm incoming (P.const True) $ \e ->
        let e' =
                case e of
                    FSN.Removed fp _ -> Left fp
                    FSN.Added fp _ -> Right fp
                    FSN.Modified fp _ -> Right fp
         in case e' of
            Left fp -> when (isKeter fp) $ terminateApp $ getAppname fp
            Right fp -> when (isKeter fp) $ runKIO' $ addApp $ incoming </> fp

    -- Install HUP handler for cases when inotify cannot be used.
    _ <- flip (installHandler sigHUP) Nothing $ Catch $ do
        actions <- M.withMVar mappMap $ \appMap -> do
            bundles <- fmap (filter isKeter) $ F.listDirectory incoming
            newMap <- fmap Map.fromList $ forM bundles $ \bundle -> do
                time <- modificationTime <$> getFileStatus (F.encodeString bundle)
                return (getAppname' $ F.encodeString bundle, (bundle, time))

            let apps = Set.toList $ Set.fromList (Map.keys newMap)
                        `Set.union` Set.fromList (Map.keys appMap)
            fmap catMaybes $ forM apps $ \appname -> return $
                case (Map.lookup appname appMap, Map.lookup appname newMap) of
                    (Nothing, Nothing) -> Nothing -- should never happen
                    (Just _, Nothing) -> Just $ terminateApp appname
                    (Nothing, Just (bundle, _)) -> Just $ runKIO' $ addApp bundle
                    (Just (_, oldTime), Just (bundle, newTime))
                        | newTime /= oldTime -> Just $ runKIO' $ addApp bundle
                        | otherwise -> Nothing
        P.sequence_ actions

    runKIO' $ forever $ threadDelay $ 60 * 1000 * 1000
  where
    getAppname = either id id . toText . basename
    getAppname' = getAppname . F.decodeString
    runThrow f = runKIO P.print f >>= either throwIO return

loadPlugins :: [KIO (Either SomeException Plugin)]
            -> KIO (Either SomeException Plugins)
loadPlugins =
    loop id
  where
    loop front [] = return $ Right $ front []
    loop front (x:xs) = do
        eres <- x
        case eres of
            Left e -> return $ Left e
            Right p -> loop (front . (p:)) xs
