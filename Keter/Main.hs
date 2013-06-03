{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keter.Main
    ( keter
    ) where

import Keter.Prelude hiding (getCurrentTime)
import qualified Keter.TempFolder as TempFolder
import qualified Keter.App as App
import qualified Keter.ProcessTracker as ProcessTracker
import qualified Keter.Postgres as Postgres
import qualified Keter.LogFile as LogFile
import qualified Keter.Logger as Logger
import qualified Keter.PortManager as PortMan
import qualified Keter.Proxy as Proxy
import qualified Keter.ReverseProxy as ReverseProxy
import System.Posix.Files (modificationTime, getFileStatus)
import System.Posix.Signals (sigHUP, installHandler, Handler (Catch))

import Data.Conduit.Network (serverSettings, HostPreference)
import qualified Control.Concurrent.MVar as M
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import qualified System.FSNotify as FSN
import Control.Monad (forever, mzero, forM)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Control.Exception (throwIO, try)
import qualified Prelude as P
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Yaml (decodeFile, FromJSON (parseJSON), Value (Object), (.:), (.:?), (.!=))
import Control.Applicative ((<$>), (<*>))
import Data.String (fromString)
import System.Posix.User (userID, userGroupID, getUserEntryForName, getUserEntryForID, userName)
import qualified Data.Text.Read
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Network.HTTP.Conduit as HTTP (newManager)

data Config = Config
    { configDir :: F.FilePath
    , configPortMan :: PortMan.Settings
    , configHost :: HostPreference
    , configPort :: PortMan.Port
    , configSsl :: Maybe Proxy.TLSConfigNoDir
    , configSetuid :: Maybe Text
    , configReverseProxy :: Set ReverseProxy.ReverseProxyConfig
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
        }

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> (F.fromText <$> o .: "root")
        <*> o .:? "port-manager" .!= def
        <*> (fmap fromString <$> o .:? "host") .!= configHost def
        <*> o .:? "port" .!= configPort def
        <*> o .:? "ssl"
        <*> o .:? "setuid"
        <*> o .:? "reverse-proxy" .!= Set.empty
    parseJSON _ = mzero

keter :: P.FilePath -- ^ root directory or config file
      -> P.IO ()
keter input' = do
    exists <- F.isFile input
    Config{..} <-
        if exists
            then decodeFile input' >>= maybe (P.error "Invalid config file") return
            else return def { configDir = input }
    let dir = F.directory input F.</> configDir

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

    processTracker <- ProcessTracker.initProcessTracker
    portman <- runThrow $ PortMan.start configPortMan
    tf <- runThrow $ TempFolder.setup $ dir </> "temp"
    postgres <- runThrow $ Postgres.load def $ dir </> "etc" </> "postgres.yaml"
    mainlog <- runThrow $ LogFile.start $ dir </> "log" </> "keter"

    let runKIO' = runKIO $ \ml -> do
            now <- getCurrentTime
            let bs = encodeUtf8 $ T.concat
                    [ T.take 22 $ show now
                    , ": "
                    , show ml
                    , "\n"
                    ]
            runKIOPrint $ LogFile.addChunk mainlog bs
        runKIOPrint = runKIO P.print

    _ <- forkIO $ Proxy.reverseProxy
            (serverSettings configPort configHost)
            (runKIOPrint . PortMan.lookupPort portman)
    case configSsl of
        Nothing -> return ()
        Just ssl -> do
            _ <- forkIO $ Proxy.reverseProxySsl
                    (Proxy.setDir dir ssl)
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
                            let dirout = dir </> "log" </> fromText ("app-" ++ appname)
                                direrr = dirout </> "err"
                            elfout <- LogFile.start dirout
                            case elfout of
                                Left e -> do
                                    $logEx e
                                    return Nothing
                                Right lfout -> do
                                    elferr <- LogFile.start direrr
                                    case elferr of
                                        Left e -> do
                                            $logEx e
                                            LogFile.close lfout
                                            return Nothing
                                        Right lferr -> fmap Just $ Logger.start lfout lferr
                        let logger = fromMaybe Logger.dummy mlogger
                        (app, rest) <- App.start
                            tf
                            muid
                            processTracker
                            portman
                            postgres
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

    let incoming = dir </> "incoming"
        isKeter fp = hasExtension fp "keter"
        isKeter' = isKeter . F.decodeString
    createTree incoming
    bundles0 <- fmap (filter isKeter) $ listDirectory incoming
    runKIO' $ mapM_ addApp bundles0

    let staticReverse r = do
          initMgr <- liftIO $ HTTP.newManager def
          case initMgr of
            Left e -> log $ ExceptionThrown "Failed to instantiate manager for reverse proxy." e
            Right mgr -> PortMan.addEntry portman (ReverseProxy.reversingHost r) $ PortMan.PEReverseProxy $ ReverseProxy.RPEntry r mgr 
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
    input = F.decodeString input'
