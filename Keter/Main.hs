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
import Keter.App (AppStartConfig (..))
import Keter.Types
import qualified Keter.HostManager as HostMan
import qualified Keter.PortPool as PortPool
import qualified Keter.Proxy as Proxy
import qualified Network.HTTP.ReverseProxy.Rewrite as Rewrite
import System.Posix.Files (modificationTime, getFileStatus)
import System.Posix.Signals (sigHUP, installHandler, Handler (Catch))
import qualified Data.Conduit.LogFile as LogFile
import qualified Keter.AppManager as AppMan
import Data.Monoid (mempty)
import Control.Monad (unless)
import qualified Data.Vector as V
import Control.Concurrent.Async (withAsync, waitAny)

import Data.Yaml.FilePath
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
import Control.Applicative ((<$>))
import System.Posix.User (userID, userGroupID, getUserEntryForName, getUserEntryForID, userName)
import qualified Data.Text.Read
import qualified Data.Set as Set
import qualified Network.HTTP.Conduit as HTTP (newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Conduit.Process.Unix (initProcessTracker)

keter :: P.FilePath -- ^ root directory or config file
      -> [F.FilePath -> KIO (Either SomeException Plugin)]
      -> P.IO ()
keter (F.decodeString -> input) mkPlugins = do
    exists <- F.isFile input
    KeterConfig{..} <-
        if exists
            then decodeFileRelative input >>= either
                    (\e -> P.error $ "Invalid config file: " ++ P.show e)
                    return
            else return def { kconfigDir = input }

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

    mainlog <- LogFile.openRotatingLog
        (F.encodeString $ kconfigDir </> "log" </> "keter")
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
        runThrow f = runKIO P.print f >>= either throwIO return

    processTracker <- initProcessTracker
    hostman <- HostMan.start
    portpool <- PortPool.start kconfigPortPool
    tf <- TempFolder.setup $ kconfigDir </> "temp"
    plugins <- runThrow $ loadPlugins $ map ($ kconfigDir) mkPlugins
    manager <- HTTP.newManager def

    let appStartConfig = AppStartConfig
            { ascTempFolder = tf
            , ascSetuid = muid
            , ascProcessTracker = processTracker
            , ascHostManager = hostman
            , ascPortPool = portpool
            , ascPlugins = plugins
            }

    appMan <- AppMan.initialize (AppMan.RunKIO runKIO') appStartConfig
    let addApp bundle = do
            etime <- modificationTime <$> getFileStatus (F.encodeString bundle)
            AppMan.perform
                appMan
                (AppMan.AINamed $ getAppname bundle)
                (AppMan.Reload $ AppMan.AIBundle bundle etime)
        terminateApp appname = AppMan.perform appMan (AppMan.AINamed appname) AppMan.Terminate

    let incoming = kconfigDir </> "incoming"
        isKeter fp = hasExtension fp "keter"
    createTree incoming
    bundles0 <- fmap (filter isKeter) $ listDirectory incoming
    mapM_ addApp bundles0

    unless (V.null kconfigBuiltinStanzas) $ AppMan.perform
        appMan
        AppMan.AIBuiltin
        (AppMan.Reload $ AppMan.AIData $ BundleConfig kconfigBuiltinStanzas mempty)

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
            Right fp -> when (isKeter fp) $ addApp $ incoming </> fp

    -- Install HUP handler for cases when inotify cannot be used.
    _ <- flip (installHandler sigHUP) Nothing $ Catch $ do
        bundles <- fmap (filter isKeter) $ F.listDirectory incoming
        newMap <- fmap Map.fromList $ forM bundles $ \bundle -> do
            time <- modificationTime <$> getFileStatus (F.encodeString bundle)
            return (getAppname bundle, (bundle, time))
        AppMan.reloadAppList appMan newMap

    runAndBlock kconfigListeners $ Proxy.reverseProxy
        kconfigIpFromHeader
        manager
        (runKIO' . HostMan.lookupAction hostman)
  where
    getAppname = either id id . toText . basename

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
