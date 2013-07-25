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

    processTracker <- initProcessTracker
    hostman <- HostMan.start
    portpool <- PortPool.start kconfigPortPool
    tf <- runThrow $ liftIO $ TempFolder.setup $ kconfigDir </> "temp"
    plugins <- runThrow $ loadPlugins $ map ($ kconfigDir) mkPlugins
    mainlog <- runThrow $ liftIO $ LogFile.openRotatingLog
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
        runKIOPrint = runKIO P.print

    manager <- HTTP.newManager def
    V.forM_ kconfigListeners
        $ forkIO
        . Proxy.reverseProxy
            kconfigIpFromHeader
            manager
            (runKIOPrint . HostMan.lookupAction hostman)

    appMan <- AppMan.initialize
    let addApp bundle = AppMan.perform
            appMan
            (AppMan.AINamed $ getAppname bundle)
            (AppMan.Reload AppMan.AIBundle)
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
    {- FIXME
    _ <- flip (installHandler sigHUP) Nothing $ Catch $ do
        actions <- do
            bundles <- fmap (filter isKeter) $ F.listDirectory incoming
            newMap <- fmap Map.fromList $ forM bundles $ \bundle -> do
                time <- modificationTime <$> getFileStatus (F.encodeString bundle)
                return (getAppname' $ F.encodeString bundle, (bundle, time))

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
    -}

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
