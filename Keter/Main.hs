{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Keter.Main
    ( keter
    ) where

import Keter.Prelude hiding (getCurrentTime)
import qualified Keter.TempFolder as TempFolder
import qualified Keter.App as App
import qualified Keter.Postgres as Postgres
import qualified Keter.LogFile as LogFile
import qualified Keter.Logger as Logger
import qualified Keter.PortManager as PortMan
import qualified Keter.Proxy as Proxy

import Data.Conduit.Network (serverSettings, HostPreference)
import qualified Control.Concurrent.MVar as M
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import qualified System.INotify as I
import Control.Monad (forever, mzero)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Control.Exception (throwIO)
import qualified Prelude as P
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFile, FromJSON (parseJSON), Value (Object), (.:), (.:?), (.!=))
import Control.Applicative ((<$>), (<*>))
import Data.String (fromString)

data Config = Config
    { configDir :: F.FilePath
    , configPortMan :: PortMan.Settings
    , configHost :: HostPreference
    , configPort :: PortMan.Port
    , configSsl :: Maybe Proxy.TLSConfigNoDir
    }
instance Default Config where
    def = Config
        { configDir = "."
        , configPortMan = def
        , configHost = "*"
        , configPort = 80
        , configSsl = Nothing
        }

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> (F.fromText <$> o .: "root")
        <*> o .:? "port-manager" .!= def
        <*> (fmap fromString <$> o .:? "host") .!= configHost def
        <*> o .:? "port" .!= configPort def
        <*> o .:? "ssl"
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
            (runKIOPrint $ PortMan.hostList portman)
    case configSsl of
        Nothing -> return ()
        Just ssl -> do
            _ <- forkIO $ Proxy.reverseProxySsl
                    (Proxy.setDir dir ssl)
                    (runKIOPrint . PortMan.lookupPort portman)
                    (runKIOPrint $ PortMan.hostList portman)
            return ()

    mappMap <- M.newMVar Map.empty
    let removeApp appname = Keter.Prelude.modifyMVar_ mappMap $ return . Map.delete appname
        addApp bundle = do
            let appname = getAppname bundle
            rest <- modifyMVar mappMap $ \appMap ->
                case Map.lookup appname appMap of
                    Just app -> do
                        App.reload app
                        return (appMap, return ())
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
                            portman
                            postgres
                            logger
                            appname
                            bundle
                            (removeApp appname)
                        let appMap' = Map.insert appname app appMap
                        return (appMap', rest)
            rest
        terminateApp appname = do
            appMap <- M.readMVar mappMap
            case Map.lookup appname appMap of
                Nothing -> return ()
                Just app -> runKIO' $ App.terminate app

    let incoming = dir </> "incoming"
        isKeter fp = hasExtension fp "keter"
        isKeter' = isKeter . F.decodeString
    createTree incoming
    bundles <- fmap (filter isKeter) $ listDirectory incoming
    runKIO' $ mapM_ addApp bundles

    let events = [I.MoveIn, I.MoveOut, I.Delete, I.CloseWrite]
    i <- I.initINotify
    _ <- I.addWatch i events (toString incoming) $ \e -> do
        case e of
            I.Deleted _ fp -> when (isKeter' fp) $ terminateApp $ getAppname' fp
            I.MovedOut _ fp _ -> when (isKeter' fp) $ terminateApp $ getAppname' fp
            I.Closed _ (Just fp) _ -> when (isKeter' fp) $ runKIO' $ addApp $ incoming </> F.decodeString fp
            I.MovedIn _ fp _ -> when (isKeter' fp) $ runKIO' $ addApp $ incoming </> F.decodeString fp
            _ -> runKIO' $ log $ ReceivedInotifyEvent $ show e

    runKIO' $ forever $ threadDelay $ 60 * 1000 * 1000
  where
    getAppname = either id id . toText . basename
    getAppname' = getAppname . F.decodeString
    runThrow f = runKIO P.print f >>= either throwIO return
    input = F.decodeString input'
