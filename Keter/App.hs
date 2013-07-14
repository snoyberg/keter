{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Keter.App
    ( App
    , start
    , reload
    , Keter.App.terminate
    ) where

import Prelude (IO, Eq, Ord, fst, snd, concat, mapM)
import Keter.Prelude
import Codec.Archive.TempTarball
import Keter.Process
import Keter.Types
import Keter.PortManager hiding (start)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Data.Yaml
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import qualified Network
import Data.Maybe (fromMaybe)
import Control.Exception (throwIO)
import System.IO (hClose)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Encoding (encodeUtf8)
import System.Posix.Types (UserID, GroupID)
import Data.Conduit.Process.Unix (ProcessTracker, RotatingLog)
import Data.Yaml.FilePath
import Data.Aeson (withObject)

data AppConfig = AppConfig
    { configExec :: F.FilePath
    , configArgs :: [Text]
    , configHost :: Text
    , configSsl :: Bool
    , configExtraHosts :: Set String
    , configRaw :: Object
    }

instance ParseYamlFile AppConfig where
    parseYamlFile basedir = withObject "AppConfig" $ \o -> AppConfig
        <$> lookupBase basedir o "exec"
        <*> o .:? "args" .!= []
        <*> o .: "host"
        <*> o .:? "ssl" .!= False
        <*> o .:? "extra-hosts" .!= Set.empty
        <*> return o

data Config = Config
    { configApp :: Maybe AppConfig
    , configStaticHosts :: Set StaticHost
    , configRedirects :: Set Redirect
    }

instance ParseYamlFile Config where
    parseYamlFile basedir = withObject "Config" $ \o -> Config
        <$> ((Just <$> parseYamlFile basedir (Object o)) <|> pure Nothing)
        <*> lookupBaseMaybe basedir o "static-hosts" .!= Set.empty
        <*> o .:? "redirects" .!= Set.empty

data StaticHost = StaticHost
    { shHost :: String
    , shRoot :: FilePath
    }
    deriving (Eq, Ord)

instance ParseYamlFile StaticHost where
    parseYamlFile basedir = withObject "StaticHost" $ \o -> StaticHost
        <$> o .: "host"
        <*> lookupBase basedir o "root"

data Redirect = Redirect
    { redFrom :: Text
    , redTo :: Text
    }
    deriving (Eq, Ord)

instance FromJSON Redirect where
    parseJSON (Object o) = Redirect
        <$> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

data Command = Reload | Terminate
newtype App = App (Command -> KIO ())

unpackBundle :: TempFolder
             -> Maybe (UserID, GroupID)
             -> F.FilePath
             -> Appname
             -> KIO (Either SomeException (FilePath, Config))
unpackBundle tf muid bundle appname = do
    log $ UnpackingBundle bundle
    liftIO $ unpackTempTar muid tf bundle appname $ \dir -> do
        let configFP = dir F.</> "config" F.</> "keter.yaml"
        mconfig <- decodeFileRelative configFP
        config <-
            case mconfig of
                Right config -> return config
                Left e -> throwIO $ InvalidConfigFile e
        config' <-
            case configApp config of
                Nothing -> return config
                Just app -> do
                    abs <- F.canonicalizePath $ configExec app
                    return config
                        { configApp = Just app
                            { configExec = abs
                            }
                        }
        return (dir, config')

start :: TempFolder
      -> Maybe (Text, (UserID, GroupID))
      -> ProcessTracker
      -> PortManager
      -> Plugins
      -> RotatingLog
      -> Appname
      -> F.FilePath -- ^ app bundle
      -> KIO () -- ^ action to perform to remove this App from list of actives
      -> KIO (App, KIO ())
start tf muid processTracker portman plugins rlog appname bundle removeFromList = do
    chan <- newChan
    return (App $ writeChan chan, rest chan)
  where
    runApp port dir config = do
        otherEnv <- pluginsGetEnv plugins appname (configRaw config)
        let env = ("PORT", show port)
                : ("APPROOT", (if configSsl config then "https://" else "http://") ++ configHost config)
                : otherEnv
        run
            processTracker
            (fst <$> muid)
            (configExec config)
            dir
            (configArgs config)
            env
            rlog

    rest chan = forkKIO $ do
        mres <- unpackBundle tf (snd <$> muid) bundle appname
        case mres of
            Left e -> do
                $logEx e
                removeFromList
            Right (dir, config) -> do
                let common = do
                        mapM_ (\StaticHost{..} -> addEntry portman shHost (PEStatic shRoot)) $ Set.toList $ configStaticHosts config
                        mapM_ (\Redirect{..} -> addEntry portman redFrom (PERedirect $ encodeUtf8 redTo)) $ Set.toList $ configRedirects config
                case configApp config of
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
                                process <- runApp port dir appconfig
                                b <- testApp port
                                if b
                                    then do
                                        addEntry portman (configHost appconfig) $ PEPort port
                                        mapM_ (flip (addEntry portman) $ PEPort port) $ Set.toList $ configExtraHosts appconfig
                                        common
                                        loop chan dir config $ Just (process, port)
                                    else do
                                        removeFromList
                                        releasePort portman port
                                        Keter.Process.terminate process

    loop chan dirOld configOld mprocPortOld = do
        command <- readChan chan
        case command of
            Terminate -> do
                removeFromList
                case configApp configOld of
                    Nothing -> return ()
                    Just appconfig -> do
                        removeEntry portman $ configHost appconfig
                        mapM_ (removeEntry portman) $ Set.toList $ configExtraHosts appconfig
                mapM_ (removeEntry portman) $ map shHost $ Set.toList $ configStaticHosts configOld
                mapM_ (removeEntry portman) $ map redFrom $ Set.toList $ configRedirects configOld
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
                                        mapM_ (\StaticHost{..} -> addEntry portman shHost (PEStatic shRoot)) $ Set.toList $ configStaticHosts config
                                        mapM_ (\Redirect{..} -> addEntry portman redFrom (PERedirect $ encodeUtf8 redTo)) $ Set.toList $ configRedirects config
                                case configApp config of
                                    Nothing -> do
                                        common
                                        loop chan dir config Nothing
                                    Just appconfig -> do
                                        process <- runApp port dir appconfig
                                        b <- testApp port
                                        if b
                                            then do
                                                addEntry portman (configHost appconfig) $ PEPort port
                                                mapM_ (flip (addEntry portman) $ PEPort port) $ Set.toList $ configExtraHosts appconfig
                                                common
                                                case configApp configOld of
                                                    Just appconfigOld | configHost appconfig /= configHost appconfigOld ->
                                                        removeEntry portman $ configHost appconfigOld
                                                    _ -> return ()
                                                log $ FinishedReloading appname
                                                terminateOld
                                                loop chan dir config $ Just (process, port)
                                            else do
                                                releasePort portman port
                                                Keter.Process.terminate process
                                                log $ ProcessDidNotStart bundle
                                                loop chan dirOld configOld mprocPortOld
      where
        terminateOld = forkKIO $ do
            threadDelay $ 20 * 1000 * 1000
            log $ TerminatingOldProcess appname
            case mprocPortOld of
                Nothing -> return ()
                Just (processOld, _) -> Keter.Process.terminate processOld
            threadDelay $ 60 * 1000 * 1000
            log $ RemovingOldFolder dirOld
            res <- liftIO $ removeTree dirOld
            case res of
                Left e -> $logEx e
                Right () -> return ()

testApp :: Port -> KIO Bool
testApp port = do
    res <- timeout (90 * 1000 * 1000) testApp'
    return $ fromMaybe False res
  where
    testApp' = do
        threadDelay $ 2 * 1000 * 1000
        eres <- liftIO $ Network.connectTo "127.0.0.1" $ Network.PortNumber $ fromIntegral port
        case eres of
            Left _ -> testApp'
            Right handle -> do
                res <- liftIO $ hClose handle
                case res of
                    Left e -> $logEx e
                    Right () -> return ()
                return True

reload :: App -> KIO ()
reload (App f) = f Reload

terminate :: App -> KIO ()
terminate (App f) = f Terminate

pluginsGetEnv :: Plugins -> Appname -> Object -> KIO [(Text, Text)]
pluginsGetEnv ps app o = fmap concat $ mapM (\p -> pluginGetEnv p app o) ps
