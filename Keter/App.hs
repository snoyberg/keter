{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Keter.App
    ( App
    , start
    , reload
    , Keter.App.terminate
    ) where

import Keter.Prelude
import Keter.TempFolder
import Keter.Postgres
import Keter.Process
import Keter.Nginx hiding (start)
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import qualified Filesystem.Path.CurrentOS as F
import Data.Yaml
import Control.Applicative ((<$>), (<*>))
import System.PosixCompat.Files
import qualified Network
import Data.Maybe (fromMaybe)
import Control.Exception (onException)
import System.IO (hClose)

data Config = Config
    { configExec :: F.FilePath
    , configArgs :: [Text]
    , configHost :: String
    , configPostgres :: Bool
    }

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> (F.fromText <$> o .: "exec")
        <*> o .:? "args" .!= []
        <*> o .: "host"
        <*> o .:? "postgres" .!= False
    parseJSON _ = fail "Wanted an object"

data Command = Reload | Terminate
newtype App = App (Command -> KIO ())

unpackBundle :: TempFolder
             -> F.FilePath
             -> Appname
             -> KIO (Either SomeException (FilePath, Config))
unpackBundle tf bundle appname = do
    elbs <- readFileLBS bundle
    case elbs of
        Left e -> return $ Left e
        Right lbs -> do
            edir <- getFolder tf appname
            case edir of
                Left e -> return $ Left e
                Right dir -> do
                    log $ UnpackingBundle bundle dir
                    let rest = do
                            Tar.unpack (F.encodeString dir) $ Tar.read $ decompress lbs
                            let configFP = dir F.</> "config" F.</> "keter.yaml"
                            Just config <- decodeFile $ F.encodeString configFP
                            return (dir, config)
                    liftIO $ rest `onException` removeTree dir

start :: TempFolder
      -> Nginx
      -> Postgres
      -> Appname
      -> F.FilePath -- ^ app bundle
      -> KIO () -- ^ action to perform to remove this App from list of actives
      -> KIO (App, KIO ())
start tf nginx postgres appname bundle removeFromList = do
    chan <- newChan
    return (App $ writeChan chan, rest chan)
  where
    runApp port dir config = do
        res1 <- liftIO $ setFileMode (toString $ dir </> "config" </> configExec config) ownerExecuteMode
        case res1 of
            Left e -> log $ ExceptionThrown e
            Right () -> return ()
        otherEnv <- do
            mdbi <-
                if configPostgres config
                    then do
                        edbi <- getInfo postgres appname
                        case edbi of
                            Left e -> do
                                log $ ExceptionThrown e
                                return Nothing
                            Right dbi -> return $ Just dbi
                    else return Nothing
            return $ case mdbi of
                Just dbi ->
                    [ ("PGHOST", "localhost")
                    , ("PGPORT", "5432")
                    , ("PGUSER", dbiUser dbi)
                    , ("PGPASS", dbiPass dbi)
                    , ("PGDATABASE", dbiName dbi)
                    ]
                Nothing -> []
        run
            ("config" </> configExec config)
            dir
            (configArgs config)
            $ ("PORT", show port)
            : ("APPROOT", "http://" ++ configHost config)
            : otherEnv

    rest chan = forkKIO $ do
        mres <- unpackBundle tf bundle appname
        case mres of
            Left e -> do
                log $ ExceptionThrown e
                removeFromList
            Right (dir, config) -> do
                eport <- getPort nginx
                case eport of
                    Left e -> do
                        log $ ExceptionThrown e
                        removeFromList
                    Right port -> do
                        process <- runApp port dir config
                        b <- testApp port
                        if b
                            then do
                                addEntry nginx (configHost config) $ AppEntry port
                                loop chan dir process port config
                            else do
                                removeFromList
                                releasePort nginx port
                                Keter.Process.terminate process

    loop chan dirOld processOld portOld configOld = do
        command <- readChan chan
        case command of
            Terminate -> do
                removeFromList
                removeEntry nginx $ configHost configOld
                log $ TerminatingApp appname
                terminateOld
            Reload -> do
                mres <- unpackBundle tf bundle appname
                case mres of
                    Left e -> do
                        log $ InvalidBundle bundle e
                        loop chan dirOld processOld portOld configOld
                    Right (dir, config) -> do
                        eport <- getPort nginx
                        case eport of
                            Left e -> log $ ExceptionThrown e
                            Right port -> do
                                process <- runApp port dir config
                                b <- testApp port
                                if b
                                    then do
                                        addEntry nginx (configHost config) $ AppEntry port
                                        when (configHost config /= configHost configOld) $
                                            removeEntry nginx $ configHost configOld
                                        log $ FinishedReloading appname
                                        terminateOld
                                        loop chan dir process port config
                                    else do
                                        releasePort nginx port
                                        Keter.Process.terminate process
                                        log $ ProcessDidNotStart bundle
                                        loop chan dirOld processOld portOld configOld
      where
        terminateOld = forkKIO $ do
            threadDelay $ 20 * 1000 * 1000
            log $ TerminatingOldProcess appname
            Keter.Process.terminate processOld
            threadDelay $ 60 * 1000 * 1000
            log $ RemovingOldFolder dirOld
            res <- liftIO $ removeTree dirOld
            case res of
                Left e -> log $ ExceptionThrown e
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
                    Left e -> log $ ExceptionThrown e
                    Right () -> return ()
                return True

reload :: App -> KIO ()
reload (App f) = f Reload

terminate :: App -> KIO ()
terminate (App f) = f Terminate
