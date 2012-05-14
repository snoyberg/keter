{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keter.App
    ( App
    , start
    , reload
    , Keter.App.terminate
    ) where

import Keter.TempFolder
import Keter.Postgres
import Keter.Process
import Keter.Nginx hiding (start)
import qualified Keter.Prelude
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import qualified Filesystem.Path.CurrentOS as F
import Data.Yaml
import Control.Applicative ((<$>), (<*>))
import System.PosixCompat.Files
import qualified Control.Concurrent.Chan as C
import Control.Concurrent (forkIO, threadDelay)
import System.Timeout (timeout)
import qualified Network
import Data.Maybe (fromMaybe)
import Control.Exception (try, SomeException, onException, throwIO)
import System.IO (hClose)
import System.Directory (removeDirectoryRecursive)
import Control.Monad (when)
import Data.Text (Text, pack)

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
newtype App = App (Command -> IO ())

tryM :: IO a -> IO (Maybe a)
tryM f = do
    res <- try f
    case res of
        Left (e :: SomeException) -> do
            putStrLn $ "Exception received: " ++ show e
            return Nothing
        Right x -> return $ Just x

unpackBundle :: TempFolder
             -> F.FilePath
             -> Appname
             -> IO (Maybe (FilePath, Config))
unpackBundle tf bundle appname = tryM $ do
    elbs <- runKIO $ Keter.Prelude.readFileLBS bundle
    lbs <- either throwIO return elbs
    edir <- runKIO $ getFolder tf appname
    dir <- either throwIO return edir
    putStrLn $ "Unpacking bundle to: " ++ show dir
    let rest = do
            Tar.unpack (F.encodeString dir) $ Tar.read $ decompress lbs
            let configFP = dir F.</> "config" F.</> "keter.yaml"
            Just config <- decodeFile $ F.encodeString configFP
            return (F.encodeString dir, config)
    rest `onException` Keter.Prelude.removeTree dir

start :: TempFolder
      -> Nginx
      -> Postgres
      -> Appname
      -> F.FilePath -- ^ app bundle
      -> IO () -- ^ action to perform to remove this App from list of actives
      -> IO (App, IO ())
start tf nginx postgres appname bundle removeFromList = do
    chan <- C.newChan
    return (App $ C.writeChan chan, rest chan)
  where
    void f = f >> return ()

    runApp port dir config = do
        setFileMode (F.encodeString $ F.decodeString dir F.</> "config" F.</> configExec config) ownerExecuteMode
        otherEnv <-
            if configPostgres config
                then do
                    dbi <- getInfo postgres appname
                    return
                        [ ("PGHOST", "localhost")
                        , ("PGPORT", "5432")
                        , ("PGUSER", dbiUser dbi)
                        , ("PGPASS", dbiPass dbi)
                        , ("PGDATABASE", dbiName dbi)
                        ]
                else return []
        runKIO $ run
            ("config" F.</> configExec config)
            (F.decodeString dir)
            (configArgs config)
            $ ("PORT", pack $ show port)
            : ("APPROOT", pack $ "http://" ++ configHost config)
            : otherEnv

    rest chan = void $ forkIO $ do
        mres <- unpackBundle tf bundle appname
        case mres of
            Nothing -> removeFromList
            Just (dir, config) -> do
                port <- getPort nginx
                process <- runApp port dir config
                b <- testApp port
                if b
                    then do
                        addEntry nginx (configHost config) $ AppEntry port
                        loop chan dir process port config
                    else do
                        removeFromList
                        releasePort nginx port
                        runKIO $ Keter.Process.terminate process

    loop chan dirOld processOld portOld configOld = do
        command <- C.readChan chan
        case command of
            Terminate -> do
                removeFromList
                removeEntry nginx $ configHost configOld
                putStrLn $ "Received terminate signal for app: " ++ show appname
                terminateOld
            Reload -> do
                mres <- unpackBundle tf bundle appname
                case mres of
                    Nothing -> do
                        runKIO $ Keter.Prelude.log $ Keter.Prelude.InvalidBundle bundle
                        loop chan dirOld processOld portOld configOld
                    Just (dir, config) -> do
                        port <- getPort nginx
                        process <- runApp port dir config
                        b <- testApp port
                        if b
                            then do
                                addEntry nginx (configHost config) $ AppEntry port
                                when (configHost config /= configHost configOld) $
                                    removeEntry nginx $ configHost configOld
                                putStrLn $ "Finished reloading: " ++ show appname
                                terminateOld
                                loop chan dir process port config
                            else do
                                releasePort nginx port
                                runKIO $ Keter.Process.terminate process
                                runKIO $ Keter.Prelude.log $ Keter.Prelude.ProcessDidNotStart bundle
                                loop chan dirOld processOld portOld configOld
      where
        terminateOld = void $ forkIO $ do
            threadDelay $ 20 * 1000 * 1000
            putStrLn $ "Terminating old process for: " ++ show appname
            runKIO $ Keter.Process.terminate processOld
            threadDelay $ 60 * 1000 * 1000
            putStrLn $ "Removing folder: " ++ dirOld
            removeDirectoryRecursive dirOld

runKIO :: Keter.Prelude.KIO a -> IO a -- FIXME remove this
runKIO = Keter.Prelude.runKIO print

testApp :: Port -> IO Bool
testApp port = do
    putStrLn $ "Testing app on port: " ++ show port
    res <- timeout (90 * 1000 * 1000) testApp'
    return $ fromMaybe False res
  where
    testApp' = do
        threadDelay $ 2 * 1000 * 1000
        eres <- try $ Network.connectTo "127.0.0.1" $ Network.PortNumber $ fromIntegral port
        case eres of
            Left (e :: SomeException) -> do
                putStrLn $ "Connection failed: " ++ show e
                testApp'
            Right handle -> do
                putStrLn $ "App is running on port: " ++ show port
                hClose handle
                return True

reload :: App -> IO ()
reload (App f) = f Reload

terminate :: App -> IO ()
terminate (App f) = f Terminate
