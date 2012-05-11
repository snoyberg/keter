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
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import Codec.Compression.GZip (decompress)
import qualified System.FilePath as F
import Data.Text (pack)
import Data.Yaml
import Control.Applicative ((<$>), (<*>))
import System.PosixCompat.Files
import qualified Control.Concurrent.Chan as C
import Control.Concurrent (forkIO, threadDelay)
import System.Timeout (timeout)
import qualified Network
import Data.Maybe (fromMaybe)
import Control.Exception (try, SomeException, onException)
import System.IO (hClose)
import System.Directory (removeDirectoryRecursive)
import Control.Monad (when)

data Config = Config
    { configExec :: FilePath
    , configArgs :: [String]
    , configHost :: String
    }

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .: "exec"
        <*> o .:? "args" .!= []
        <*> o .: "host"
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
             -> FilePath
             -> Appname
             -> IO (Maybe (FilePath, Config))
unpackBundle tf bundle appname = tryM $ do
    lbs <- L.readFile bundle
    dir <- getFolder tf appname
    putStrLn $ "Unpacking bundle to: " ++ dir
    let rest = do
            Tar.unpack dir $ Tar.read $ decompress lbs
            let configFP = dir F.</> "config" F.</> "keter.yaml"
            Just config <- decodeFile configFP
            return (dir, config)
    rest `onException` removeDirectoryRecursive dir

start :: TempFolder
      -> Nginx
      -> FilePath -- ^ app bundle
      -> IO () -- ^ action to perform to remove this App from list of actives
      -> IO (App, IO ())
start tf nginx bundle removeFromList = do
    chan <- C.newChan
    return (App $ C.writeChan chan, rest chan)
  where
    appname = pack $ F.takeBaseName bundle
    void f = f >> return ()

    runApp port dir config = do
        setFileMode (dir F.</> "config" F.</> configExec config) ownerExecuteMode
        run
            ("config" F.</> configExec config)
            dir
            (configArgs config)
            [ ("PORT", show port)
            ]

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
                        Keter.Process.terminate process

    loop chan dirOld processOld portOld configOld = do
        command <- C.readChan chan
        case command of
            Terminate -> do
                removeFromList
                removeEntry nginx $ configHost configOld
                terminateOld
            Reload -> do
                mres <- unpackBundle tf bundle appname
                case mres of
                    Nothing -> do
                        putStrLn $ "Invalid bundle: " ++ bundle
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
                                terminateOld
                                loop chan dir process port config
                            else do
                                releasePort nginx port
                                Keter.Process.terminate process
                                putStrLn $ "Processing didn't start correctly: " ++ bundle
                                loop chan dirOld processOld portOld configOld
      where
        terminateOld = void $ forkIO $ do
            threadDelay $ 20 * 1000 * 1000
            putStrLn $ "Terminating old process for: " ++ show appname
            Keter.Process.terminate processOld
            threadDelay $ 60 * 1000 * 1000
            putStrLn $ "Removing folder: " ++ dirOld
            removeDirectoryRecursive dirOld

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
