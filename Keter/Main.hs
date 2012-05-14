{-# LANGUAGE OverloadedStrings #-}
module Keter.Main
    ( keter
    ) where

import qualified Keter.Nginx as Nginx
import qualified Keter.TempFolder as TempFolder
import qualified Keter.App as App
import qualified Keter.Postgres as Postgres
import qualified Keter.Prelude

import Data.Default (def)
import System.FilePath ((</>), takeBaseName)
import qualified Control.Concurrent.MVar as M
import qualified Data.Map as Map
import Data.Text (pack)
import System.Directory (getDirectoryContents)
import Control.Concurrent (threadDelay)
import qualified System.INotify as I
import Control.Monad (forever, when)
import Data.List (isSuffixOf)
import qualified Filesystem.Path.CurrentOS as F
import Control.Exception (throwIO)

keter :: FilePath -- ^ root directory, with incoming, temp, and etc folders
      -> IO ()
keter dir = do
    nginx <- Nginx.start def
    etf <- Keter.Prelude.runKIO print $ TempFolder.setup $ F.decodeString dir F.</> "temp"
    tf <- either throwIO return etf
    postgres <- Postgres.load def $ dir </> "etc" </> "postgres.yaml"

    mappMap <- M.newMVar Map.empty
    let removeApp appname = M.modifyMVar_ mappMap $ return . Map.delete appname
        addApp bundle = do
            let appname = getAppname bundle
            rest <- M.modifyMVar mappMap $ \appMap ->
                case Map.lookup appname appMap of
                    Just app -> do
                        App.reload app
                        return (appMap, return ())
                    Nothing -> do
                        (app, rest) <- App.start
                            tf
                            nginx
                            postgres
                            appname
                            (F.decodeString bundle)
                            (removeApp appname)
                        let appMap' = Map.insert appname app appMap
                        return (appMap', rest)
            rest
        terminateApp appname = do
            appMap <- M.readMVar mappMap
            case Map.lookup appname appMap of
                Nothing -> return ()
                Just app -> App.terminate app

    let incoming = dir </> "incoming"
    let hidden ('.':_) = True
        hidden _ = False
        isKeter = isSuffixOf ".keter"
    bundles <- fmap (map (incoming </>) . filter isKeter . filter (not . hidden))
             $ getDirectoryContents incoming
    mapM_ addApp bundles

    let events = [I.MoveIn, I.MoveOut, I.Delete, I.CloseWrite]
    i <- I.initINotify
    _ <- I.addWatch i events incoming $ \e ->
        case e of
            I.Deleted _ fp -> when (isKeter fp) $ terminateApp $ getAppname fp
            I.MovedOut _ fp _ -> when (isKeter fp) $ terminateApp $ getAppname fp
            I.Closed _ (Just fp) _ -> when (isKeter fp) $ addApp $ incoming </> fp
            I.MovedIn _ fp _ -> when (isKeter fp) $ addApp $ incoming </> fp
            _ -> print e

    forever $ threadDelay $ 60 * 1000 * 1000
  where
    getAppname = pack . takeBaseName
