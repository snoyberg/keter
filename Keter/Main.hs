{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Keter.Main
    ( keter
    ) where

import Keter.Prelude
import qualified Keter.Nginx as Nginx
import qualified Keter.TempFolder as TempFolder
import qualified Keter.App as App
import qualified Keter.Postgres as Postgres

import qualified Control.Concurrent.MVar as M
import qualified Data.Map as Map
import qualified System.INotify as I
import Control.Monad (forever)
import qualified Filesystem.Path.CurrentOS as F
import Control.Exception (throwIO)
import qualified Prelude as P

keter :: P.FilePath -- ^ root directory, with incoming, temp, and etc folders
      -> P.IO ()
keter dir' = do
    nginx <- runThrow $ Nginx.start def
    tf <- runThrow $ TempFolder.setup $ dir </> "temp"
    postgres <- runThrow $ Postgres.load def $ dir </> "etc" </> "postgres.yaml"

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
                        (app, rest) <- App.start
                            tf
                            nginx
                            postgres
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
    bundles <- fmap (filter isKeter) $ listDirectory incoming
    runKIO' $ mapM_ addApp bundles

    let events = [I.MoveIn, I.MoveOut, I.Delete, I.CloseWrite]
    i <- I.initINotify
    _ <- I.addWatch i events (toString incoming) $ \e ->
        case e of
            I.Deleted _ fp -> when (isKeter' fp) $ terminateApp $ getAppname' fp
            I.MovedOut _ fp _ -> when (isKeter' fp) $ terminateApp $ getAppname' fp
            I.Closed _ (Just fp) _ -> when (isKeter' fp) $ runKIO' $ addApp $ incoming </> F.decodeString fp
            I.MovedIn _ fp _ -> when (isKeter' fp) $ runKIO' $ addApp $ incoming </> F.decodeString fp
            _ -> P.print e -- FIXME

    runKIO' $ forever $ threadDelay $ 60 * 1000 * 1000
  where
    getAppname = either id id . toText . basename
    getAppname' = getAppname . F.decodeString
    runThrow f = runKIO' f >>= either throwIO return
    runKIO' = runKIO P.print
    dir = F.decodeString dir'
