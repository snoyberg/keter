{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keter.Nginx
    ( -- * Types
      Port
    , Host
    , Entry (..)
    , Nginx
      -- ** Settings
    , Settings
    , configFile
    , reloadAction
    , startAction
    , portRange
      -- * Actions
    , getPort
    , releasePort
    , addEntry
    , removeEntry
      -- * Initialize
    , start
    ) where

import Data.Default (Default (def))
import System.Cmd (rawSystem)
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Chan as C
import Control.Concurrent (forkIO)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (copyByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromShow)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.ByteString.Char8 ()
import System.Directory (renameFile)
import qualified Network
import Control.Exception (SomeException, try)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = String

data Command = GetPort (Port -> IO ())
             | ReleasePort Port
             | AddEntry Host Entry
             | RemoveEntry Host

-- | An individual virtual host may either be a reverse proxy to an app
-- (@AppEntry@), or may serve static files (@StaticEntry@).
data Entry = AppEntry Port
           | StaticEntry FilePath

-- | An abstract type which can accept commands and sends them to a background
-- nginx thread.
newtype Nginx = Nginx (Command -> IO ())

-- | Controls execution of the nginx thread. Follows the settings type pattern.
-- See: <http://www.yesodweb.com/book/settings-types>.
data Settings = Settings
    { configFile :: FilePath
      -- ^ Location of config file. Default: \/etc\/nginx\/sites-enabled\/keter
    , reloadAction :: IO ()
      -- ^ How to tell Nginx to reload config file. Default: \/etc\/init.d\/nginx reload
    , startAction :: IO ()
      -- ^ How to tell Nginx to start running. Default: \/etc\/init.d\/nginx start
    , portRange :: [Port]
      -- ^ Which ports to assign to apps. Default: 4000-4999
    }

instance Default Settings where
    def = Settings
        { configFile = "/etc/nginx/sites-enabled/keter"
        , reloadAction = rawSystem "/etc/init.d/nginx" ["reload"] >> return ()
        , startAction = rawSystem "/etc/init.d/nginx" ["start"] >> return ()
        , portRange = [4000..4999]
        }

-- | Start running a separate thread which will accept commands and modify
-- Nginx's behavior accordingly.
start :: Settings -> IO Nginx
start Settings{..} = do
    chan <- C.newChan
    _ <- forkIO $ flip S.evalStateT (NState portRange [] Map.empty) $ forever $ do
        command <- lift $ C.readChan chan
        case command of
            GetPort f -> do
                ns0 <- S.get
                let loop ns =
                        case nsAvail ns of
                            p:ps -> do
                                res <- try $ Network.listenOn $ Network.PortNumber $ fromIntegral p
                                case res of
                                    Left (_ :: SomeException) -> do
                                        putStrLn $ "Removing port from use: " ++ show p
                                        loop ns { nsAvail = ps }
                                    Right socket -> do
                                        Network.sClose socket
                                        return (p, ns { nsAvail = ps })
                            [] ->
                                case reverse $ nsRecycled ns of
                                    [] -> return (error "No ports available", ns)
                                    ps -> loop ns { nsAvail = ps, nsRecycled = [] }
                (port, ns) <- lift $ loop ns0
                S.put ns
                lift $ f port
            ReleasePort p ->
                S.modify $ \ns -> ns { nsRecycled = p : nsRecycled ns }
            AddEntry h e -> change $ Map.insert h e
            RemoveEntry h -> change $ Map.delete h
    return $ Nginx $ C.writeChan chan
  where
    change f = do
        ns <- S.get
        let entries = f $ nsEntries ns
        S.put $ ns { nsEntries = entries }
        let tmp = configFile ++ ".tmp"
        lift $ L.writeFile tmp $ mkConfig entries
        lift $ renameFile tmp configFile
        lift reloadAction
    mkConfig = toLazyByteString . mconcat . map mkConfig' . Map.toList
    mkConfig' (host, entry) =
        copyByteString "server {\n    listen 80;\n    server_name " <>
        fromString host <> copyByteString ";\n" <>
        mkConfigEntry entry <>
        copyByteString "}\n"
    mkConfigEntry (AppEntry port) =
        copyByteString "    location / {\n        proxy_pass http://127.0.0.1:" <>
        fromShow port <> copyByteString ";\n    }\n"
    mkConfigEntry (StaticEntry fp) =
        copyByteString "    root " <> fromString fp <> copyByteString ";\n    expires max;\n"

data NState = NState
    { nsAvail :: [Port]
    , nsRecycled :: [Port]
    , nsEntries :: Map.Map Host Entry
    }

-- | Gets an unassigned port number.
getPort :: Nginx -> IO Port
getPort (Nginx f) = do
    x <- M.newEmptyMVar
    f $ GetPort $ \p -> M.putMVar x p
    M.takeMVar x

-- | Inform the nginx thread that the given port number is no longer being
-- used, and may be reused by a new process. Note that recycling puts the new
-- ports at the end of the queue (FIFO), so that if an application holds onto
-- the port longer than expected, there should be no issues.
releasePort :: Nginx -> Port -> IO ()
releasePort (Nginx f) p = f $ ReleasePort p

-- | Add a new entry to the configuration for the given hostname and reload
-- nginx. Will overwrite any existing configuration for the given host. The
-- second point is important: it is how we achieve zero downtime transitions
-- between an old and new version of an app.
addEntry :: Nginx -> Host -> Entry -> IO ()
addEntry (Nginx f) h e = f $ AddEntry h e

-- | Remove an entry from the configuration and reload nginx.
removeEntry :: Nginx -> Host -> IO ()
removeEntry (Nginx f) h = f $ RemoveEntry h
