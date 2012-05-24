{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Keter.Prelude
import System.Cmd (rawSystem)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (copyByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromShow)
import Data.Monoid (Monoid, mconcat)
import Data.ByteString.Char8 ()
import qualified Network
import qualified Data.ByteString as S
import System.Exit (ExitCode (ExitSuccess))

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = String

data Command = GetPort (Either SomeException Port -> KIO ())
             | ReleasePort Port
             | AddEntry Host Entry
             | RemoveEntry Host

-- | An individual virtual host may either be a reverse proxy to an app
-- (@AppEntry@), or may serve static files (@StaticEntry@).
data Entry = AppEntry Port
           | StaticEntry FilePath

-- | An abstract type which can accept commands and sends them to a background
-- nginx thread.
newtype Nginx = Nginx (Command -> KIO ())

-- | Controls execution of the nginx thread. Follows the settings type pattern.
-- See: <http://www.yesodweb.com/book/settings-types>.
data Settings = Settings
    { configFile :: FilePath
      -- ^ Location of config file. Default: \/etc\/nginx\/sites-enabled\/keter
    , reloadAction :: KIO (Either SomeException ())
      -- ^ How to tell Nginx to reload config file. Default: \/etc\/init.d\/nginx reload
    , startAction :: KIO (Either SomeException ())
      -- ^ How to tell Nginx to start running. Default: \/etc\/init.d\/nginx start
    , portRange :: [Port]
      -- ^ Which ports to assign to apps. Default: 4000-4999
    }

instance Default Settings where
    def = Settings
        { configFile = "/etc/nginx/sites-enabled/keter"
        , reloadAction = rawSystem' "/etc/init.d/nginx" ["reload"]
        , startAction = rawSystem' "/etc/init.d/nginx" ["start"]
        , portRange = [4000..4999]
        }

rawSystem' :: FilePath -> [String] -> KIO (Either SomeException ())
rawSystem' fp args = do
    eec <- liftIO $ rawSystem (toString fp) (map toString args)
    case eec of
        Left e -> return $ Left e
        Right ec
            | ec == ExitSuccess -> return $ Right ()
            | otherwise -> return $ Left $ toException $ ExitCodeFailure fp ec

-- | Start running a separate thread which will accept commands and modify
-- Nginx's behavior accordingly.
start :: Settings -> KIO (Either SomeException Nginx)
start Settings{..} = do
    -- Start off by ensuring we can read and write the config file and reload
    eres <- liftIO $ do
        exists <- isFile configFile
        config0 <-
            if exists
                then S.readFile $ toString configFile
                else return ""
        let tmp = configFile <.> "tmp"
        S.writeFile (toString tmp) config0
        rename tmp configFile
    case eres of
        Left e -> return $ Left e
        Right () -> do
            eres2 <- reloadAction
            case eres2 of
                Left e -> return $ Left e
                Right () -> go

 where
    go :: KIO (Either SomeException Nginx)
    go = do
        chan <- newChan
        forkKIO $ flip S.evalStateT (NState portRange [] Map.empty) $ forever $ do
            command <- lift $ readChan chan
            case command of
                GetPort f -> do
                    ns0 <- S.get
                    let loop :: NState -> KIO (Either SomeException Port, NState)
                        loop ns =
                            case nsAvail ns of
                                p:ps -> do
                                    res <- liftIO $ Network.listenOn $ Network.PortNumber $ fromIntegral p
                                    case res of
                                        Left (_ :: SomeException) -> do
                                            log $ RemovingPort p
                                            loop ns { nsAvail = ps }
                                        Right socket -> do
                                            res' <- liftIO $ Network.sClose socket
                                            case res' of
                                                Left e -> do
                                                    $logEx e
                                                    log $ RemovingPort p
                                                    loop ns { nsAvail = ps }
                                                Right () -> return (Right p, ns { nsAvail = ps })
                                [] ->
                                    case reverse $ nsRecycled ns of
                                        [] -> return (Left $ toException NoPortsAvailable, ns)
                                        ps -> loop ns { nsAvail = ps, nsRecycled = [] }
                    (eport, ns) <- lift $ loop ns0
                    S.put ns
                    lift $ f eport
                ReleasePort p ->
                    S.modify $ \ns -> ns { nsRecycled = p : nsRecycled ns }
                AddEntry h e -> change $ Map.insert h e
                RemoveEntry h -> change $ Map.delete h
        return $ Right $ Nginx $ writeChan chan

    change f = do
        ns <- S.get
        let entries = f $ nsEntries ns
        S.put $ ns { nsEntries = entries }
        let tmp = configFile <.> "tmp"
        lift $ do
            res1 <- liftIO $ do
                L.writeFile (toString tmp) $ mkConfig entries
                rename tmp configFile
            res2 <- case res1 of
                Left e -> return $ Left e
                Right () -> reloadAction
            case res2 of
                Left e -> $logEx e
                Right () -> return ()
    mkConfig = toLazyByteString . mconcat . map mkConfig' . Map.toList
    mkConfig' (host, entry) =
        copyByteString "server {\n    listen 80;\n    server_name " ++
        fromText host ++ copyByteString ";\n" ++
        mkConfigEntry entry ++
        copyByteString "}\n"
    mkConfigEntry (AppEntry port) =
        copyByteString "    location / {\n        proxy_pass http://127.0.0.1:" ++
        fromShow port ++ copyByteString ";\n        proxy_set_header X-Real-IP $remote_addr;\n    }\n"
    mkConfigEntry (StaticEntry fp) =
        copyByteString "    root " ++ fromString (toString fp) ++ copyByteString ";\n    expires max;\n"

data NState = NState
    { nsAvail :: [Port]
    , nsRecycled :: [Port]
    , nsEntries :: Map.Map Host Entry
    }

-- | Gets an unassigned port number.
getPort :: Nginx -> KIO (Either SomeException Port)
getPort (Nginx f) = do
    x <- newEmptyMVar
    f $ GetPort $ \p -> putMVar x p
    takeMVar x

-- | Inform the nginx thread that the given port number is no longer being
-- used, and may be reused by a new process. Note that recycling puts the new
-- ports at the end of the queue (FIFO), so that if an application holds onto
-- the port longer than expected, there should be no issues.
releasePort :: Nginx -> Port -> KIO ()
releasePort (Nginx f) p = f $ ReleasePort p

-- | Add a new entry to the configuration for the given hostname and reload
-- nginx. Will overwrite any existing configuration for the given host. The
-- second point is important: it is how we achieve zero downtime transitions
-- between an old and new version of an app.
addEntry :: Nginx -> Host -> Entry -> KIO ()
addEntry (Nginx f) h e = f $ AddEntry h e

-- | Remove an entry from the configuration and reload nginx.
removeEntry :: Nginx -> Host -> KIO ()
removeEntry (Nginx f) h = f $ RemoveEntry h
