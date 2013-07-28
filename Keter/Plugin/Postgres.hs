{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Keter.Plugin.Postgres
    ( -- * Settings
      Settings
    , setupDBInfo
      -- * Functions
    , load
    ) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception         (throwIO, try)
import           Control.Monad             (void)
import           Control.Monad             (forever, mzero, replicateM)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import           Data.Default
import qualified Data.HashMap.Strict       as HMap
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (fromText, toLazyText)
import           Data.Yaml
import           Filesystem                (createTree, isFile, rename)
import           Filesystem.Path.CurrentOS (directory, encodeString, (<.>))
import           Keter.Types
import           Prelude                   hiding (FilePath)
import           System.Process            (readProcess)
import qualified System.Random             as R

data Settings = Settings
    { setupDBInfo :: DBInfo -> IO ()
      -- ^ How to create the given user/database. Default: uses the @psql@
      -- command line tool and @sudo -u postgres@.
    }

instance Default Settings where
    def = Settings
        { setupDBInfo = \DBInfo{..} -> do
            let sql = toLazyText $
                    "CREATE USER "         <> fromText dbiUser <>
                    " PASSWORD '"          <> fromText dbiPass <>
                    "';\nCREATE DATABASE " <> fromText dbiName <>
                    " OWNER "              <> fromText dbiUser <>
                    ";"
            _ <- readProcess "sudo" ["-u", "postgres", "psql"] $ TL.unpack sql
            return ()
        }

-- | Information on an individual PostgreSQL database.
data DBInfo = DBInfo
    { dbiName :: Text
    , dbiUser :: Text
    , dbiPass :: Text
    }
    deriving Show

randomDBI :: R.StdGen -> (DBInfo, R.StdGen)
randomDBI =
    S.runState (DBInfo <$> rt <*> rt <*> rt)
  where
    rt = T.pack <$> replicateM 10 (S.state $ R.randomR ('a', 'z'))

instance ToJSON DBInfo where
    toJSON (DBInfo n u p) = object
        [ "name" .= n
        , "user" .= u
        , "pass" .= p
        ]

instance FromJSON DBInfo where
    parseJSON (Object o) = DBInfo
        <$> o .: "name"
        <*> o .: "user"
        <*> o .: "pass"
    parseJSON _ = mzero

data Command = GetConfig Appname (Either SomeException DBInfo -> IO ())

-- | Load a set of existing connections from a config file. If the file does
-- not exist, it will be created. Any newly created databases will
-- automatically be saved to this file.
load :: Settings -> FilePath -> IO Plugin
load Settings{..} fp = do
    createTree $ directory fp
    e <- isFile fp
    edb <- if e
        then decodeFileEither $ encodeString fp
        else return $ Right Map.empty
    case edb of
        Left ex -> throwIO ex
        Right db -> go db
  where
    go db0 = do
        chan <- newChan
        g0 <- R.newStdGen
        -- FIXME stop using the worker thread approach?
        void $ forkIO $ flip S.evalStateT (db0, g0) $ forever $ loop chan
        return Plugin
            { pluginGetEnv = \appname o -> do
                case HMap.lookup "postgres" o of
                    Just (Bool True) -> do
                        x <- newEmptyMVar
                        writeChan chan $ GetConfig appname $ putMVar x
                        edbi <- takeMVar x
                        edbiToEnv edbi
                    _ -> return []
            }

    tmpfp = fp <.> "tmp"

    loop chan = do
        GetConfig appname f <- lift $ readChan chan
        (db, g) <- S.get
        dbi <-
            case Map.lookup appname db of
                Just dbi -> return $ Right dbi
                Nothing -> do
                    let (dbi', g') = randomDBI g
                    let dbi = dbi'
                            { dbiName = sanitize appname <> dbiName dbi'
                            , dbiUser = sanitize appname <> dbiUser dbi'
                            }
                    ex <- lift $ try $ setupDBInfo dbi
                    case ex of
                        Left e -> return $ Left e
                        Right () -> do
                            let db' = Map.insert appname dbi db
                            ey <- lift $ try $ do
                                encodeFile (encodeString tmpfp) db'
                                rename tmpfp fp
                            case ey of
                                Left e -> return $ Left e
                                Right () -> do
                                    S.put (db', g')
                                    return $ Right dbi
        lift $ f dbi

    sanitize = T.map sanitize'
    sanitize' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'

edbiToEnv :: Either SomeException DBInfo
          -> IO [(Text, Text)]
edbiToEnv (Left e) = throwIO e
edbiToEnv (Right dbi) = return
    [ ("PGHOST", "localhost")
    , ("PGPORT", "5432")
    , ("PGUSER", dbiUser dbi)
    , ("PGPASS", dbiPass dbi)
    , ("PGDATABASE", dbiName dbi)
    ]
