{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Keter.Postgres
    ( -- * Types
      Appname
    , DBInfo (..)
    , Postgres
      -- ** Settings
    , Settings
    , setupDBInfo
      -- * Functions
    , load
    , getInfo
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as M
import Control.Monad (forever, mzero, replicateM)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<$>), (<*>))
import qualified System.Random as R
import Data.Monoid (Monoid, mappend)
import System.Directory (renameFile)
import Data.Text.Lazy.Builder (toLazyText, fromText)
import qualified Data.Text.Lazy as TL
import System.Process (readProcess)
import Data.Default (Default (def))

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

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

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

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

-- | Abstract type allowing access to config information via 'getInfo'
newtype Postgres = Postgres
    { getInfo :: Appname -> IO DBInfo
    -- ^ Get information on an individual app\'s database information. If no
    -- information exists, it will create a random database, add it to the
    -- config file, and return it.
    }

data Command = GetConfig Appname (DBInfo -> IO ())

-- | Load a set of existing connections from a config file. If the file does
-- not exist, it will be created. Any newly created databases will
-- automatically be saved to this file.
load :: Settings -> FilePath -> IO Postgres
load Settings{..} fp = do
    e <- doesFileExist fp
    mdb <-
        if e
            then decodeFile fp
            else return $ Just Map.empty
    db0 <-
        case mdb of
            Nothing -> error $ "Unable to parse Postgres file: " ++ show fp
            Just db -> return db
    chan <- C.newChan
    g0 <- R.newStdGen
    _ <- forkIO $ flip S.evalStateT (db0, g0) $ forever $ do
        GetConfig appname f <- lift $ C.readChan chan
        (db, g) <- S.get
        dbi <-
            case Map.lookup appname db of
                Just dbi -> return dbi
                Nothing -> do
                    let (dbi', g') = randomDBI g
                    let dbi = dbi'
                            { dbiName = T.append appname $ dbiName dbi'
                            , dbiUser = T.append appname $ dbiUser dbi'
                            }
                    lift $ setupDBInfo dbi
                    let db' = Map.insert appname dbi db
                    lift $ encodeFile tmpfp db'
                    lift $ renameFile tmpfp fp
                    S.put (db', g')
                    return dbi
        lift $ f dbi
    return $ Postgres $ \appname -> do
        x <- M.newEmptyMVar
        C.writeChan chan $ GetConfig appname $ M.putMVar x
        M.takeMVar x
  where
    tmpfp = fp ++ ".tmp"
