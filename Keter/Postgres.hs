{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import Keter.Prelude
import qualified Prelude as P
import qualified Data.Text as T
import Data.Yaml
import qualified Data.Map as Map
import Control.Monad (forever, mzero, replicateM)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<$>), (<*>))
import qualified System.Random as R
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy as TL
import System.Process (readProcess)

data Settings = Settings
    { setupDBInfo :: DBInfo -> P.IO ()
      -- ^ How to create the given user/database. Default: uses the @psql@
      -- command line tool and @sudo -u postgres@.
    }

instance Default Settings where
    def = Settings
        { setupDBInfo = \DBInfo{..} -> do
            let sql = toLazyText $
                    "CREATE USER "         ++ fromText dbiUser ++
                    " PASSWORD '"          ++ fromText dbiPass ++
                    "';\nCREATE DATABASE " ++ fromText dbiName ++
                    " OWNER "              ++ fromText dbiUser ++
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
    { getInfo :: Appname -> KIO (Either SomeException DBInfo)
    -- ^ Get information on an individual app\'s database information. If no
    -- information exists, it will create a random database, add it to the
    -- config file, and return it.
    }

data Command = GetConfig Appname (Either SomeException DBInfo -> KIO ())

-- | Load a set of existing connections from a config file. If the file does
-- not exist, it will be created. Any newly created databases will
-- automatically be saved to this file.
load :: Settings -> FilePath -> KIO (Either SomeException Postgres)
load Settings{..} fp = do
    mdb <- liftIO $ do
        createTree $ directory fp
        e <- isFile fp
        if e
            then decodeFile $ toString fp
            else return $ Just Map.empty
    case mdb of
        Left e -> return $ Left e
        Right Nothing -> return $ Left $ toException $ CannotParsePostgres fp
        Right (Just db0) -> go (db0 :: Map.Map Appname DBInfo)
  where
    go db0 = do
        chan <- newChan
        g0 <- newStdGen
        forkKIO $ flip S.evalStateT (db0, g0) $ forever $ loop chan
        return $ Right $ Postgres $ \appname -> do
            x <- newEmptyMVar
            writeChan chan $ GetConfig appname $ putMVar x
            takeMVar x

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
                            { dbiName = sanitize appname ++ dbiName dbi'
                            , dbiUser = sanitize appname ++ dbiUser dbi'
                            }
                    ex <- lift $ liftIO $ setupDBInfo dbi
                    case ex of
                        Left e -> return $ Left e
                        Right () -> do
                            let db' = Map.insert appname dbi db
                            ey <- lift $ liftIO $ do
                                encodeFile (toString tmpfp) db'
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
