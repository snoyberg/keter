{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Keter.Types.Common
    ( module Keter.Types.Common
    , FilePath
    , Text
    , ByteString
    , Set
    , Map
    , Exception
    , SomeException
    ) where

import           Control.Exception          (Exception, SomeException)
import           Data.Aeson                 (Object)
import           Data.ByteString            (ByteString)
import           Data.Map                   (Map)
import           Data.Set                   (Set)
import           Data.Text                  (Text, pack, unpack)
import           Data.Typeable              (Typeable)
import qualified Data.Yaml
import           Filesystem.Path.CurrentOS  (FilePath, basename, encodeString,
                                             toText)
import qualified Language.Haskell.TH.Syntax as TH
import           Prelude                    hiding (FilePath)
import           System.Exit                (ExitCode)

-- | Name of the application. Should just be the basename of the application
-- file.
type Appname = Text

data Plugin = Plugin
    { pluginGetEnv :: Appname -> Object -> IO [(Text, Text)]
    }

type Plugins = [Plugin]

-- | Used for versioning data types.
class ToCurrent a where
    type Previous a
    toCurrent :: Previous a -> a
instance ToCurrent a => ToCurrent (Maybe a) where
    type Previous (Maybe a) = Maybe (Previous a)
    toCurrent = fmap toCurrent

-- | A port for an individual app to listen on.
type Port = Int

-- | A virtual host we want to serve content from.
type Host = Text

type HostBS = ByteString

getAppname :: FilePath -> Text
getAppname = either id id . toText . basename

data LogMessage
    = ProcessCreated FilePath
    | InvalidBundle FilePath SomeException
    | ProcessDidNotStart FilePath
    | ExceptionThrown Text SomeException
    | RemovingPort Int
    | UnpackingBundle FilePath
    | TerminatingApp Text
    | FinishedReloading Text
    | TerminatingOldProcess AppId
    | RemovingOldFolder FilePath
    | ReceivedInotifyEvent Text
    | ProcessWaiting FilePath
    | OtherMessage Text
    | ErrorStartingBundle Text SomeException
    | SanityChecksPassed

instance Show LogMessage where
    show (ProcessCreated f) = "Created process: " ++ encodeString f
    show (InvalidBundle f e) = concat
        [ "Unable to parse bundle file '"
        , encodeString f
        , "': "
        , show e
        ]
    show (ProcessDidNotStart fp) = concat
        [ "Could not start process within timeout period: "
        , encodeString fp
        ]
    show (ExceptionThrown t e) = concat
        [ unpack t
        , ": "
        , show e
        ]
    show (RemovingPort p) = "Port in use, removing from port pool: " ++ show p
    show (UnpackingBundle b) = concat
        [ "Unpacking bundle '"
        , encodeString b
        , "'"
        ]
    show (TerminatingApp t) = "Shutting down app: " ++ unpack t
    show (FinishedReloading t) = "App finished reloading: " ++ unpack t
    show (TerminatingOldProcess (AINamed t)) = "Sending old process TERM signal: " ++ unpack t
    show (TerminatingOldProcess AIBuiltin) = "Sending old process TERM signal: builtin"
    show (RemovingOldFolder fp) = "Removing unneeded folder: " ++ encodeString fp
    show (ReceivedInotifyEvent t) = "Received unknown INotify event: " ++ unpack t
    show (ProcessWaiting f) = "Process restarting too quickly, waiting before trying again: " ++ encodeString f
    show (OtherMessage t) = unpack t
    show (ErrorStartingBundle name e) = concat
        [ "Error occured when launching bundle "
        , unpack name
        , ": "
        , show e
        ]
    show SanityChecksPassed = "Sanity checks passed"

data KeterException = CannotParsePostgres FilePath
                    | ExitCodeFailure FilePath ExitCode
                    | NoPortsAvailable
                    | InvalidConfigFile Data.Yaml.ParseException
                    | InvalidKeterConfigFile !FilePath !Data.Yaml.ParseException
                    | CannotReserveHosts !AppId !(Map Host AppId)
                    | FileNotExecutable !FilePath
                    | ExecutableNotFound !FilePath
    deriving (Show, Typeable)
instance Exception KeterException

logEx :: TH.Q TH.Exp
logEx = do
    let showLoc TH.Loc { TH.loc_module = m, TH.loc_start = (l, c) } = concat
            [ m
            , ":"
            , show l
            , ":"
            , show c
            ]
    loc <- fmap showLoc TH.qLocation
    [|(. ExceptionThrown (pack $(TH.lift loc)))|]

data AppId = AIBuiltin | AINamed !Appname
    deriving (Eq, Ord, Show)
