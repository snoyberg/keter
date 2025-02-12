{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.Cli
    ( launchCli
    , CliStates(..)
    ) where

import Control.Concurrent (forkFinally)
import Control.Exception qualified as E
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.ByteString qualified as S
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Conc
import Keter.AppManager
import Keter.Common
import Keter.Context
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Options.Applicative

data Commands = CmdListRunningApps
              | CmdExit

data CliStates = MkCliStates
  { csAppManager :: !AppManager
  , csPort       :: !Port
  }

launchCli :: KeterM CliStates ()
launchCli = do
  MkCliStates{..} <- ask
  void $ withRunInIO $ \rio -> forkIO $
    withSocketsDo $ do
        addr <- resolve $ show csPort
        E.bracket (open addr) close $ \x -> rio $ do
            $logInfo $ T.pack $ "Bound cli to " <> show addr
            loop x

commandParser :: Parser Commands
commandParser = hsubparser $
  fold [
  command "exit"
    (info (pure CmdExit)
      (progDesc "List all ports"))
  ,
  command "apps"
      (info (pure CmdListRunningApps)
        (progDesc "Exit the program"))
  ]

resolve :: ServiceName -> IO AddrInfo
resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    -- If the prefork technique is not used,
    -- set CloseOnExec for the security reasons.
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress addr)
    listen sock 10
    return sock

loop :: Socket -> KeterM CliStates b
loop sock = forever $ do
    (conn, peer) <- liftIO $ accept sock
    $logInfo $ T.pack $ "CLI Connection from " <> show peer
    void $ withRunInIO $ \rio ->
        forkFinally (rio $ talk conn) (\_ -> close conn)

listRunningApps :: Socket -> KeterM CliStates ()
listRunningApps conn = do
  MkCliStates{..} <- ask
  txt <- liftIO $ atomically $ renderApps csAppManager
  liftIO $ sendAll conn $ T.encodeUtf8 txt <> "\n"

talk :: Socket -> KeterM CliStates ()
talk conn = do
    msg <- liftIO $ recv conn 1024
    unless (S.null msg) $ do
      case T.decodeUtf8' msg of
        Left exception -> liftIO $ sendAll conn ("decode error: " <> T.encodeUtf8 (T.pack $ show exception))
        Right txt -> do
          let res = execParserPure defaultPrefs (info (commandParser <**> helper)
                                                (fullDesc <> header "server repl" <> progDesc
                        "repl for inspecting program state. You can connect to a socket and ask predefined questions") ) (T.unpack <$> T.words txt)
          isLoop <- case res of
            (Success CmdListRunningApps) -> True <$ listRunningApps conn
            (Success CmdExit) -> False <$ liftIO (sendAll conn "bye\n")
            (CompletionInvoked x) -> True <$ liftIO (sendAll conn "completion ignored \n")
            Failure failure        ->
              True <$ liftIO (sendAll conn (T.encodeUtf8 (T.pack $ fst $ renderFailure failure "") <> "\n"))
          when isLoop $ talk conn
