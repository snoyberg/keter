{-# LANGUAGE OverloadedStrings #-}
module Keter.Cli
    ( launchCli
    , CliStates(..)
    ) where

import Keter.Types.Common
import Keter.AppManager
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void, when)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative
import Data.Foldable
import GHC.Conc

data Commands = CmdListRunningApps
              | CmdExit

data CliStates = MkCliStates
  { csAppManager :: !AppManager
  , csLog        :: !(LogMessage -> IO ())
  , csPort       :: !Port
  }

launchCli :: CliStates -> IO ()
launchCli states = void $ forkIO $ withSocketsDo $ do
    addr <- resolve $ show $ csPort states
    E.bracket (open addr) close $ \x -> do
                                    csLog states $ BindCli addr
                                    loop states x
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
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock (addrAddress addr)
    listen sock 10
    return sock

loop :: CliStates -> Socket -> IO b
loop states sock = forever $ do
    (conn, peer) <- accept sock
    csLog states $ ReceivedCliConnection peer
    void $ forkFinally (talk states conn) (\_ -> close conn)

listRunningApps :: CliStates -> Socket -> IO ()
listRunningApps states conn = do
  txt <- atomically $ renderApps $ csAppManager states
  sendAll conn $ T.encodeUtf8 txt <> "\n"

talk :: CliStates -> Socket -> IO ()
talk states conn = do
    msg <- recv conn 1024
    unless (S.null msg) $ do
      case T.decodeUtf8' msg of
        Left exception -> sendAll conn ("decode error: " <> T.encodeUtf8 (T.pack $ show exception))
        Right txt -> do
          let res = execParserPure defaultPrefs (info (commandParser <**> helper)
                                                (fullDesc <> header "server repl" <> progDesc (
                        "repl for inspecting program state. You can connect to a socket and ask predefined questions")) ) (T.unpack <$> T.words txt)
          isLoop <- case res of
            (Success (CmdListRunningApps)) -> True <$ listRunningApps states conn
            (Success (CmdExit   )) -> False <$ sendAll conn "bye\n"
            (CompletionInvoked x) -> True <$ sendAll conn "completion ignored \n"
            Failure failure        ->
              True <$ sendAll conn (T.encodeUtf8 (T.pack $ fst $ renderFailure failure "") <> "\n")
          when isLoop $ talk states conn
