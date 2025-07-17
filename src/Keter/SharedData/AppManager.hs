{-# LANGUAGE OverloadedStrings #-}

module Keter.SharedData.AppManager
    ( Action(..)
    , AppState(..)
    , showAppState
    ) where

import Control.Concurrent.STM (STM, TVar, readTVar)
import Data.Foldable (fold)
import Data.Text (Text, pack, unpack)
import Keter.Config (AppInput)
import Keter.SharedData.App (App, showApp)
import System.Posix.Types (EpochTime)
import Text.Printf (printf)

data Action = Reload AppInput | Terminate
 deriving Show

data AppState = ASRunning App
              | ASStarting
                    !(Maybe App)
                    !(TVar (Maybe EpochTime))
                    !(TVar (Maybe Action)) -- ^ the next one to try
              | ASTerminated

showAppState :: AppState -> STM Text
showAppState (ASRunning x) = (\x' -> "running(" <> x' <> ")") <$> showApp x
showAppState (ASStarting mapp tmtime tmaction) = do
  mtime   <- readTVar tmtime
  maction <- readTVar tmaction
  mtext <- traverse showApp mapp
  pure $ pack $ printf "starting app %s, time %s, action %s \n" (unpack $ fold mtext) (show mtime) (show maction)
showAppState ASTerminated = pure "terminated"
