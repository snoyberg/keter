{-# LANGUAGE OverloadedStrings #-}
import Keter.TempFolder
import Keter.App
import qualified Keter.Nginx as N
import Data.Default
import Control.Concurrent
import System.Directory (copyFile)

main :: IO ()
main = do
    tf <- setup "temp"
    nginx <- N.start def
    copyFile "incoming/foo1.keter" "incoming/foo.keter"
    (app, toRun) <- start tf nginx "incoming/foo.keter" $ putStrLn "It's dead Jim"
    toRun
    threadDelay $ 2 * 1000 * 1000
    copyFile "incoming/foo2.keter" "incoming/foo.keter"
    reload app
    threadDelay $ 2 * 1000 * 1000
    putStrLn "Terminating..."
    terminate app
    threadDelay $ 120 * 1000 * 1000
