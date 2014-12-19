{-# LANGUAGE OverloadedStrings #-}

import System.Environment


main :: IO ()
main = do
    [arg] <- getArgs
    env <- getEnvironment
    putStrLn "Running Background App."
    putStrLn $ "       Args: " ++ show arg
    putStrLn $ "Environment: " ++ show env
    putStrLn "Stopping Background App."
