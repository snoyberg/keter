{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Version (showVersion)
import Keter.Main (keter)
import Keter.Plugin.Postgres qualified as Postgres
import Paths_keter (version)
import Prelude (IO, error, putStrLn, ($), (++))
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn $ "keter version: " ++ showVersion version
        ["--help"] -> printUsage
        [dir] -> keter
            dir
            [\configDir -> Postgres.load Postgres.defaultSettings $ configDir </> "etc" </> "postgres.yaml"]
        _ -> printUsage

printUsage :: IO ()
printUsage = do
    pn <- getProgName
    error $ "Usage: " ++ pn ++ " <config file>"
