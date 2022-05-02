{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude (error, (++), ($), IO, putStrLn)
import System.Environment (getArgs, getProgName)
import Keter.Main (keter)
import Paths_keter (version)
import Data.Version (showVersion)
import qualified Keter.Plugin.Postgres as Postgres
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
