{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Prelude (error, (++), ($), IO, putStrLn)
import System.Environment (getArgs, getProgName)
import Keter.Main (keter)
import Paths_keter (version)
import Data.Version (showVersion)
import qualified Keter.Plugin.Postgres as Postgres
import Data.Default (def)
import Filesystem.Path.CurrentOS ((</>), decodeString)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn $ "keter version: " ++ showVersion version
        [dir] -> keter
            (decodeString dir)
            [\configDir -> Postgres.load def $ configDir </> "etc" </> "postgres.yaml"]
        _ -> do
            pn <- getProgName
            error $ "Usage: " ++ pn ++ " <config file>"
