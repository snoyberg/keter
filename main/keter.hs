{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (error, (++), ($), IO, putStrLn)
import System.Environment (getArgs, getProgName)
import Keter.Main (keter)
import Paths_keter (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn $ "keter version: " ++ showVersion version
        [dir] -> keter dir
        _ -> do
            pn <- getProgName
            error $ "Usage: " ++ pn ++ " <root folder>"
