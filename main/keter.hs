{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (error, (++), ($), IO)
import System.Environment (getArgs, getProgName)
import Keter.Main (keter)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> keter dir
        _ -> do
            pn <- getProgName
            error $ "Usage: " ++ pn ++ " <root folder>"
