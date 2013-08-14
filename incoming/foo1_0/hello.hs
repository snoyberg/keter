{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Directory
import Control.Monad.IO.Class
import System.IO
import Network.Wai.Middleware.RequestLogger
import Data.Default

main :: IO ()
main = do
    fp <- canonicalizePath "."
    [msg] <- getArgs
    portS <- getEnv "PORT"
    env <- getEnvironment
    let port = read portS
    logger <- mkRequestLogger def
        { outputFormat = Apache FromHeader
        }
    run port $ logger $ \req -> do
        liftIO $ putStrLn $ "Received a request at: " ++ show (pathInfo req)
        liftIO $ hFlush stdout
        liftIO $ hPutStrLn stderr $ "Testing standard error"
        liftIO $ hFlush stderr
        return $ responseLBS status200 [("content-type", "text/plain")] $ L8.pack $ unlines
            $ ("Message: " ++ msg)
            : ("Path: " ++ fp)
            : ("Headers: " ++ show (requestHeaders req))
            : map (\(k, v) -> concat
                [ "Env: "
                , k
                , " = "
                , v
                ]) env
