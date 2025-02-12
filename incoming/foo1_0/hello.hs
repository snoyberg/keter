{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 qualified as L8
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Directory
import System.Environment
import System.IO


main :: IO ()
main = do
    fp <- canonicalizePath "."
    [msg] <- getArgs
    portS <- getEnv "PORT"
    env <- getEnvironment
    let port = read portS
    logger <- mkRequestLogger defaultRequestLoggerSettings
        { outputFormat = Apache FromHeader
        }
    run port $ logger $ \req send -> do
        liftIO $ putStrLn $ "Received a request at: " ++ show (pathInfo req)
        liftIO $ hFlush stdout
        liftIO $ hPutStrLn stderr $ "Testing standard error"
        liftIO $ hFlush stderr
        send $ responseLBS status200 [("content-type", "text/plain")]
             $ L8.pack $ unlines
             $ ("Message: " ++ msg)
             : ("Path: " ++ fp)
             : ("Headers: " ++ show (requestHeaders req))
             : map (\(k, v) -> concat [ "Env: "
                                      , k, " = ", v
                                      ]) env
