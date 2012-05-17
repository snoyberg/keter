{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Directory
import Control.Monad.IO.Class
import System.IO

main :: IO ()
main = do
    fp <- canonicalizePath "."
    [msg] <- getArgs
    portS <- getEnv "PORT"
    let port = read portS
    run port $ \req -> do
        liftIO $ putStrLn $ "Received a request at: " ++ show (pathInfo req)
        liftIO $ hFlush stdout
        liftIO $ hPutStrLn stderr $ "Testing standard error"
        liftIO $ hFlush stderr
        return $ responseLBS status200 [("content-type", "text/plain")] $ L8.pack $ unlines
            [ "Message: " ++ msg
            , "Path: " ++ fp
            ]
