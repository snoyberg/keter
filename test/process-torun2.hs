import Control.Concurrent
import Data.Time
import Control.Monad
import System.IO

main = withFile "process2.txt" WriteMode $ \h -> forever $ do
    now <- getCurrentTime
    hPutStrLn h $ show now
    hFlush h
    threadDelay 1000000
