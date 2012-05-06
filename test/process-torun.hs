import Control.Concurrent
import Data.Time

main = do
    now <- getCurrentTime
    writeFile "process.txt" $ show now
    threadDelay 1000000
