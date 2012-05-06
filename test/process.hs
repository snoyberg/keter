import Keter.Process
import Control.Concurrent

main = do
    p <- run "runghc" "." ["test/process-torun.hs"] []
    threadDelay $ 2 * 1000 * 1000
    putStrLn "Done with part 1"
    terminate p

    p <- run "runghc" "." ["test/process-torun2.hs"] []
    threadDelay $ 5 * 1000 * 1000
    terminate p
    putStrLn "Done with part 2"
