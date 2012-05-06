import Keter.Nginx
import Data.Default

main :: IO ()
main = do
    let settings = def
            { configFile = "nginx.conf"
            , reloadAction = do
                putStrLn "Reloading"
                readFile "nginx.conf" >>= putStrLn
            }
    nginx <- start settings
    p <- getPort nginx
    addEntry nginx "host1" $ AppEntry p
    p2 <- getPort nginx
    addEntry nginx "static.host1" $ StaticEntry "/some/path"
    p3 <- getPort nginx
    addEntry nginx "host1" $ AppEntry p3
    releasePort nginx p
    removeEntry nginx "static.host1"
    releasePort nginx p2
    removeEntry nginx "host1"
    releasePort nginx p
