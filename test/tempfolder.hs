{-# LANGUAGE OverloadedStrings #-}
import Keter.TempFolder

main :: IO ()
main = do
    tf <- setup "temp"
    getFolder tf "foo" >>= print
    getFolder tf "foo" >>= print
    getFolder tf "bar" >>= print
    getFolder tf "bar" >>= print
    getFolder tf "foo" >>= print
