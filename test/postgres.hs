{-# LANGUAGE OverloadedStrings #-}
import Keter.Postgres

main :: IO ()
main = do
    pg <- load "postgres.yaml"

    getInfo pg "foo" >>= print
    getInfo pg "bar" >>= print
    getInfo pg "foo" >>= print
    getInfo pg "bar" >>= print
