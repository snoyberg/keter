{-# LANGUAGE OverloadedStrings #-}
import Keter.LogFile
import Keter.Prelude

main = runKIO print $ do
    Right lf <- start "log/test"
    addChunk lf "foo\n"
    addChunk lf "bar\n"
    addChunk lf "baz\n"
