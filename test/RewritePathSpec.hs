{-# LANGUAGE OverloadedStrings #-}

module RewritePathSpec where

import           Data.Monoid
import           Network.HTTP.ReverseProxy.Rewrite
import           Network.URI                       (URI (..), URIAuth (..),
                                                    nullURI,
                                                    parseRelativeReference)
import           Test.Hspec

site :: String
site = "subdomain.host.com"

defURI :: URI
defURI = nullURI{uriAuthority = Just $ URIAuth "" site "",
                 uriPath      = "",
                 uriQuery     = ""}

spec :: Spec
spec = do
  describe "Merging Queries" $ do
    it "merges two queries" $ do
      let origQS = nullURI{uriQuery="?qs1=1&chngQ=origValue"}
          rewQS  = nullURI{uriQuery="?qs2=3&qs3=&chngQ=rewValue"}
      uriQuery (mergeQueries origQS rewQS) `shouldBe` "?chngQ=rewValue&qs1=1&qs2=3&qs3="

  describe "Rewrite Path" $ do
    it "matches Host, Path and Query" $ do
      let url =                                    "//" <> site <> "/?query=qValue1&another=qValue2"
          rp  = RewritePath HostPathQuery (read $ "^//" <> site <> "(/?)\\?query=(.*)&another=(.*)")
                                                  ("//" <> site <> "/abc?query=$2&another=$3")
      (parseRelativeReference url >>= rewritePathRule [rp])
         `shouldBe` Just defURI{ uriPath  = "/abc",
                                 uriQuery ="?another=qValue2&query=qValue1"}

    it "matches Host, Path and Query with subdomain prefix" $ do
      let url =                                    "//part-" <> site <> "/?query=qValue1"
          rp  = RewritePath HostPathQuery (read $ "^//(.*)-" <> site <> "(/?)\\?query=(.*)")
                                                      ( "//" <> site <> "/abc?domain=$1&query=$3")
      (parseRelativeReference url >>= rewritePathRule [rp])
         `shouldBe` Just defURI{ uriPath  = "/abc",
                                 uriQuery ="?domain=part&query=qValue1"}

    it "matches Path and Query" $ do
      let url =                    "/path1/more_path/?query=qValue1"
          rp  = RewritePath PathQuery (read "^/(.*)/\\?query=(.*)")
                                            "/$1/abc?query=$2"
      (parseRelativeReference url >>= rewritePathRule [rp])
         `shouldBe` Just nullURI{ uriPath  = "/path1/more_path/abc",
                                  uriQuery ="?query=qValue1"}

    it "matches Path and Query, rewrite part to query" $ do
      let url =                            "/path1/more_fruit/rest"
          rp  = RewritePath PathOnly (read "^/(.*)/more_(.*)/(.*)")
                                           "/$1/move-to-query/$3/?query=$2"

      (parseRelativeReference url >>= rewritePathRule [rp])
         `shouldBe` Just nullURI{ uriPath  = "/path1/move-to-query/rest/?query=fruit"}
    it "checks that vars in regex are inside the bounds" $ do
      let url =                                  "//host.com/abc?query=$2&another=$4"
          rp  = RewritePath HostPathQuery (read "^//host.com(/?)\\?query=(.*)&another=(.*)") url
      checkRegexVars (pathRegex rp) url `shouldBe` False
