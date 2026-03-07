{-# LANGUAGE OverloadedStrings #-}

module Keter.RewriteSpec (tests) where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString.Char8 qualified as BS
import Test.Tasty
import Test.Tasty.HUnit
import Keter.Rewrite

tests :: TestTree
tests = testGroup "Keter.Rewrite"
  [ testGroup "ReverseProxyConfig parsing"
    [ testCase "preserve-host defaults to False when omitted" casePreserveHostDefault
    , testCase "preserve-host parses True" casePreserveHostTrue
    , testCase "preserve-host parses False explicitly" casePreserveHostFalse
    ]
  , testGroup "filterHeaders"
    [ testCase "strips Host header when preserveHost is False" caseFilterHeadersStripsHost
    , testCase "preserves Host header when preserveHost is True" caseFilterHeadersPreservesHost
    , testCase "always strips Transfer-Encoding" caseFilterHeadersStripsTransferEncoding
    , testCase "always strips Content-Length" caseFilterHeadersStripsContentLength
    , testCase "preserves other headers" caseFilterHeadersPreservesOthers
    ]
  ]

-- | Helper to parse ReverseProxyConfig from JSON/YAML bytes
parseReverseProxyConfig :: BS.ByteString -> Either String ReverseProxyConfig
parseReverseProxyConfig = eitherDecodeStrict

-- Config parsing tests

casePreserveHostDefault :: IO ()
casePreserveHostDefault = do
    let json = BS.unlines
          [ "{"
          , "  \"reversed-host\": \"localhost\","
          , "  \"reversed-port\": 3000,"
          , "  \"reversed-ssl\": false,"
          , "  \"reversing-host\": \"example.com\""
          , "}"
          ]
    case parseReverseProxyConfig json of
        Left err -> assertFailure ("Failed to parse: " <> err)
        Right cfg -> reversePreserveHost cfg @?= False

casePreserveHostTrue :: IO ()
casePreserveHostTrue = do
    let json = BS.unlines
          [ "{"
          , "  \"reversed-host\": \"localhost\","
          , "  \"reversed-port\": 3000,"
          , "  \"reversed-ssl\": false,"
          , "  \"reversing-host\": \"example.com\","
          , "  \"preserve-host\": true"
          , "}"
          ]
    case parseReverseProxyConfig json of
        Left err -> assertFailure ("Failed to parse: " <> err)
        Right cfg -> reversePreserveHost cfg @?= True

casePreserveHostFalse :: IO ()
casePreserveHostFalse = do
    let json = BS.unlines
          [ "{"
          , "  \"reversed-host\": \"localhost\","
          , "  \"reversed-port\": 3000,"
          , "  \"reversed-ssl\": false,"
          , "  \"reversing-host\": \"example.com\","
          , "  \"preserve-host\": false"
          , "}"
          ]
    case parseReverseProxyConfig json of
        Left err -> assertFailure ("Failed to parse: " <> err)
        Right cfg -> reversePreserveHost cfg @?= False

-- filterHeaders tests

caseFilterHeadersStripsHost :: IO ()
caseFilterHeadersStripsHost = do
    let headers = [("Host", "example.com"), ("Accept", "text/html")]
        result = filterHeaders False headers
    result @?= [("Accept", "text/html")]

caseFilterHeadersPreservesHost :: IO ()
caseFilterHeadersPreservesHost = do
    let headers = [("Host", "example.com"), ("Accept", "text/html")]
        result = filterHeaders True headers
    result @?= [("Host", "example.com"), ("Accept", "text/html")]

caseFilterHeadersStripsTransferEncoding :: IO ()
caseFilterHeadersStripsTransferEncoding = do
    let headers = [("Transfer-Encoding", "chunked"), ("Accept", "text/html")]
    -- Should be stripped regardless of preserveHost
    filterHeaders False headers @?= [("Accept", "text/html")]
    filterHeaders True headers @?= [("Accept", "text/html")]

caseFilterHeadersStripsContentLength :: IO ()
caseFilterHeadersStripsContentLength = do
    let headers = [("Content-Length", "123"), ("Accept", "text/html")]
    -- Should be stripped regardless of preserveHost
    filterHeaders False headers @?= [("Accept", "text/html")]
    filterHeaders True headers @?= [("Accept", "text/html")]

caseFilterHeadersPreservesOthers :: IO ()
caseFilterHeadersPreservesOthers = do
    let headers = [("Accept", "text/html"), ("User-Agent", "test"), ("X-Custom", "value")]
        result = filterHeaders False headers
    result @?= headers
