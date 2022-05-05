{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort)
import Data.Maybe (isJust)
import Keter.LabelMap as LM
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain keterTests

keterTests :: TestTree
keterTests =
  testGroup
    "Pre-2.0 Tests"
    [ testCase "Subdomain Integrity" caseSubdomainIntegrity,
      testCase "Wildcard Domains" caseWildcards
    ]

caseSubdomainIntegrity :: Assertion
caseSubdomainIntegrity = do
  let test0 = LM.empty
      test1 = LM.insert "someapp.com" () test0
      test2 = LM.insert "api.someapp.com" () test1
      test3a = LM.delete "someapp.com" test2
      test3b = LM.insert "api.someapp.com" () test0 -- case from the bug report
      msg = "Subdomains inserted and deleted between bundles"
  print test3a
  print test3b
  assertBool msg $ test3a == test3b

caseWildcards :: Assertion
caseWildcards = do
  let test0 = LM.empty
      test1 = LM.insert "*.someapp.com" () test0
      test2 = LM.lookup "a.someapp.com" test1
      msg = "Wildcards domains"
  assertBool msg $ isJust test2
