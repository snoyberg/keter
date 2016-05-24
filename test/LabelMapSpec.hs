
{-# LANGUAGE OverloadedStrings #-}

module LabelMapSpec where

import qualified Data.IORef     as I
import           Test.Hspec
import           Test.HUnit

import qualified Keter.LabelMap as LM
import           Data.Maybe


spec :: Spec
spec = do
  describe "LabelMap" $ do
    it "modified subdmonains" caseSubdomainIntegrity
    it "assert wildcards" caseWildcards


{-

I have two bundles.

Bundle A:

stanzas:
    - type: webapp
      exec: ../dist/build/app/app
      hosts:
          - someapp.com
Bundle B:

stanzas:
    - type: webapp
      exec: ../dist/build/api/api
      hosts:
          - api.someapp.com
-}
caseSubdomainIntegrity :: Assertion
caseSubdomainIntegrity = do

  let test0 = LM.empty
      test1 = LM.insert "someapp.com"         () test0
      test2 = LM.insert "api.someapp.com"     () test1
      test3a = LM.delete "someapp.com"           test2
      test3b = LM.insert "api.someapp.com"    () test0 -- case from the bug report

  let test3 = LM.insert "ipa.someapp.com"     () test2
      test4 = LM.insert "bla.api.someapp.com" () test3
      test5 = LM.delete "someapp.com"            test4
      test6 = LM.delete "ipa.someapp.com"        test5
      test7 = LM.delete "api.someapp.com"        test6
      test8 = LM.delete "bla.api.someapp.com"    test7
      test9 = LM.delete "bla.api.someapp.com"    test4
      msg = "Subdomains inserted and deleted between bundles"

  print test3a
  print test3b

  assertBool msg $ test3a == test3b

caseWildcards :: Assertion
caseWildcards = do
  let test0 = LM.empty
      test1 = LM.insert "*.someapp.com"       () test0
      test2 = LM.lookup "a.someapp.com"         test1
      msg = "Wildcards domains"

  assertBool msg $ isJust test2
