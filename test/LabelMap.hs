{-#LANGUAGE  OverloadedStrings#-}
module LabelMap where

import  qualified Keter.LabelMap as LM

-- create
test0 = LM.empty

{--
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

test1 = LM.insert "someapp.com"     () test0
test2 = LM.insert "api.someapp.com" () test1
test3 = LM.insert "ipa.someapp.com" () test2
test4 = LM.insert "bla.api.someapp.com"  () test3
test5 = LM.delete "someapp.com"        test4
test6 = LM.delete "ipa.someapp.com"    test5
test7 = LM.delete "api.someapp.com"    test6
test8 = LM.delete "bla.api.someapp.com"    test7
test9 = LM.delete "bla.api.someapp.com"    test4


testb = LM.insert "api.someapp.com" () test0
-- test = test5 == test7 !!
