{-# LANGUAGE OverloadedStrings #-}

module Keter.Config.V10Spec (tests) where

import Data.ByteString.Char8 qualified as BS
import Data.Set qualified as Set
import Keter.Config.V10
import Keter.Yaml.FilePath (decodeFileRelative)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Keter.Config.V10 (WebAppConfig parsing)"
  [ testCase "port-env-vars parses a list of variable names" casePortEnvVarsList
  , testCase "port-env-vars defaults to empty when omitted" casePortEnvVarsDefault
  , testCase "port-env-vars parses empty list" casePortEnvVarsEmpty
  ]

-- | Helper: write YAML bytes to a temp file, then parse as a 'WebAppConfig'.
parseWebAppConfig :: BS.ByteString -> IO (WebAppConfig ())
parseWebAppConfig yaml =
    withSystemTempFile "keter-test-.yaml" $ \fp h -> do
        BS.hPut h yaml
        hClose h
        result <- decodeFileRelative fp
        case result of
            Left err  -> assertFailure ("Failed to parse: " <> show err) >> error "unreachable"
            Right wac -> return wac

casePortEnvVarsList :: IO ()
casePortEnvVarsList = do
    wac <- parseWebAppConfig $ BS.unlines
        [ "exec: /tmp/myapp"
        , "host: example.com"
        , "port-env-vars:"
        , "  - YESOD_PORT"
        , "  - RAILS_PORT"
        ]
    waconfigPortEnvVars wac @?= Set.fromList ["YESOD_PORT", "RAILS_PORT"]

casePortEnvVarsDefault :: IO ()
casePortEnvVarsDefault = do
    wac <- parseWebAppConfig $ BS.unlines
        [ "exec: /tmp/myapp"
        , "host: example.com"
        ]
    waconfigPortEnvVars wac @?= Set.empty

casePortEnvVarsEmpty :: IO ()
casePortEnvVarsEmpty = do
    wac <- parseWebAppConfig $ BS.unlines
        [ "exec: /tmp/myapp"
        , "host: example.com"
        , "port-env-vars: []"
        ]
    waconfigPortEnvVars wac @?= Set.empty
