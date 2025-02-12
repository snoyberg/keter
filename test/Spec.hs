{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Keter.Config.V10
import Keter.Context
import Keter.LabelMap as LM
import Keter.Proxy
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Types.Status (ok200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wreq qualified as Wreq
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain keterTests

keterTests :: TestTree
keterTests =
  testGroup
    "Tests"
    [ testCase "Subdomain Integrity" caseSubdomainIntegrity
    , testCase "Wildcard Domains" caseWildcards
    , testCase "Head then post doesn't crash" headThenPostNoCrash
    ]

caseSubdomainIntegrity :: IO ()
caseSubdomainIntegrity = do
  let test0 = LM.empty
      test1 = LM.insert "someapp.com" () test0
      test2 = LM.insert "api.someapp.com" () test1
      test3a = LM.delete "someapp.com" test2
      test3b = LM.insert "api.someapp.com" () test0 -- case from the bug report
      msg = "Subdomains inserted and deleted between bundles"
  assertBool msg $ test3a == test3b

caseWildcards :: IO ()
caseWildcards = do
  let test0 = LM.empty
      test1 = LM.insert "*.someapp.com" () test0
      test2 = LM.lookup "a.someapp.com" test1
      msg = "Wildcards domains"
  assertBool msg $ isJust test2

headThenPostNoCrash :: IO ()
headThenPostNoCrash = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  exceptions <- newTQueueIO

  _ <- forkIO $ do
    Warp.run 6781 $ \req resp -> do
      void $ Wai.strictRequestBody req
      resp $ Wai.responseLBS ok200 [] "ok"

  _ <- forkIO $
    flip runReaderT (settings manager) $
      flip runLoggingT (\_ _ _ msg -> atomically $ writeTQueue exceptions msg) $
        filterLogger isException $
          runKeterM $
            reverseProxy $ LPInsecure "*" 6780

  threadDelay 0_100_000

  _res <- Wreq.head_ "http://localhost:6780"

  void $ Wreq.post "http://localhost:6780" content

  found <- atomically $ flushTQueue exceptions
  assertBool ("the list is not empty " <> show found) (null found)
  where
    content :: ByteString
    content = "a"

    -- For 'reverseProxy', only exceptions (and strictly exceptions!) are logged as LevelError.
    isException :: LogSource -> LogLevel -> Bool
    isException _ LevelError = True
    isException _ _ = False

    settings :: Manager -> ProxySettings
    settings manager = MkProxySettings {
        psHostLookup     = const $ pure $ Just ((PAPort 6781 Nothing, False), error "unused tls certificate")
      , psManager        = manager
      , psUnknownHost    = const ""
      , psMissingHost    = ""
      , psProxyException = ""
      , psIpFromHeader   = False
      , psConnectionTimeBound = 5 * 60 * 1000
      , psHealthcheckPath = Nothing
      }
