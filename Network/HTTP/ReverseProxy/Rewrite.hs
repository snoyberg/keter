{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
module Network.HTTP.ReverseProxy.Rewrite
  ( ReverseProxyConfig (..)
  , RewriteRule (..)
  , RPEntry (..)
  , simpleReverseProxy
  )
  where

import Control.Applicative
import Control.Exception (bracket)
import Data.Function (fix)
import Data.Monoid ((<>))

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Array ((!))
import Data.Aeson
import Control.Monad (unless)

import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.CaseInsensitive as CI

import Blaze.ByteString.Builder (fromByteString)

import Keter.Types.Common
-- Configuration files
import Data.Default

-- Regular expression parsing, replacement, matching
import Data.Attoparsec.Text (string, takeWhile1, endOfInput, parseOnly, Parser)
import Text.Regex.TDFA (makeRegex, matchOnceText, MatchText)
import Text.Regex.TDFA.String (Regex)
import Data.Char (isDigit)

-- Reverse proxy apparatus
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as I
import Network.HTTP.Client.Conduit
import qualified Network.HTTP.Client as NHC
import Network.HTTP.Types

data RPEntry = RPEntry
    { config :: ReverseProxyConfig
    , httpManager :: Manager
    }

instance Show RPEntry where
  show x = "RPEntry { config = " ++ (show $ config x) ++ " }"

getGroup :: MatchText String -> Int -> String
getGroup matches i = fst $ matches ! i

rewrite :: (String, MatchText String, String) -> String -> String -> Text
rewrite (before, match, after) input replacement =
  case parseOnly parseSubstitute (T.pack replacement) of
    Left _ -> T.pack input
    Right result -> T.pack before <> result <> T.pack after
  where
    parseSubstitute :: Parser Text
    parseSubstitute =
          (endOfInput >> "")
      <|> do
          { _ <- string "\\\\"
          ; rest <- parseSubstitute
          ; return $ "\\" <> rest
          }
      <|> do
          { _ <- string "\\"
          ; n <- (fmap (read . T.unpack) $ takeWhile1 isDigit) :: Parser Int
          ; rest <- parseSubstitute
          ; return $ T.pack (getGroup match n) <> rest
          }
      <|> do
          { text <- takeWhile1 (/= '\\')
          ; rest <- parseSubstitute
          ; return $ text <> rest
          }

rewriteHeader :: Map HeaderName RewriteRule -> Header -> Header
rewriteHeader rules header@(name, value) =
  case Map.lookup name rules of
    Nothing -> header
    Just  r -> (name, regexRewrite r value)

rewriteHeaders :: Map HeaderName RewriteRule -> [Header] -> [Header]
rewriteHeaders ruleMap = map (rewriteHeader ruleMap)

regexRewrite :: RewriteRule -> S.ByteString -> S.ByteString
regexRewrite (RewriteRule _ regex' replacement) input =
  case matchOnceText regex strInput of
    Just  match -> encodeUtf8 $ rewrite match strInput strReplacement
    Nothing     -> input
  where
    strRegex = T.unpack regex'
    regex :: Regex
    regex = makeRegex strRegex
    strInput = T.unpack . decodeUtf8 $ input
    strReplacement = T.unpack replacement

filterHeaders :: [Header] -> [Header]
filterHeaders = filter useHeader
  where
    useHeader ("Transfer-Encoding", _) = False
    useHeader ("Content-Length", _)    = False
    useHeader ("Host", _)              = False
    useHeader _                        = True

mkRuleMap :: Set RewriteRule -> Map HeaderName RewriteRule
mkRuleMap = Map.fromList . map (\k -> (CI.mk . encodeUtf8 $ ruleHeader k, k)) . Set.toList

mkRequest :: ReverseProxyConfig -> Wai.Request -> Request
mkRequest rpConfig request =
#if MIN_VERSION_http_client(0, 5, 0)
   NHC.defaultRequest
      { NHC.checkResponse = \_ _ -> return ()
      , NHC.responseTimeout = maybe NHC.responseTimeoutNone NHC.responseTimeoutMicro $ reverseTimeout rpConfig
#else
   def
      { NHC.checkStatus = \_ _ _ -> Nothing
      , NHC.responseTimeout = reverseTimeout rpConfig
#endif
      , method = Wai.requestMethod request
      , secure = reversedUseSSL rpConfig
      , host   = encodeUtf8 $ reversedHost rpConfig
      , port   = reversedPort rpConfig
      , path   = Wai.rawPathInfo request
      , queryString = Wai.rawQueryString request
      , requestHeaders = filterHeaders $ rewriteHeaders reqRuleMap (Wai.requestHeaders request)
      , requestBody =
          case Wai.requestBodyLength request of
            Wai.ChunkedBody   -> RequestBodyStreamChunked ($ I.getRequestBodyChunk request)
            Wai.KnownLength n -> RequestBodyStream (fromIntegral n) ($ I.getRequestBodyChunk request)
      , decompress = const False
      , redirectCount = 0
      , cookieJar = Nothing
      , requestVersion = Wai.httpVersion request
      }
  where
    reqRuleMap = mkRuleMap $ rewriteRequestRules rpConfig

simpleReverseProxy :: Manager -> ReverseProxyConfig -> Wai.Application
simpleReverseProxy mgr rpConfig request sendResponse = bracket
    (NHC.responseOpen proxiedRequest mgr)
    responseClose
    $ \res -> sendResponse $ Wai.responseStream
        (responseStatus res)
        (rewriteHeaders respRuleMap $ responseHeaders res)
        (sendBody $ responseBody res)
  where
    proxiedRequest = mkRequest rpConfig request
    respRuleMap = mkRuleMap $ rewriteResponseRules rpConfig
    sendBody body send _flush = fix $ \loop -> do
        bs <- body
        unless (S.null bs) $ do
            () <- send $ fromByteString bs
            loop

data ReverseProxyConfig = ReverseProxyConfig
    { reversedHost :: Text
    , reversedPort :: Int
    , reversedUseSSL :: Bool
    , reversingHost :: Text
    , reversingUseSSL :: !SSLConfig
    , reverseTimeout :: Maybe Int
    , rewriteResponseRules :: Set RewriteRule
    , rewriteRequestRules :: Set RewriteRule
    } deriving (Eq, Ord, Show)

instance FromJSON ReverseProxyConfig where
    parseJSON (Object o) = ReverseProxyConfig
        <$> o .: "reversed-host"
        <*> o .: "reversed-port"
        <*> o .: "reversed-ssl" .!= False
        <*> o .: "reversing-host"
        <*> o .:? "ssl" .!= SSLFalse
        <*> o .:? "timeout" .!= Nothing
        <*> o .:? "rewrite-response" .!= Set.empty
        <*> o .:? "rewrite-request" .!= Set.empty
    parseJSON _ = fail "Wanted an object"

instance ToJSON ReverseProxyConfig where
    toJSON ReverseProxyConfig {..} = object
        [ "reversed-host" .= reversedHost
        , "reversed-port" .= reversedPort
        , "reversed-ssl" .= reversedUseSSL
        , "reversing-host" .= reversingHost
        , "ssl" .= reversingUseSSL
        , "timeout" .= reverseTimeout
        , "rewrite-response" .= rewriteResponseRules
        , "rewrite-request" .= rewriteRequestRules
        ]

instance Default ReverseProxyConfig where
    def = ReverseProxyConfig
        { reversedHost = ""
        , reversedPort = 80
        , reversedUseSSL = False
        , reversingHost = ""
        , reversingUseSSL = SSLFalse
        , reverseTimeout = Nothing
        , rewriteResponseRules = Set.empty
        , rewriteRequestRules = Set.empty
        }

data RewriteRule = RewriteRule
    { ruleHeader :: Text
    , ruleRegex :: Text
    , ruleReplacement :: Text
    } deriving (Eq, Ord, Show)

instance FromJSON RewriteRule where
    parseJSON (Object o) = RewriteRule
        <$> o .: "header"
        <*> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

instance ToJSON RewriteRule where
    toJSON RewriteRule {..} = object
        [ "header" .= ruleHeader
        , "from" .= ruleRegex
        , "to" .= ruleReplacement
        ]
