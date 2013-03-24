{-# LANGUAGE OverloadedStrings #-}
module Keter.ReverseProxy 
  ( ReverseProxyConfig (..)
  , RewriteRule (..)
  , RPEntry (..)
  , simpleReverseProxy
  )
  where

import Control.Applicative ((<$>),(<*>),(<|>))
import Data.Monoid ((<>))

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Array ((!))

import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.CaseInsensitive as CI

import Blaze.ByteString.Builder (flush, fromByteString)

-- Configuration files
import Data.Yaml (FromJSON (..), Value (Object), (.:), (.:?), (.!=))
import Data.Default

-- Regular expression parsing, replacement, matching
import Data.Attoparsec.Text (string, takeWhile1, endOfInput, parseOnly, Parser)
import Text.Regex.TDFA (makeRegex, matchOnceText, MatchText)
import Text.Regex.TDFA.String (Regex)
import Data.Char (isDigit)

-- Reverse proxy apparatus
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (ResumableSource (..))

import qualified Network.Wai as Wai
import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Monad.IO.Class

data ReverseProxyConfig = ReverseProxyConfig
    { reversedHost :: Text
    , reversedPort :: Int
    , reversingHost :: Text
    , reverseUseSSL :: Bool
    , rewriteResponseRules :: Set RewriteRule
    , rewriteRequestRules :: Set RewriteRule
    } deriving (Eq, Ord)

instance FromJSON ReverseProxyConfig where
    parseJSON (Object o) = ReverseProxyConfig
        <$> o .: "reversed-host"
        <*> o .: "reversed-port"
        <*> o .: "reversing-host"
        <*> o .: "ssl" .!= False
        <*> o .:? "rewrite-response" .!= Set.empty
        <*> o .:? "rewrite-request" .!= Set.empty
    parseJSON _ = fail "Wanted an object"

instance Default ReverseProxyConfig where
    def = ReverseProxyConfig
        { reversedHost = ""
        , reversedPort = 80
        , reversingHost = ""
        , reverseUseSSL = False
        , rewriteResponseRules = Set.empty
        , rewriteRequestRules = Set.empty
        }

data RewriteRule = RewriteRule
    { ruleHeader :: Text
    , ruleRegex :: Text
    , ruleReplacement :: Text
    } deriving (Eq, Ord)

instance FromJSON RewriteRule where
    parseJSON (Object o) = RewriteRule
        <$> o .: "header"
        <*> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

data RPEntry = RPEntry
    { config :: ReverseProxyConfig
    , httpManager :: Manager
    }

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

-- RFC 4.4 Message Length - the server closed the connection
hasLegacyMessageLength :: ResponseHeaders -> Bool
hasLegacyMessageLength headers =
  let connection = lookup "Connection" headers
      headerNames = map fst headers in
    (connection `elem` [Just "Close",Just "close"])
    && "Transfer-Encoding" `notElem` headerNames
    && "Content-Length" `notElem` headerNames

-- Pull the first message, throw away and close the connection, and return what was pulled in a single response.
handleLegacyMessage :: Status -> ResponseHeaders -> ResumableSource (ResourceT IO) S.ByteString -> ResourceT IO Wai.Response
handleLegacyMessage status headers body = do
  liftIO $ print "Pre consume"
  content <- body $$+- (CL.mapM (liftIO . go) =$ CL.isolate 100 =$ CL.consume)
  liftIO $ print "Post consume"
  return $
    case content of
      [] -> Wai.ResponseBuilder status headers flush
      (x:_)  -> Wai.ResponseBuilder status headers (foldl1 (<>) $ map fromByteString content)
  where
    go s = do
      putStrLn $ "Received chunk of size: " ++ show (S.length s)
      return s

-- Simply map the output of the HTTP-Conduit to a response without unwrapping the base ResourceT.
mapResponse :: Status -> ResponseHeaders -> (Source (ResourceT IO) S.ByteString, (ResourceT IO) ()) -> Wai.Response
mapResponse status headers (body, _) =
    Wai.ResponseSource status headers $ mapOutput (Chunk . fromByteString) body

simpleReverseProxy :: RPEntry -> Wai.Application
simpleReverseProxy (RPEntry (ReverseProxyConfig h p _ ssl respRules reqRules) mgr) request =
  do
    let proxiedRequest = def
          { method = Wai.requestMethod request
          , secure = ssl
          , host   = encodeUtf8 h
          , port   = p
          , path   = Wai.rawPathInfo request
          , queryString = Wai.rawQueryString request
          , requestHeaders = filterHeaders $ map (rewriteHeader reqRuleMap) (Wai.requestHeaders request)
          , requestBody = RequestBodySourceChunked (mapOutput fromByteString $ Wai.requestBody request)
          , decompress = const False
          , redirectCount = 0
          , checkStatus = \_ _ _ -> Nothing
          --, responseTimeout = 5000 -- current default (as of 2013-03-18)
          , cookieJar = Nothing
          }
    response <- http proxiedRequest mgr
    let status = responseStatus response
        respHeaders = filterHeaders $ map (rewriteHeader respRuleMap) (responseHeaders response)
    -- hasLegacyMessageLength checks the response headers before stripping out
    -- transfer encoding, content length fields
    if hasLegacyMessageLength (responseHeaders response)
      then handleLegacyMessage status respHeaders (responseBody response)
      else mapResponse status respHeaders <$> unwrapResumable (responseBody response)
  where
    reqRuleMap = Map.fromList . map (\k -> (CI.mk . encodeUtf8 $ ruleHeader k, k)) $ Set.toList reqRules
    respRuleMap = Map.fromList . map (\k -> (CI.mk . encodeUtf8 $ ruleHeader k, k)) $ Set.toList respRules
    useHeader ("Transfer-Encoding", _) = False
    useHeader ("Content-Length", _)    = False
    useHeader ("Host", _)              = False
    useHeader _                        = True
    filterHeaders = filter useHeader