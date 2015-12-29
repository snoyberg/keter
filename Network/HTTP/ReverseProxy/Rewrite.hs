{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.HTTP.ReverseProxy.Rewrite
  ( ReverseProxyConfig (..)
  , RewriteRule (..)
  , RPEntry (..)
  , RewritePath (..)
  , MatchType (..)
  , simpleReverseProxy
  , rewritePathRule
  , mergeQueries
  , checkRegexVars
  )
  where

import           Control.Applicative         ((<$>), (<*>), (<|>))
import           Control.Exception           (bracket)
import           Data.Function               (fix)
import           Data.List.Split             (splitOn)
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Monoid                 ((<>))
import           Text.Read                   (readMaybe)

import           Control.Monad               (unless)
import           Data.Aeson
import           Data.Array                  (bounds, (!))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as BSC
import qualified Data.CaseInsensitive        as CI
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Blaze.ByteString.Builder    (fromByteString)

-- Configuration files
import           Data.Default

-- Regular expression parsing, replacement, matching
import           Data.Attoparsec.Text        (Parser, char, endOfInput,
                                              parseOnly, string, takeWhile1)
import           Data.Char                   (isDigit)
import           Text.Regex.TDFA             (MatchText, makeRegex,
                                              matchOnceText)
import           Text.Regex.TDFA.Common      (Regex (..))

-- Reverse proxy apparatus
import qualified Network.HTTP.Client         as NHC
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import           Network.URI                 (URI (..), URIAuth (..), nullURI,
                                              parseRelativeReference)
import qualified Network.Wai                 as Wai

data RPEntry = RPEntry
    { config      :: ReverseProxyConfig
    , httpManager :: Manager
    }

instance Show RPEntry where
  show x = "RPEntry { config = " ++ (show $ config x) ++ " }"

getGroup :: MatchText String -> Int -> String
getGroup matches i = fst $ matches ! i

rewrite :: Char -> (String, MatchText String, String) -> String -> String -> Text
rewrite varPrefix (before, match, after) input replacement =
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
          { _ <- char varPrefix
          ; n <- (read . T.unpack <$> takeWhile1 isDigit) :: Parser Int
          ; rest <- parseSubstitute
          ; return $ T.pack (getGroup match n) <> rest
          }
      <|> do
          { text <- takeWhile1 (/= varPrefix)
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
    Just  match -> encodeUtf8 $ rewrite '\\' match strInput strReplacement
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

mkRequest :: ReverseProxyConfig -> Wai.Request -> (Request,Maybe URI)
mkRequest rpConfig request =
  (def {
     method         = Wai.requestMethod request
   , secure         = reverseUseSSL rpConfig
   , host           = BSC.pack host
   , port           = reversedPort rpConfig
   , path           = BSC.pack $ uriPath  uri
   , queryString    = BSC.pack $ uriQuery uri
   , requestHeaders = filterHeaders $ rewriteHeaders reqRuleMap headers
   , requestBody    =
       case Wai.requestBodyLength request of
         Wai.ChunkedBody   -> RequestBodyStreamChunked ($ Wai.requestBody request)
         Wai.KnownLength n -> RequestBodyStream (fromIntegral n) ($ Wai.requestBody request)
   , decompress      = const False
   , redirectCount   = 10 -- FIXMEE: Why is this reduced to 0 from default 10???
   , checkStatus     = \_ _ _ -> Nothing
   , responseTimeout = reverseTimeout rpConfig
   , cookieJar       = Nothing
   }
  , mkURI)
  where
    headers    = Wai.requestHeaders request
    mkURI      = rewritePathRule (rewritePath rpConfig) rewURI
    uri        = fromMaybe rewURI mkURI
    reqRuleMap = mkRuleMap $ rewriteRequestRules rpConfig
    host       = T.unpack  $ reversedHost        rpConfig
    rewURI     =
      nullURI{uriAuthority = Just $ URIAuth ""
                                            (maybe host BSC.unpack $ lookup "Host" headers)
                                            "",
              uriPath      = BSC.unpack $ Wai.rawPathInfo    request,
              uriQuery     = BSC.unpack $ Wai.rawQueryString request}


simpleReverseProxy :: Manager -> ReverseProxyConfig -> Wai.Application
simpleReverseProxy mgr rpConfig request sendResponse = bracket
    (NHC.responseOpen proxiedRequest mgr)
    (\res -> do
        responseClose res
        case mRewrite of
          Just rp -> putStrLn $ "Rewrite path: " <> show rp
          _       -> return ())
    $ \res -> sendResponse $ Wai.responseStream
        (responseStatus res)
        (rewriteHeaders respRuleMap $ responseHeaders res)
        (sendBody $ responseBody res)
  where
    (proxiedRequest,mRewrite) = mkRequest rpConfig request
    respRuleMap = mkRuleMap $ rewriteResponseRules rpConfig
    sendBody body send _flush = fix $ \loop -> do
        bs <- body
        unless (S.null bs) $ do
            () <- send $ fromByteString bs
            loop

data ReverseProxyConfig = ReverseProxyConfig
    { reversedHost         :: Text
    , reversedPort         :: Int
    , reversingHost        :: Text
    , reverseUseSSL        :: Bool
    , reverseTimeout       :: Maybe Int
    , rewriteResponseRules :: Set RewriteRule
    , rewriteRequestRules  :: Set RewriteRule
    , rewritePath          :: [RewritePath]
    } deriving (Eq, Ord, Show)

instance FromJSON ReverseProxyConfig where
    parseJSON (Object o) = ReverseProxyConfig
        <$> o .: "reversed-host"
        <*> o .: "reversed-port"
        <*> o .: "reversing-host"
        <*> o .:? "ssl" .!= False
        <*> o .:? "timeout" .!= Nothing
        <*> o .:? "rewrite-response" .!= Set.empty
        <*> o .:? "rewrite-request" .!= Set.empty
        <*> o .:? "rewrite-path"     .!= []
    parseJSON _ = fail "Wanted an object"

instance ToJSON ReverseProxyConfig where
    toJSON ReverseProxyConfig {..} = object
        [ "reversed-host" .= reversedHost
        , "reversed-port" .= reversedPort
        , "reversing-host" .= reversingHost
        , "ssl" .= reverseUseSSL
        , "timeout" .= reverseTimeout
        , "rewrite-response" .= rewriteResponseRules
        , "rewrite-request" .= rewriteRequestRules
        , "rewrite-path"     .= rewritePath
        ]

instance Default ReverseProxyConfig where
    def = ReverseProxyConfig
        { reversedHost = ""
        , reversedPort = 80
        , reversingHost = ""
        , reverseUseSSL = False
        , reverseTimeout = Nothing
        , rewriteResponseRules = Set.empty
        , rewriteRequestRules = Set.empty
        , rewritePath          = []
        }

data RewriteRule = RewriteRule
    { ruleHeader      :: Text
    , ruleRegex       :: Text
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

----------------------------------------------------------------------------------
-- |
-- | Rewriting path
-- |
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- | Types, records and instances
----------------------------------------------------------------------------------

-- | Which parts of URI get matched
data MatchType = PathOnly | PathQuery | HostPath | HostPathQuery deriving (Eq,Ord)

instance Show MatchType where
  show PathOnly      = "path-only"
  show PathQuery     = "path-query"
  show HostPath      = "host-path"
  show HostPathQuery = "host-path-query"

instance Read MatchType where
  readsPrec _ s = case s of
    "path-only"       -> [(PathOnly,"")]
    "path-query"      -> [(PathQuery,"")]
    "host-path"       -> [(HostPath,"")]
    "host-path-query" -> [(HostPathQuery,"")]
    _                 -> []

-- | Just optimization, for storing compiled regex in structure
newtype PathRegex = PathRegex {unPathRegex :: Regex}
instance Show PathRegex where show _        = "RewriteRule regex"
instance Read PathRegex where readsPrec _ s = [(PathRegex $ makeRegex s,"")]
-- | Eq and Ord needed for RewritePath which is part of ReverseProxyConfig
instance Eq   PathRegex where _a == _b      = False   -- Should this be implemented?
instance Ord  PathRegex where _a <= _b      = False   -- Should this be implemented?


-- | Info needed for declaring rewrite rule
data RewritePath = RewritePath
    { pathMatchType   :: MatchType    -- ^ which parts of URI are passed for matching
    , pathRegex       :: PathRegex    -- ^ regex for matching specified URI parts
    , pathReplacement :: String       -- ^ new URI
    } deriving (Eq, Ord, Show)

instance FromJSON RewritePath where
  parseJSON = withObject "RewritePath" $ \o -> do
    pathMatchType   <- read <$> o .: "match-type"
    pathRegex       <- read <$> o .: "if-matches"
    pathReplacement <-          o .: "rewrite-to"
    if checkRegexVars pathRegex pathReplacement
      then return RewritePath{..}
      else fail "Incorrect var indexes. 'if-matches' and 'rewrite-to' must match!"

instance ToJSON RewritePath where
    toJSON RewritePath {..} = object
        [ "match-type" .= show pathMatchType
        , "if-matches" .= show pathRegex
        , "rewrite-to" .= pathReplacement
        ]

----------------------------------------------------------------------------------
-- | Functions
----------------------------------------------------------------------------------

-- | Match URI with RewritePath rules and rewrite URL
--   Rules are matched as "first-match-wins"
--
--   Matching types:
--  - "path-only"    - Matched:   only path
--                     Rewritten: only path
--      example: /bar/baz
--            >> /baz/bar
--  - "path-query"   - Matched:   path and query
--                     Rewritten: path and query
--      example: /bar/baz/?query=Today
--            >> /baz/bar/?today=Query
--  - "host-path"    - Matched:   host and path
--                     Rewritten: only path
--                     Host is taken from "reversed-host"
--                     New query is merged with Old query.
--                     New query takes precedence, if equal exists in both.
--                     Queries are reordered alphabetically.
--      example: //sub-dom.foo.com/bar/baz
--            >> //foo.com/baz/bar?a=sub-dom
--  - "host-path-qs" - Matched:   host, path and query
--                     Rewritten: path and query
--                     Host is taken from "reversed-host"
--                     New query is merged with Old query.
--                     New query takes precedence, if equal exists in both.
--                     Queries are reordered alphabetically.
--      example: //sub-dom.foo.com/bar/baz?a=1&b=2&c
--            >> //foo.com/baz/bar?a=sub-dom&b=1&c=
rewritePathRule :: [RewritePath] -> URI -> Maybe URI
rewritePathRule []     _           = Nothing
rewritePathRule (x:xs) uri@URI{..} = regexPath x <|> rewritePathRule xs uri
  where
    regexPath RewritePath{..} =
      case pathMatchType of
        HostPath      -> mergeQueries uri <$> goH  (show uriHostPath)
        HostPathQuery -> mergeQueries uri <$> goH  (show uriHostPathQuery)
        PathQuery     -> mergeQueries uri <$> goPQ (show uriPathQuery)
        PathOnly      ->                      goP        uriPath
      where
        goP sMatch  = (\p -> uri{uriPath = subst uriPath p}) <$> doMatch sMatch
        goH sMatch  = doMatch sMatch >>= parseRelativeReference . subst sMatch
        goPQ sMatch = doMatch sMatch >>= parseRelativeReference . subst sMatch
                                     >>= (\u -> Just u{uriAuthority = uriAuthority})
        doMatch     = matchOnceText (unPathRegex pathRegex)

        uriHostPath        = nullURI    {uriPath  = uriPath, uriAuthority = uriAuthority}
        uriPathQuery       = nullURI    {uriPath  = uriPath, uriQuery = uriQuery}
        uriHostPathQuery   = uriHostPath{                    uriQuery = uriQuery}

        subst sMatch match = T.unpack $ rewrite '$' match sMatch pathReplacement


-- | Merge two queries. Keep NEW query item, and remove OLD query item, if both exist.
mergeQueries :: URI -> URI -> URI
mergeQueries old new = new{uriQuery = BSC.unpack mkQuery}
  where
    mkQuery = renderSimpleQuery True . Map.toList
               $ queryToMap new `Map.union` queryToMap old
    queryToMap = foldr go Map.empty . parseSimpleQuery . BSC.pack . uriQuery
      where
        go (key,value) = Map.insert key value


-- | Check if rewrite rules fall inside regex bopunds
checkRegexVars :: PathRegex -> String -> Bool
checkRegexVars PathRegex{..} s = all (\i -> mn <= i && i <= mx) $ listVars s
  where
    (mn,mx)     = bounds $ regex_groups unPathRegex
    listVars xs = mapMaybe (readMaybe . takeWhile isDigit) $ splitOn "$" xs
