{-# LANGUAGE NoImplicitPrelude #-}
-- | Utilities for dealing with YAML config files which contain relative file
-- paths.
module Data.Yaml.FilePath
    ( decodeFileRelative
    , getFilePath
    , BaseDir
    , ParseYamlFile (..)
    ) where

import Control.Applicative ((<$>))
import Filesystem.Path.CurrentOS (FilePath, encodeString, directory, fromText, (</>))
import Data.Yaml (decodeFileEither, ParseException (AesonException))
import Prelude (($!), ($), Either (..), return, IO, (.))
import Data.Aeson.Types ((.:), Object, Parser, Value, parseEither)
import Data.Text (Text)

-- | The directory from which we're reading the config file.
newtype BaseDir = BaseDir FilePath

-- | Parse a config file, using the 'ParseYamlFile' typeclass.
decodeFileRelative :: ParseYamlFile a
                   => FilePath
                   -> IO (Either ParseException a)
decodeFileRelative fp = do
    evalue <- decodeFileEither $ encodeString fp
    return $! case evalue of
        Left e -> Left e
        Right value ->
            case parseEither (parseYamlFile basedir) value of
                Left s -> Left $! AesonException s
                Right x -> Right $! x
  where
    basedir = BaseDir $ directory fp

-- | A replacement for the @.:@ operator which will both parse a file path and
-- apply the relative file logic.
getFilePath :: BaseDir -> Object -> Text -> Parser FilePath
getFilePath (BaseDir dir) o t = ((dir </>) . fromText) <$> o .: t

-- | A replacement for the standard @FromJSON@ typeclass which can handle relative filepaths.
class ParseYamlFile a where
    parseYamlFile :: BaseDir -> Value -> Parser a
