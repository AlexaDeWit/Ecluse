-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Checked per-distribution Core Metadata for the PyPI upload protocol.
module Ecluse.Core.Registry.PyPI.UploadMetadata (
    PyPIUploadMetadata,
    parseUploadMetadata,
    uploadFields,
) where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Project (projectName)
import Ecluse.Core.Version (Version, canonicalPep440)

-- | Ordered upload fields, preserving repeated values and author spelling.
newtype PyPIUploadMetadata = PyPIUploadMetadata [(Text, Text)]
    deriving stock (Eq, Show)

-- | Reject malformed headers, ambiguous single-use fields, and inconsistent package identity.
parseUploadMetadata :: Int -> PackageName -> Version -> ByteString -> Either ParseError PyPIUploadMetadata
parseUploadMetadata cap package version bytes = do
    when (cap <= 0 || BS.length bytes > cap) (bad "Core Metadata exceeds its byte bound")
    document <- first (const (ParseError "Core Metadata is not UTF-8")) (TE.decodeUtf8' bytes)
    when (T.any invalidControl document) (bad "Core Metadata contains a control character")
    fields <- parseHeaders (T.splitOn "\n" document)
    checked <- traverse projectField fields
    let counts = Map.fromListWith (+) [(name, 1 :: Int) | (name, _) <- fields]
    for_ (Map.toList counts) $ \(name, count) ->
        when (count > 1 && name `notElem` repeatedFields) (bad ("repeated single-use Core Metadata field: " <> name))
    format <- required "metadata-version" fields
    unless (format `elem` ["1.0", "1.1", "1.2", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6"]) $
        bad "unsupported Core Metadata version"
    declaredName <- required "name" fields >>= projectName
    unless (declaredName == package) (bad "Core Metadata names another package")
    declaredVersion <- required "version" fields
    unless (canonicalPep440 declaredVersion == Just version) (bad "Core Metadata names another version")
    for_ [value | ("dynamic", value) <- fields] $ \value ->
        when (T.toLower value `elem` ["name", "version", "metadata-version", "dynamic"] || isNothing (fieldName (T.toLower value))) $
            bad "invalid Dynamic declaration"
    pure (PyPIUploadMetadata checked)

-- | Form names follow the native upload API, including its pluralisation exceptions.
uploadFields :: PyPIUploadMetadata -> [(Text, Text)]
uploadFields (PyPIUploadMetadata fields) = fields

invalidControl :: Char -> Bool
invalidControl character = (character < ' ' && character /= '\n' && character /= '\r' && character /= '\t') || character == '\DEL'

parseHeaders :: [Text] -> Either ParseError [(Text, Text)]
parseHeaders = go 0 []
  where
    go _ held [] = Right (finishFields held)
    go count held (rawLine : remaining)
        | rawLine `elem` ["", "\r"] = finish held remaining
        | count >= (65536 :: Int) = bad "Core Metadata header count exceeds its bound"
        | T.length rawLine > 16384 = bad "Core Metadata header line exceeds its bound"
        | otherwise = addLine (count + 1) held (fromMaybe rawLine (T.stripSuffix "\r" rawLine)) remaining
    finish held body
        | null body || body == [""] = Right (finishFields held)
        | any ((== "description") . fst) held = bad "description appears in both header and body"
        | otherwise = Right (finishFields held <> [("description", T.intercalate "\n" body)])
    finishFields = map (second (T.intercalate "\n" . reverse)) . reverse
    addLine count held line remaining
        | T.isPrefixOf " " line || T.isPrefixOf "\t" line = case held of
            [] -> bad "Core Metadata starts with a continuation"
            (name, pieces) : rest -> go count ((name, line : pieces) : rest) remaining
        | otherwise = do
            let (name, suffix) = T.breakOn ":" line
            when (T.null name || T.null suffix || not (T.all headerChar name)) (bad "malformed Core Metadata header")
            let value = T.dropWhile (`elem` [' ', '\t']) (T.drop 1 suffix)
            when (T.any (== '\r') value) (bad "bare carriage return in Core Metadata")
            go count ((T.toLower name, [value]) : held) remaining

headerChar :: Char -> Bool
headerChar character = isAsciiLower character || isAsciiUpper character || isDigit character || character == '-'

required :: Text -> [(Text, Text)] -> Either ParseError Text
required name fields = case [value | (key, value) <- fields, key == name] of
    [value] | not (T.null value) -> Right value
    _ -> bad ("missing or ambiguous Core Metadata field: " <> name)

projectField :: (Text, Text) -> Either ParseError (Text, Text)
projectField (name, value) = case fieldName name of
    Nothing -> bad ("unsupported Core Metadata field: " <> name)
    Just target -> Right (target, value)

fieldName :: Text -> Maybe Text
fieldName name
    | name == "classifier" = Just "classifiers"
    | name == "project-url" = Just "project_urls"
    | name `elem` singleFields || name `elem` repeatedFields = Just (T.replace "-" "_" name)
    | otherwise = Nothing

singleFields :: [Text]
singleFields =
    [ "metadata-version"
    , "name"
    , "version"
    , "summary"
    , "description"
    , "description-content-type"
    , "keywords"
    , "home-page"
    , "download-url"
    , "author"
    , "author-email"
    , "maintainer"
    , "maintainer-email"
    , "license"
    , "license-expression"
    , "requires-python"
    ]

repeatedFields :: [Text]
repeatedFields =
    [ "platform"
    , "supported-platform"
    , "classifier"
    , "requires"
    , "provides"
    , "obsoletes"
    , "requires-dist"
    , "provides-dist"
    , "obsoletes-dist"
    , "requires-external"
    , "project-url"
    , "provides-extra"
    , "dynamic"
    , "license-file"
    , "import-name"
    , "import-namespace"
    ]

bad :: Text -> Either ParseError a
bad = Left . ParseError
