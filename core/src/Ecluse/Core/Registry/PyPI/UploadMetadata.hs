-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Checked per-distribution Core Metadata for the PyPI upload protocol.
module Ecluse.Core.Registry.PyPI.UploadMetadata (
    PyPIUploadMetadata,
    parseUploadMetadata,
    uploadMetadataMemoryBytes,
    uploadFields,
) where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..), parseRefusal)
import Ecluse.Core.Registry.PyPI.Project (projectName)
import Ecluse.Core.Version (Version, canonicalPep440)

-- | Ordered upload fields, preserving repeated values and author spelling.
newtype PyPIUploadMetadata = PyPIUploadMetadata [(Text, Text)]
    deriving stock (Eq, Show)

-- | Reject malformed headers, ambiguous single-use fields, and inconsistent package identity.
parseUploadMetadata :: Int -> PackageName -> Version -> ByteString -> Either ParseError PyPIUploadMetadata
parseUploadMetadata cap package version bytes = do
    when (cap <= 0 || BS.length bytes > cap) (parseRefusal "Core Metadata exceeds its byte bound")
    document <- first (const (ParseError "Core Metadata is not UTF-8")) (TE.decodeUtf8' bytes)
    when (T.any invalidControl document) (parseRefusal "Core Metadata contains a control character")
    fields <- parseHeaders document
    checked <- traverse projectField fields
    let counts = Map.fromListWith (+) [(name, 1 :: Int) | (name, _) <- fields]
    for_ (Map.toList counts) $ \(name, count) ->
        when (count > 1 && name `notElem` repeatedFields) (parseRefusal ("repeated single-use Core Metadata field: " <> name))
    format <- required "metadata-version" fields
    unless (format `elem` ["1.0", "1.1", "1.2", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6"]) $
        parseRefusal "unsupported Core Metadata version"
    declaredName <- required "name" fields >>= projectName
    unless (declaredName == package) (parseRefusal "Core Metadata names another package")
    declaredVersion <- required "version" fields
    unless (canonicalPep440 declaredVersion == Just version) (parseRefusal "Core Metadata names another version")
    for_ [value | ("dynamic", value) <- fields] $ \value ->
        when (T.toLower value `elem` ["name", "version", "metadata-version", "dynamic"] || isNothing (fieldName (T.toLower value))) $
            parseRefusal "invalid Dynamic declaration"
    pure (PyPIUploadMetadata checked)

-- | Charge the actual raw, decoded, and encoded metadata plus bounded header structures before preparation.
uploadMetadataMemoryBytes :: ByteString -> Either ParseError Int
uploadMetadataMemoryBytes bytes
    | wanted > toInteger (maxBound :: Int) = parseRefusal "Core Metadata memory charge exceeds the platform bound"
    | otherwise = Right (fromInteger wanted)
  where
    wanted = 1024 + 6 * toInteger (BS.length bytes) + 512 * toInteger (headerCount 0 bytes)
    headerCount seen remaining
        | BS.null remaining || seen >= (65537 :: Int) = seen
        | otherwise =
            let (line, suffix) = BS.break (== 10) remaining
             in if line == "" || line == "\r" then seen else headerCount (seen + 1) (BS.drop 1 suffix)

-- | Form names follow the native upload API, including its pluralisation exceptions.
uploadFields :: PyPIUploadMetadata -> [(Text, Text)]
uploadFields (PyPIUploadMetadata fields) = fields

invalidControl :: Char -> Bool
invalidControl character = (character < ' ' && character /= '\n' && character /= '\r' && character /= '\t') || character == '\DEL'

parseHeaders :: Text -> Either ParseError [(Text, Text)]
parseHeaders = go 0 "" []
  where
    go count previousEnding held remaining
        | T.null remaining = Right (finishFields held)
        | otherwise =
            let (rawLine, suffix) = T.breakOn "\n" remaining
                rest = T.drop 1 suffix
                (line, ending) = maybe (rawLine, "\n") (,"\r\n") (T.stripSuffix "\r" rawLine)
             in if rawLine `elem` ["", "\r"]
                    then finish held rest
                    else do
                        when (count >= (65536 :: Int)) (parseRefusal "Core Metadata header count exceeds its bound")
                        when (T.compareLength rawLine 16384 == GT) (parseRefusal "Core Metadata header line exceeds its bound")
                        when (T.null suffix && ending == "\r\n") (parseRefusal "bare carriage return in Core Metadata")
                        addLine (count + 1) previousEnding ending held line rest
    finish held body
        | T.null body = Right (finishFields held)
        | any ((== "description") . fst) held = parseRefusal "description appears in both header and body"
        | otherwise = Right (finishFields held <> [("description", body)])
    finishFields = map finishField . reverse
    finishField (name, pieces) =
        let value = T.concat (reverse pieces)
         in (name, if name == "description" then T.replace "\n       |" "\n" (T.replace "\r\n       |" "\r\n" value) else value)
    addLine count previousEnding nextEnding held line remaining
        | T.any (== '\r') line = parseRefusal "bare carriage return in Core Metadata"
        | T.isPrefixOf " " line || T.isPrefixOf "\t" line = case held of
            [] -> parseRefusal "Core Metadata starts with a continuation"
            (name, pieces) : rest -> go count nextEnding ((name, (previousEnding <> line) : pieces) : rest) remaining
        | otherwise = do
            let (name, suffix) = T.breakOn ":" line
            when (T.null name || T.null suffix || not (T.all headerChar name)) (parseRefusal "malformed Core Metadata header")
            let value = T.dropWhile (`elem` [' ', '\t']) (T.drop 1 suffix)
            go count nextEnding ((T.toLower name, [value]) : held) remaining

headerChar :: Char -> Bool
headerChar character = isAsciiLower character || isAsciiUpper character || isDigit character || character == '-'

required :: Text -> [(Text, Text)] -> Either ParseError Text
required name fields = case [value | (key, value) <- fields, key == name] of
    [value] | not (T.null value) -> Right value
    _ -> parseRefusal ("missing or ambiguous Core Metadata field: " <> name)

projectField :: (Text, Text) -> Either ParseError (Text, Text)
projectField (name, value) = case fieldName name of
    Nothing -> parseRefusal ("unsupported Core Metadata field: " <> name)
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
