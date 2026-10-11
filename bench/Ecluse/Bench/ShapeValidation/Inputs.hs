-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Frozen full-read cases for the three-revision CI experiment.
module Ecluse.Bench.ShapeValidation.Inputs (
    InputCase (..),
    inputCases,
    generateInputs,
    readInput,
    inputFacts,
    preservesProjection,
) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertFailure)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Test.Corpus (CaptureRecord (..), CorpusPackage (..), CorpusTier (Large), corpusPackages, cpName, pypiCorpusPackages, readCaptureRecords)
import Ecluse.Test.Corpus.PyPI (benchProject, syntheticIndexBytes)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Security.Limits (checkNestingDepth)
import Ecluse.Test.Support (expectRight)

data Shape = Original | Generated Int | LongText | Escaped | IgnoredScalar | IgnoredObjects
    deriving stock (Eq, Show)

-- | Each label selects one input and package, so replay paths cannot collide.
data InputCase = InputCase
    { inputLabel :: Text
    , inputEcosystem :: Ecosystem
    , inputPackage :: CorpusPackage
    , inputShape :: Shape
    }

-- | The fixed 16-case design. Generated counts denote releases, with two files per release.
inputCases :: [InputCase]
inputCases =
    [make PyPI package Original | package <- pypiCorpusPackages]
        <> [InputCase ("pypi-generated-" <> show count) PyPI synthetic (Generated count) | count <- generatedCounts]
        <> [make PyPI package shape | shape <- [LongText, Escaped, IgnoredScalar, IgnoredObjects], package <- largePyPI]
        <> [make Npm package Original | package <- corpusPackages, cpName package `elem` npmNames]
  where
    synthetic = CorpusPackage benchProject "" Large 1
    largePyPI = filter ((`elem` largePyPINames) . cpName) pypiCorpusPackages
    make ecosystem package shape = InputCase (ecosystemName ecosystem <> "-" <> T.replace "/" "_" (T.replace "@" "" (cpName package)) <> "-" <> shapeLabel shape) ecosystem package shape

shapeLabel :: Shape -> Text
shapeLabel = \case
    Original -> "original"
    Generated count -> "generated-" <> show count
    LongText -> "long-retained"
    Escaped -> "escaped"
    IgnoredScalar -> "ignored-scalar"
    IgnoredObjects -> "ignored-objects"

-- | Materialise cases once, after checking committed capture provenance.
generateInputs :: FilePath -> IO ()
generateInputs directory = do
    assertBool "case count differs from the fixed design" (length inputCases == expectedCaseCount)
    assertBool "case labels are not unique" (Set.size (Set.fromList (map inputLabel inputCases)) == expectedCaseCount)
    createDirectoryIfMissing True directory
    records <- forM inputCases $ \input -> do
        raw <- sourceBytes input
        value <- expectRight (eitherDecodeStrict raw)
        changed <- transform (inputShape input) value
        let bytes = case inputShape input of
                Original -> raw
                Generated _ -> raw
                Escaped -> escapedJson value
                _ -> toStrict (encode changed)
        checked <- validateInput input bytes
        when (inputShape input == Escaped) (assertBool "escaping changed the JSON value" (checked == value))
        BS.writeFile (inputPath directory input) bytes
        pure $ object ["case" .= inputLabel input, "ecosystem" .= ecosystemName (inputEcosystem input), "package" .= cpName (inputPackage input), "sourcePath" .= cpPath (inputPackage input), "sourceSha256" .= hexSha256Of raw, "transformation" .= shapeLabel (inputShape input), "facts" .= inputFacts bytes checked]
    BS.writeFile (directory </> "inputs.json") (toStrict (encode records))
    writeFileText (directory </> "cases.txt") (T.unlines (map inputLabel inputCases))

sourceBytes :: InputCase -> IO ByteString
sourceBytes input = case inputShape input of
    Generated count -> pure (syntheticIndexBytes count)
    _ -> do
        raw <- BS.readFile (cpPath (inputPackage input))
        records <- readCaptureRecords (inputEcosystem input) >>= expectRight
        record <- maybe (assertFailure "capture has no provenance record") pure (Map.lookup (cpName (inputPackage input)) records)
        assertBool "capture byte count differs from its pin" (fromIntegral (BS.length raw) == crBytes record)
        assertBool "capture SHA-256 differs from its pin" (hexSha256Of raw == crSha256 record)
        pure raw

-- | Load a generated case and enforce production limits before reading it.
readInput :: FilePath -> InputCase -> IO ByteString
readInput directory input = do
    bytes <- BS.readFile (inputPath directory input)
    _ <- validateInput input bytes
    pure bytes

inputPath :: FilePath -> InputCase -> FilePath
inputPath directory input = directory </> toString (inputLabel input) <> ".json"

validateInput :: InputCase -> ByteString -> IO Value
validateInput input bytes = do
    assertBool "fixture exceeds the production body limit" (BS.length bytes <= maxMetadataBytes defaultLimits)
    value <- expectRight (eitherDecodeStrict bytes)
    _ <- expectRight (checkNestingDepth defaultLimits value)
    assertBool "fixture exceeds the production artifact limit" (fileCount value <= maxArtifactCount defaultLimits)
    assertBool "fixture exceeds the production version limit" (versionCount value <= maxVersionCount defaultLimits)
    case inputShape input of
        Generated count -> assertBool "generated file count differs" (fileCount value == filesPerRelease * count)
        _ -> pass
    pure value

-- | Record wire size and shape without treating allocation as residency.
inputFacts :: ByteString -> Value -> Value
inputFacts bytes value =
    object
        [ "bytes" .= BS.length bytes
        , "sha256" .= hexSha256Of bytes
        , "fileEntries" .= fileCount value
        , "declaredVersions" .= versionCount value
        , "maxStringBytes" .= maximumString value
        , "backslashBytes" .= BS.count backslashByte bytes
        ]

fileCount :: Value -> Int
fileCount (Object fields) = case KeyMap.lookup "files" fields of
    Just (Array files) -> V.length files
    _ -> 0
fileCount _ = 0

versionCount :: Value -> Int
versionCount (Object fields) = case KeyMap.lookup "versions" fields of
    Just (Object versions) -> KeyMap.size versions
    Just (Array versions) -> V.length versions
    _ -> 0
versionCount _ = 0

maximumString :: Value -> Int
maximumString = \case
    String text -> BS.length (encodeUtf8 text)
    Object fields -> foldl' max 0 (map maximumString (KeyMap.elems fields))
    Array values -> V.foldl' (\largest value -> max largest (maximumString value)) 0 values
    _ -> 0

-- | Escapes and ignored fields preserve the original typed and served projection.
preservesProjection :: InputCase -> Bool
preservesProjection input = inputShape input `elem` [Escaped, IgnoredScalar, IgnoredObjects]

transform :: Shape -> Value -> IO Value
transform shape original@(Object fields) = case shape of
    LongText -> case KeyMap.lookup "project-status" fields of
        Just (Object status) -> pure (Object (KeyMap.insert "project-status" (Object (KeyMap.insert "reason" (String (plainText longFieldBytes)) status)) fields))
        _ -> assertFailure "long-text source lacks project-status"
    IgnoredScalar -> padIgnored original original
    IgnoredObjects -> case KeyMap.lookup "files" fields of
        Just (Array files) -> do
            changed <- traverse addIgnored files
            padIgnored original (Object (KeyMap.insert "files" (Array changed) fields))
        _ -> assertFailure "structured source lacks files"
    _ -> pure original
transform _ _ = assertFailure "fixture root is not an object"

addIgnored :: Value -> IO Value
addIgnored (Object fields) = pure (Object (KeyMap.insert ignoredKey ignoredObject fields))
addIgnored _ = assertFailure "fixture file is not an object"

padIgnored :: Value -> Value -> IO Value
padIgnored original (Object fields) = do
    let unpadded = Object (KeyMap.insert paddingKey (String "") fields)
        target = BS.length (toStrict (encode original)) + ignoredAddedBytes
        padding = target - BS.length (toStrict (encode unpadded))
    assertBool "ignored objects exceed their fixed wire budget" (padding >= 0)
    pure (Object (KeyMap.insert paddingKey (String (plainText padding)) fields))
padIgnored _ _ = assertFailure "padding source is not an object"

plainText :: Int -> Text
plainText size = T.take size (T.replicate (size `div` T.length plainAlphabet + 1) plainAlphabet)

escapedJson :: Value -> ByteString
escapedJson = \case
    String text -> escapedString text
    Object fields -> "{" <> BS.intercalate "," [escapedString (Key.toText key) <> ":" <> escapedJson value | (key, value) <- KeyMap.toList fields] <> "}"
    Array values -> "[" <> BS.intercalate "," (map escapedJson (toList values)) <> "]"
    other -> toStrict (encode other)

escapedString :: Text -> ByteString
escapedString text = "\"" <> BS.concat (zipWith character [0 :: Int ..] (T.unpack text)) <> "\""
  where
    character position char
        | position `mod` escapeStride == 0
        , ord char <= unicodeBmpEnd =
            let digits = T.pack (showHex (ord char) "")
             in "\\u" <> encodeUtf8 (T.justifyRight unicodeHexWidth '0' digits)
        | otherwise =
            let encoded = toStrict (encode (T.singleton char))
             in BS.take (BS.length encoded - jsonQuotesBytes) (BS.drop jsonQuoteBytes encoded)

generatedCounts :: [Int]
generatedCounts = [4096, 8192]

largePyPINames, npmNames :: [Text]
largePyPINames = ["boto3", "numpy"]
npmNames = ["typescript", "react", "@types/node"]

longFieldBytes, ignoredAddedBytes, filesPerRelease, escapeStride, unicodeHexWidth, unicodeBmpEnd, jsonQuoteBytes, jsonQuotesBytes, expectedCaseCount :: Int
longFieldBytes = 131072
ignoredAddedBytes = 1048576
filesPerRelease = 2
escapeStride = 8
unicodeHexWidth = 4
unicodeBmpEnd = 65535
jsonQuoteBytes = 1
jsonQuotesBytes = 2
expectedCaseCount = 16

backslashByte :: Word8
backslashByte = 92

plainAlphabet :: Text
plainAlphabet = "abcdefghijklmnopqrstuvwxyz0123456789"

ignoredKey, paddingKey :: Key.Key
ignoredKey = "benchmark-extra"
paddingKey = "benchmark-padding"

ignoredObject :: Value
ignoredObject = object ["values" .= [Null, Bool True, Bool False, Number 0, String "sample"], "nested" .= object ["leaf" .= ("ignored" :: Text)]]
