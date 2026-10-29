-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Durable native rows preserve source facts independently of target publication metadata.
module Ecluse.Core.Registry.RubyGems.SourceState (
    gemSourceValue,
    parseGemSource,
    validateGemSource,
    gemSourcePublishedAt,
    gemSourceDecodeCharge,
) where

import Data.Aeson (Value (..), object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, withObject)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Registry.WireSupport (newestUpload)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (renderVersion)

-- | Store exact source bytes and physical positions in a versioned native payload.
gemSourceValue :: GemDocument -> Value
gemSourceValue document =
    object
        [ "kind" .= ("rubygems" :: Text)
        , "format" .= (1 :: Int)
        , "name" .= renderPackageName (gemName document)
        , "header" .= encodeBytes (gemHeader document)
        , "entries" .= [object ["position" .= position row, "source" .= encodeBytes (grSource row)] | row <- gemRows document]
        ]
  where
    position row = case grEntryKey row of
        RecordEntry value -> value
        _ -> -1

-- | Reparse native fields rather than trusting independently stored copies of policy facts.
parseGemSource :: Value -> Parser GemDocument
parseGemSource = withObject "RubyGems source" $ \fields -> do
    format <- fields .: "format"
    unless (format == (1 :: Int)) (fail "unsupported RubyGems source format")
    name <- fields .: "name" >>= either (fail . toString . parseErrorMessage) pure . projectName
    header <- fields .: "header" >>= decodeBytes
    rows <-
        fields .: "entries"
            >>= traverse
                ( withObject "RubyGems source row" $ \entry -> do
                    position <- entry .: "position"
                    when (position < 0) (fail "invalid RubyGems source position")
                    bytes <- entry .: "source" >>= decodeBytes
                    either (fail . show) pure (decodeInfoRow name (RecordEntry position) bytes)
                )
    let keys = map grEntryKey rows
    unless (Set.size (Set.fromList keys) == length keys) (fail "duplicate RubyGems source position")
    pure (GemDocument name header rows)

-- | Require each retained row to agree with its admitted artifact and exact native identity.
validateGemSource :: PackageDetails -> GemDocument -> Either Text ()
validateGemSource details document = do
    unless (gemName document == pkgName details) (Left "RubyGems source names another package")
    let entries = Map.fromList [(grEntryKey row, row) | row <- gemRows document]
        expected = Set.fromList (map artEntryKey (toList (pkgArtifacts details)))
    unless (all nativeKey (Map.keys entries)) (Left "RubyGems source position is not a physical record")
    unless (Map.keysSet entries == expected && Map.size entries == length (gemRows document) && Set.size expected == length (pkgArtifacts details)) (Left "RubyGems source membership disagrees with its artifacts")
    unless (pkgInstallCode details == CodeExecUnknown && pkgAvailability details == Available) (Left "RubyGems source has inconsistent policy facts")
    for_ (pkgArtifacts details) $ \artifact -> do
        row <- maybeToRight "missing RubyGems source row" (Map.lookup (artEntryKey artifact) entries)
        reparsed <- first show (decodeInfoRow (gemName document) (grEntryKey row) (grSource row))
        unless (reparsed == row) (Left "RubyGems retained row differs from its source bytes")
        case (grPublishedAt row, pkgPublishedAt details) of
            (Nothing, Just _) -> Left "RubyGems unknown source time became a known admission time"
            (Just published, Just admitted) | published > admitted -> Left "RubyGems source upload is newer than its admitted version"
            _ -> pure ()
        let coordinate = grCoordinate row
        unless (coordinateName coordinate == pkgName details && renderVersion (coordinateVersion coordinate) == renderVersion (pkgVersion details)) (Left "RubyGems source identity disagrees with its admitted version")
        unless (unFilename (coordinateFilename coordinate) == artFilename artifact && artHashes artifact == [grChecksum row] && isNothing (artCoreMetadata artifact) && isNothing (artSize artifact)) (Left "RubyGems artifact disagrees with its native source")

encodeBytes :: ByteString -> Text
encodeBytes bytes = decodeUtf8 (convertToBase Base64 bytes :: ByteString)

decodeBytes :: Text -> Parser ByteString
decodeBytes = either fail pure . sourceBytes

sourceBytes :: Text -> Either String ByteString
sourceBytes encoded = convertFromBase Base64 (encodeUtf8 encoded :: ByteString)

nativeKey :: EntryKey -> Bool
nativeKey (RecordEntry position) = position >= 0
nativeKey _ = False

-- | Any unknown platform time leaves the complete version's source age unknown.
gemSourcePublishedAt :: GemDocument -> Maybe UTCTime
gemSourcePublishedAt document = case gemRows document of
    [] -> Nothing
    firstRow : rest -> foldl' (\previous row -> newestUpload previous (grPublishedAt row)) (grPublishedAt firstRow) rest

-- | Native reparsing pays the same row workspace allowance as a live source read.
gemSourceDecodeCharge :: Value -> Integer
gemSourceDecodeCharge (Object fields) = case KeyMap.lookup "entries" fields of
    Just (Array entries) -> sum (map entryCharge (toList entries))
    _ -> 0
  where
    entryCharge (Object entry) = case KeyMap.lookup "source" entry of
        Just (String encoded) -> either (const 0) (toInteger . decodedDocumentCharge) (sourceBytes encoded)
        _ -> 0
    entryCharge _ = 0
gemSourceDecodeCharge _ = 0
