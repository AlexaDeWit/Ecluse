-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native corpus transforms preserve Compact Index headers and complete platform rows.
module Ecluse.Test.Corpus.RubyGems (
    benchGem,
    syntheticInfoBytes,
    rubyGemsVersionKeys,
    decodeRubyGemsValues,
    encodeRubyGemsValues,
    keepRubyGemsVersions,
    heavyRubyGemsBase,
    projectRubyGemsTree,
    projectRubyGemsValues,
    projectRubyGemsInfo,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (Artifact (..), Availability (Available), CodeExecSignal (CodeExecUnknown), DependencySources (RegistryDependencies), HashAlg (SHA256), PackageDetails (..), PackageInfo (..), PackageName, mkHash, mkPackageName, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, rubyGemsCached)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps)
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Metadata (finishGemFull)
import Ecluse.Core.Registry.RubyGems.Project (mkCoordinate, mkGemPlatform)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (FullRead), gemInfoSteps)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes), checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Registry.JsonStream (readStepsChunks)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)

-- | Synthetic releases use a distinct package name, separate from captured histories.
benchGem :: PackageName
benchGem = mkPackageName RubyGems Nothing "bench-gem"

-- | Keep raw release identifiers in encounter order, grouping their platform artifacts.
rubyGemsVersionKeys :: ByteString -> Either Text [Text]
rubyGemsVersionKeys = fmap (ordNub . map rowVersion) . referenceInfo

-- | The internal tree retains original row bytes and independently read version keys.
decodeRubyGemsValues :: ByteString -> Either Text Value
decodeRubyGemsValues bytes = do
    input <- first show (Text.decodeUtf8' bytes)
    rows <- referenceInfo bytes
    let physical = Map.fromList (zip [0 ..] (T.lines input))
    unless (T.isSuffixOf "\n" input) (Left "incomplete RubyGems corpus framing")
    sources <- forM rows $ \row -> do
        source <- maybeToRight "RubyGems corpus row position is absent" (Map.lookup (rowPosition row) physical)
        pure (object ["version" .= rowVersion row, "source" .= source])
    pure (object ["header" .= corpusHeader input, "rows" .= sources])

-- | Emit native records without exposing the internal JSON envelope on the wire.
encodeRubyGemsValues :: Value -> Either Text ByteString
encodeRubyGemsValues (Object document) = do
    header <- textField "header" document
    rows <- case KeyMap.lookup "rows" document of
        Just (Array values) -> traverse row (toList values)
        _ -> Left "RubyGems corpus transform requires rows"
    let bytes = encodeUtf8 (header <> foldMap (<> "\n") rows)
    _ <- referenceInfo bytes
    pure bytes
  where
    row (Object fields) = textField "source" fields
    row _ = Left "RubyGems corpus transform contains a non-object row"
encodeRubyGemsValues _ = Left "RubyGems corpus transform requires a document"

-- | Retain every platform artifact of each selected raw version in source order.
keepRubyGemsVersions :: Set Text -> Value -> Value
keepRubyGemsVersions kept = onRows (filter selected)
  where
    selected (Object fields) = case KeyMap.lookup "version" fields of
        Just (String version) -> version `Set.member` kept
        _ -> False
    selected _ = False

-- | Add an opaque native field to measure large private metadata without changing release facts.
heavyRubyGemsBase :: Int -> Value -> Value
heavyRubyGemsBase size value = onRows (map (enlarge width)) value
  where
    width = max 1 (size `div` max 1 (rowCount value))
    enlarge count (Object fields) = Object (KeyMap.mapWithKey (\key original -> if key == "source" then extend original else original) fields)
      where
        extend (String source) = String (source <> ",ecluse_benchmark:" <> T.replicate count "x")
        extend other = other
    enlarge _ other = other

-- | Positive counts produce complete native records with independent release keys and checksums.
syntheticInfoBytes :: Int -> ByteString
syntheticInfoBytes count = encodeUtf8 ("---\n" <> foldMap record [0 .. count - 1])
  where
    record index = "1.0." <> show index <> " benchmark-dependency:>= 0|checksum:" <> T.replicate 64 "0" <> ",created_at:2020-01-01T00:00:00Z\n"

textField :: Key -> KeyMap.KeyMap Value -> Either Text Text
textField key fields = case KeyMap.lookup key fields of
    Just (String value) -> Right value
    _ -> Left "RubyGems corpus transform requires a text field"

onRows :: ([Value] -> [Value]) -> Value -> Value
onRows change (Object fields) = Object (KeyMap.mapWithKey (\key original -> if key == "rows" then transform original else original) fields)
  where
    transform (Array rows) = toJSON (change (toList rows))
    transform other = other
onRows _ other = other

rowCount :: Value -> Int
rowCount (Object fields) = case KeyMap.lookup "rows" fields of
    Just (Array rows) -> length rows
    _ -> 0
rowCount _ = 0

{- | The held-tree oracle: reference rows, with archive filenames and grouped age derived here.
Production constructors build only the coordinate, platform and hash values the document holds.
-}
projectRubyGemsTree :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectRubyGemsTree limits name bytes = do
    input <- first (const MetadataUndecodable) (Text.decodeUtf8' bytes)
    native <- first (const MetadataUndecodable) (referenceInfo bytes)
    rows <- traverse (projectRow (Map.fromList (zip [0 ..] (T.lines input)))) native
    first MetadataBoundExceeded (checkArtifactCount limits (length rows))
    unless (Set.size (Set.fromList [filename | (_, filename, _) <- rows]) == length rows) (Left MetadataUndecodable)
    let versions = foldl' insert Map.empty rows
    first MetadataBoundExceeded (checkVersionCountOf limits (Map.size versions))
    pure (PackageInfo name (Map.mapWithKey settle versions) Map.empty [], fst rubyGemsCached (GemDocument name (encodeUtf8 (corpusHeader input)) [row | (_, _, row) <- rows]))
  where
    projectRow physical observed = do
        platform <- first (const MetadataUndecodable) (mkGemPlatform (rowPlatform observed))
        coordinate <- first (const MetadataUndecodable) (mkCoordinate name (mkVersion RubyGems (rowVersion observed)) platform)
        let fields = Map.fromList [(T.copy key, T.copy value) | (key, value) <- rowFields observed]
        rawChecksum <- maybeToRight MetadataUndecodable (Map.lookup "checksum" fields)
        checksum <- first (const MetadataUndecodable) (mkHash SHA256 rawChecksum)
        dependencies <- forM (rowDependencies observed) $ \(dependency, requirements) -> do
            clauses <- maybeToRight MetadataUndecodable (nonEmpty (map T.copy requirements))
            pure (GemDependency (mkPackageName RubyGems Nothing dependency) clauses)
        ruby <- requirement "ruby" fields
        rubygems <- requirement "rubygems" fields
        source <- maybeToRight MetadataUndecodable (Map.lookup (rowPosition observed) physical)
        let published = Map.lookup "created_at" fields >>= iso8601ParseM . toString
        pure (rowVersion observed, archiveFilename observed, GemRow (RecordEntry (rowPosition observed)) coordinate dependencies ruby rubygems checksum published fields (BS.copy (encodeUtf8 source)))
    requirement field fields = maybe (Right (">= 0" :| [])) (maybeToRight MetadataUndecodable . nonEmpty . map T.copy . T.splitOn "&") (Map.lookup field fields)
    -- The native archive name: the gem name, its raw version and any platform other than ruby.
    archiveFilename observed = renderPackageName name <> "-" <> rowVersion observed <> (if rowPlatform observed == "ruby" then "" else "-" <> rowPlatform observed) <> ".gem"
    insert versions (version, filename, row) =
        let artifact = Artifact (grEntryKey row) filename ("https://rubygems.org/gems/" <> filename) [grChecksum row] Nothing Nothing
         in Map.insertWith (flip (<>)) version ((artifact, grPublishedAt row) :| []) versions
    settle version artifacts = PackageDetails name (mkVersion RubyGems version) (groupedAge artifacts) CodeExecUnknown RegistryDependencies Available (fmap fst artifacts)
    -- The newest upload, or no age once any artifact's time is unknown.
    groupedAge ((_, firstTime) :| rest) = foldl' (\newest (_, time) -> max <$> newest <*> time) firstTime rest

-- | Legacy residency keeps its internal tree while reading native rows through the independent oracle.
projectRubyGemsValues :: Limits -> PackageName -> Value -> Either MetadataError (PackageInfo, Value)
projectRubyGemsValues limits name value = do
    bytes <- first (const MetadataUndecodable) (encodeRubyGemsValues value)
    (info, _) <- projectRubyGemsTree limits name bytes
    pure (info, value)

-- | Pure corpus reads use the production native steps and the shared complete-body driver.
projectRubyGemsInfo :: Text -> Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectRubyGemsInfo base limits name bytes = do
    streamed <- first MetadataBoundExceeded (readStepsChunks finishRecordSteps (MetadataBodyLimit (maxMetadataBytes limits)) (gemInfoSteps (const (pure ())) limits name base FullRead) [bytes])
    finishGemFull limits name base streamed

corpusHeader :: Text -> Text
corpusHeader input = T.unlines (takeWhile (/= "---") (T.lines input) <> ["---"])
