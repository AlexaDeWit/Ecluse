-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native compact companions and managed info files, each bound to its checked source rows.
module Ecluse.Core.Registry.RubyGems.Mirror (
    rubyMirrorCompanion,
    rubyGemsMirrorDocuments,
    mirrorRow,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Deadline (inspectionDeadline)
import Ecluse.Core.Mirror.Documents (MirrorDocuments (..), restoreFragments)
import Ecluse.Core.Mirror.State (MirrorVersion, recordPackage)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts, pkgPublishedAt))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry (ParseError (parseErrorMessage), PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (MirrorCompanion (..))
import Ecluse.Core.Registry.RubyGems.CompactSpec (compactSpecBytes, validateCompactSpec)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinateFilename, gemFileStem)
import Ecluse.Core.Registry.RubyGems.Request (compactSpecPath, gemPath)
import Ecluse.Core.Registry.RubyGems.SourceState (gemSourcePublishedAt)
import Ecluse.Core.Server.Path (unFilename)

-- | A companion cannot supply authority or substitute another platform's specification.
rubyMirrorCompanion :: MirrorCompanion
rubyMirrorCompanion =
    MirrorCompanion
        { mcLocation = \file source -> do
            let filename = artFilename (expectedArtifact file)
            base <- maybeToRight (PublishSourceUnavailable "RubyGems artifact URL disagrees with its native filename") (T.stripSuffix ("/" <> gemPath filename) source)
            stem <- maybeToRight (PublishSourceUnavailable "RubyGems artifact lacks its native extension") (gemFileStem filename)
            pure (base <> "/" <> compactSpecPath stem)
        , mcValidate = \deadlines limits charge inventory file bytes -> inspectionDeadline deadlines (PublishSourceUnavailable "compact specification agreement timed out") $ case mirrorRow inventory file of
            Left fault -> pure (Left fault)
            Right row -> fmap (first (PublishSourceUnavailable . parseErrorMessage) . fmap compactSpecBytes) (validateCompactSpec limits charge (grCoordinate row) row bytes)
        }

-- | Rows keep record coordinates and versions keep version-text order. A version restores every retained platform row.
rubyGemsMirrorDocuments :: MirrorDocuments
rubyGemsMirrorDocuments =
    MirrorDocuments
        { mdRestore = restoreGemVersion
        , mdOrder = id
        , mdEntryKey = RecordEntry
        , mdRekey = \rekey raw -> do
            native <- snd rubyGemsCached raw
            pure (fst rubyGemsCached native{gemRows = [row{grEntryKey = rekey (grEntryKey row)} | row <- gemRows native]})
        , mdAssemble = \record docs ->
            let documents = [document | doc <- docs, Just raw <- [vdRaw doc], Just document <- [snd rubyGemsCached raw]]
             in fst rubyGemsCached (GemDocument (recordPackage record) (maybe mempty gemHeader (listToMaybe documents)) (concatMap gemRows documents))
        }

-- The first fragment's header and details carry the version, and its publication time covers every restored row.
restoreGemVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreGemVersion mirror current = do
    fragments@((firstDetails, firstSource, _) :| _) <- restoreFragments RecordEntry gemEntries mirror current
    let pairs = concatMap (\(_, _, restored) -> toList restored) fragments
        source = firstSource{gemRows = [row{grEntryKey = artEntryKey artifact} | (artifact, row) <- pairs]}
    artifacts <- nonEmpty (map fst pairs)
    pure (VersionDoc firstDetails{pkgArtifacts = artifacts, pkgPublishedAt = gemSourcePublishedAt source} (Just (fst rubyGemsCached source)))
  where
    gemEntries = \case
        RubyGemsMirrorRead source -> Just (source, Map.fromList [(grEntryKey row, row) | row <- gemRows source])
        _ -> Nothing

-- | Require exactly one original source row for the expected physical member.
mirrorRow :: MirrorInventory -> ExpectedMirrorFile -> Either PublishFault GemRow
mirrorRow inventory file = case inventoryReadData inventory of
    RubyGemsMirrorRead source -> case filter ((== artEntryKey (expectedArtifact file)) . grEntryKey) (gemRows source) of
        [row] | unFilename (coordinateFilename (grCoordinate row)) == artFilename (expectedArtifact file) -> Right row
        _ -> Left (PublishSourceUnavailable "RubyGems upload has no unique paired source row")
    _ -> Left (PublishSourceUnavailable "RubyGems upload has no native source document")
