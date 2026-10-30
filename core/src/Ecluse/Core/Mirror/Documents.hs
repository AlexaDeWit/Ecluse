-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A durable mirror format's operations over its managed documents.
Each adapter declares them, so the mirror view restores and assembles retained versions without naming a format.
-}
module Ecluse.Core.Mirror.Documents (
    MirrorDocuments (..),
    restoreFragments,
) where

import Data.Map.Strict qualified as Map
import Data.Traversable (for)

import Ecluse.Core.Mirror.State (MirrorVersion, ProjectMirrorRecord, fragmentDetails, fragmentReadData, versionFragments)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts))
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (CompanionRequirement)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (VersionDoc)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData)

-- | Restore, ordering and assembly of one format's managed mirror documents.
data MirrorDocuments = MirrorDocuments
    { mdRestore :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
    -- ^ Pair retained source entries with the current target files. Any missing pairing restores nothing.
    , mdOrder :: [(Text, VersionDoc)] -> [(Text, VersionDoc)]
    -- ^ Place restored versions, supplied in version-text order, in the native listing order.
    , mdEntryKey :: Int -> EntryKey
    -- ^ The native entry coordinate for a project-wide position.
    , mdRekey :: (EntryKey -> EntryKey) -> CachedDoc -> Maybe CachedDoc
    -- ^ Rewrite a restored document's entry coordinates.
    , mdAssemble :: ProjectMirrorRecord -> [VersionDoc] -> CachedDoc
    -- ^ Assemble the managed listing from rebased versions in listing order.
    , mdCompanions :: CompanionRequirement
    -- ^ Which files need a retained companion. Boot requires the mirror codec to prepare native ones.
    }

-- | Pair each fragment's retained entries with the current target file of the same name, numbering artifacts across fragments.
restoreFragments ::
    (Int -> EntryKey) ->
    (MirrorReadData -> Maybe (source, Map EntryKey entry)) ->
    MirrorVersion ->
    [Artifact] ->
    Maybe (NonEmpty (PackageDetails, source, NonEmpty (Artifact, entry)))
restoreFragments entryKey project mirror current = sequenceA restored
  where
    target = Map.fromList [(artFilename artifact, artifact) | artifact <- current]
    (_, restored) = mapAccumL (\start fragment -> (start + length (pkgArtifacts (fragmentDetails fragment)), restoreFragment start fragment)) 0 (versionFragments mirror)
    restoreFragment start fragment = do
        (source, entries) <- project (fragmentReadData fragment)
        let original = fragmentDetails fragment
        pairs <- for (zip [start ..] (toList (pkgArtifacts original))) $ \(position, artifact) -> do
            native <- Map.lookup (artFilename artifact) target
            entry <- Map.lookup (artEntryKey artifact) entries
            pure (artifact{artEntryKey = entryKey position, artUrl = artUrl native}, entry)
        (original,source,) <$> nonEmpty pairs
