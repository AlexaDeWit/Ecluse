-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Production serve transforms for performance harnesses.
The caller supplies the fetch snapshot so measurements do not rehash whole documents.
-}
module Ecluse.Test.Server.Transform (
    serveTransformSize,
    serveDocumentBytes,
    serveDocumentSize,
    serveDocumentSizeUnder,
    SelectedDepth (..),
    selectiveDepth,
    detailsDepth,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName, artHashes, pkgArtifacts)
import Ecluse.Core.Package.Filter (fpSurvivors, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource), SourceId, mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)

import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Rules.Types (EvalContext, PrecededRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (snapshotValue))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (permissiveAgeRules, syntheticProxyBase)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmVersion)
import Ecluse.Test.Rules (filterPlan, inertRuleDeps)
import Ecluse.Test.Snapshot (readDetails)

-- | Measure the npm transform against the original fetch snapshot, excluding its digest cost.
serveTransformSize :: EvalContext -> (Snapshot Value, PackageInfo) -> IO Int
serveTransformSize ctx input = fromIntegral . BSL.length <$> transformBody inertRuleDeps permissiveAgeRules assemble ctx input
  where
    assemble sources plan base = encode (assembleMergedPackument syntheticProxyBase sources plan base)

-- | Transform an adapter's cached document using the supplied fetch snapshot and projection.
serveDocumentBytes :: AdapterMetadata -> EvalContext -> (Snapshot CachedDoc, PackageInfo) -> IO ByteString
serveDocumentBytes adapter ctx input = BSL.toStrict <$> serveDocumentBody inertRuleDeps permissiveAgeRules adapter ctx input

-- | Force the served body without adding a strict-buffer copy to the measured transform.
serveDocumentSize :: AdapterMetadata -> EvalContext -> (Snapshot CachedDoc, PackageInfo) -> IO Int
serveDocumentSize = serveDocumentSizeUnder inertRuleDeps permissiveAgeRules

-- | 'serveDocumentSize' with the rule phase's dependencies and policy supplied.
serveDocumentSizeUnder :: RuleDeps -> [PrecededRule] -> AdapterMetadata -> EvalContext -> (Snapshot CachedDoc, PackageInfo) -> IO Int
serveDocumentSizeUnder deps policy adapter ctx input = fromIntegral . BSL.length <$> serveDocumentBody deps policy adapter ctx input

serveDocumentBody :: RuleDeps -> [PrecededRule] -> AdapterMetadata -> EvalContext -> (Snapshot CachedDoc, PackageInfo) -> IO LByteString
serveDocumentBody deps policy adapter = transformBody deps policy assemble
  where
    assemble sources plan base =
        metadataSerialise adapter (metadataAssemble adapter syntheticProxyBase sources plan (Just base))

transformBody :: RuleDeps -> [PrecededRule] -> (Map SourceId (Snapshot raw) -> MergePlan -> raw -> LByteString) -> EvalContext -> (Snapshot raw, PackageInfo) -> IO LByteString
transformBody deps policy assemble ctx (source, info) = do
    plan <- filterPlan deps ctx policy info
    pure $ case mergePackuments [(GatedSource, restrictToSurvivors (fpSurvivors plan) info <$ source)] of
        Just merged
            | not (Map.null (mpSurvivors merged)) ->
                assemble (Map.singleton 0 source) merged (snapshotValue source)
        _ -> BSL.empty

-- | Distinguish a measured read from a missing version or failed projection.
data SelectedDepth
    = -- | The selected version's artifact-digest count.
      Depth Int
    | -- | The packument decoded and carries no such version.
      VersionAbsent
    | -- | The packument did not decode within the parser limits.
      DecodeFailed
    deriving stock (Eq, Show)

-- | Read one version's snapshot out of a raw packument, parsing only that version.
selectiveDepth :: PackageName -> (ByteString, Version) -> SelectedDepth
selectiveDepth pkg (raw, version) =
    case projectNpmVersion defaultLimits pkg version raw of
        Left _ -> DecodeFailed
        Right versionRead -> detailsDepth (readDetails versionRead)

-- | Force a selected snapshot through a deep field, its artifact digests.
detailsDepth :: Maybe PackageDetails -> SelectedDepth
detailsDepth = maybe VersionAbsent (Depth . length . artHashes . NE.head . pkgArtifacts)
