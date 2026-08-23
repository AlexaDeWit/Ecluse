-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm serve-path transforms the performance harnesses time.

'serveTransformSize' is the full-packument work a metadata read pays: the rule sweep, the
merge, the served-document assembly with the fused tarball rewrite, and the re-serialise.
'selectiveDepth' is the single-version read the cold tarball gate makes. Each result is
forced when it is built, so a caller times the real work rather than a thunk.
-}
module Ecluse.Test.Server.Transform (
    serveTransformSize,
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
import Ecluse.Core.Package.Merge (MergePlan (mpSurvivors), Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmVersion)
import Ecluse.Core.Rules.Types (EvalContext)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Corpus (permissiveAgeRules, syntheticProxyBase)
import Ecluse.Test.Rules (filterPlan, inertRuleDeps)

{- | The full serve transform over a decoded packument and its projection. The returned
served-body size forces the transform, and the paired argument suits @whnfAppIO@.
-}
serveTransformSize :: EvalContext -> (Value, PackageInfo) -> IO Int
serveTransformSize ctx (value, info) = do
    plan <- filterPlan inertRuleDeps ctx permissiveAgeRules info
    pure $ case mergePackuments [(GatedSource, restrictToSurvivors (fpSurvivors plan) info)] of
        Just merged
            | not (Map.null (mpSurvivors merged)) ->
                let body = encode (assembleMergedPackument syntheticProxyBase (Map.singleton 0 value) merged value)
                 in fromIntegral (BSL.length body)
        _ -> 0

{- | The outcome of a single-version metadata read. A caller distinguishes a measured read
from one that never reached the projection.
-}
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
        Right details -> detailsDepth details

-- | Force a selected snapshot through a deep field, its artifact digests.
detailsDepth :: Maybe PackageDetails -> SelectedDepth
detailsDepth = maybe VersionAbsent (Depth . length . artHashes . NE.head . pkgArtifacts)
