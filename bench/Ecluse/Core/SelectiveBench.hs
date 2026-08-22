-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request bench for the __single-version__ metadata read: the cold tarball
gate's cost to consult one version of a packument. It puts the whole-document decode
against the selective decode.

The serve path's tarball gate needs exactly one version's
'Ecluse.Core.Package.PackageDetails'. The status-quo cold path decodes the /whole/
packument and selects one entry ("full decode + select"). The optimised path parses only
the requested version's object and @time@ entry, skipping the others ("selective
decode"): "Ecluse.Core.Registry.Npm.Metadata.projectNpmVersion". Both run over each
corpus entry's @latest@ version, so the bench reports the saving across the real
distribution of package sizes. A heavy packument of thousands of versions is where a
whole-document decode dominates and the selective decode pays off.

Each result forces an 'Int' over a deep field of the selected version, so the bench
evaluates the projected snapshot instead of leaving a thunk.
-}
module Ecluse.Core.SelectiveBench (
    benchmarks,
) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (CorpusEntry (cePackage), LoadedEntry, entryName, versionKeysOf)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails, PackageInfo (infoVersions), artHashes, pkgArtifacts)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest, projectNpmVersion)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The single-version read benches over each corpus entry, at its @latest@-ish version.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup
        "single-version metadata (per package)"
        [ bgroup
            (entryName le)
            [ bench "full decode + select" (whnf (fullSelectDepth ce) (raw, version))
            , bench "selective decode" (whnf (selectiveDepth ce) (raw, version))
            ]
        | le@(ce, raw, value) <- loaded
        , version <- maybeToList (targetVersion value)
        ]

{- The version each entry is read at: the last key in its @versions@ object, the most
recently published and the realistic install target. -}
targetVersion :: Value -> Maybe Version
targetVersion value = mkVersion Npm . NE.last <$> nonEmpty (versionKeysOf value)

-- | The status quo: decode the whole packument, then select the one version's snapshot.
fullSelectDepth :: CorpusEntry -> (ByteString, Version) -> Int
fullSelectDepth ce (raw, version) =
    case projectNpmManifest defaultLimits (cePackage ce) raw of
        Left _ -> -1
        Right (info, _raw) -> detailsDepth (Map.lookup (renderVersion version) (infoVersions info))

-- | The optimised path: parse only the requested version's snapshot.
selectiveDepth :: CorpusEntry -> (ByteString, Version) -> Int
selectiveDepth ce (raw, version) =
    case projectNpmVersion defaultLimits (cePackage ce) version raw of
        Left _ -> -1
        Right mDetails -> detailsDepth mDetails

{- | Force a selected snapshot by a deep field, the artifact digests (the decision
surface models no dependencies). @-2@ marks an unexpectedly absent version.
-}
detailsDepth :: Maybe PackageDetails -> Int
detailsDepth = maybe (-2) (length . artHashes . NE.head . pkgArtifacts)
