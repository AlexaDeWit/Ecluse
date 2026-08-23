-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request bench for the __single-version__ metadata read: the cold tarball
gate's cost to consult one version of a packument.

The status-quo cold path decodes the /whole/ packument and selects one entry ("full decode
+ select"). The optimised path parses only the requested version's object and @time@ entry
("selective decode"). Both run at each corpus entry's @latest@ version, so the bench
reports the saving across the real distribution of sizes.
-}
module Ecluse.Core.SelectiveBench (
    benchmarks,
) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (LoadedEntry, entryName, versionKeysOf)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.Server.Transform (SelectedDepth (DecodeFailed), detailsDepth, selectiveDepth)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The single-version read benches over each corpus entry, at its @latest@-ish version.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup
        "single-version metadata (per package)"
        [ bgroup
            (entryName le)
            [ bench "full decode + select" (whnf (fullSelectDepth (cpPackage cp)) (raw, version))
            , bench "selective decode" (whnf (selectiveDepth (cpPackage cp)) (raw, version))
            ]
        | le@(cp, raw, value) <- loaded
        , version <- maybeToList (targetVersion value)
        ]

{- The version each entry is read at: the last key in its @versions@ object, the most
recently published and the realistic install target. -}
targetVersion :: Value -> Maybe Version
targetVersion value = mkVersion Npm . NE.last <$> nonEmpty (versionKeysOf value)

-- | The status quo: decode the whole packument, then select the one version's snapshot.
fullSelectDepth :: PackageName -> (ByteString, Version) -> SelectedDepth
fullSelectDepth pkg (raw, version) =
    case projectNpmManifest defaultLimits pkg raw of
        Left _ -> DecodeFailed
        Right (info, _raw) -> detailsDepth (Map.lookup (renderVersion version) (infoVersions info))
