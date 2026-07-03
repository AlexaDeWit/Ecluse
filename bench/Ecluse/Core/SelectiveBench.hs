{- | Work-per-request bench for the __single-version__ metadata read: the cold tarball
gate's cost to consult one version of a packument, the whole-document decode against the
selective decode.

The serve path's tarball gate needs exactly one version's
'Ecluse.Core.Package.PackageDetails'. The status-quo cold path decodes the /whole/
packument and selects one entry ("full decode + select"); the optimised path parses only
the requested version's object and @time@ entry, skipping the others
("selective decode") -- "Ecluse.Core.Registry.Npm.Metadata.projectNpmVersion". Both run
over each corpus entry's @latest@ version, so the saving is reported across the real
distribution of package sizes (the heavy packuments -- thousands of versions -- are where a
whole-document decode dominates and the selective decode pays off).

Each result is forced to an 'Int' over a deep field of the selected version, so the
projected snapshot is evaluated rather than left a thunk.
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

{- The version each entry is read at: the last key in its @versions@ object (the most
recently published, the realistic install target). 'Nothing' for a value with no
versions -- never the case for the curated corpus, which 'loadCorpus' guarantees projects
to a non-empty version set. -}
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

{- | Force a selected snapshot by a deep field (the artifact digests -- the
decision surface no longer models dependencies); @-2@ marks an unexpectedly
absent version.
-}
detailsDepth :: Maybe PackageDetails -> Int
detailsDepth = maybe (-2) (length . artHashes . NE.head . pkgArtifacts)
