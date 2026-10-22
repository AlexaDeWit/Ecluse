-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure response bounds, the integrity floor, and the artifact location check over each ecosystem's
wire documents and projected releases. Parsed documents enter the nesting guard without timing their decoding.
-}
module Ecluse.Core.SecurityBench (benchmarks) where

import Ecluse.Test.Security.Limits (checkVersionCount)

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryInfo, entryName, forcedTree, syntheticPackageInfo, unHeldTree)
import Ecluse.Core.Package (PackageInfo, artUrl, infoDistTags, infoInvalidEntries, infoVersions, pkgArtifacts)
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Package.Filter.Internal (ArtifactOrigin, artifactOrigin, locateArtifact, noAuthorityVerdict)
import Ecluse.Core.Package.Integrity (IntegrityFloor)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, boundedRead, defaultLimits, maxMetadataBytes)
import Ecluse.Core.Server.Pipeline.Internal (admitByIntegrity)
import Ecluse.Core.Server.Pipeline.Shared (integrityBelowFloor, integrityMissing, trustedIntegrityBelowFloor, trustedIntegrityMissing)
import Ecluse.Core.Server.Response (ServeDecision)
import Ecluse.Test.Corpus (CaptureUpstream (..))
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry (..))
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Test.Tasty (withResource)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfAppIO, whnfIO)

-- | Exercise bounded reads, both structural guards and the location check on real and synthetic inputs.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "security guards" $
        [env (pure bodyChunks) $ \chunks -> bench "boundedRead (8 MiB body, 64 KiB chunks)" (whnfIO (boundedReadDepth chunks))]
            <> [ bgroup
                    (entryName entry)
                    [ withResource (forcedTree ecosystem document) (const pass) $ \held ->
                        bench "checkNestingDepth" (whnfAppIO (fmap (ebNestingDepth ecosystem . unHeldTree)) held)
                    , bench "checkVersionCount" (whnf versionCountDepth (entryInfo entry))
                    , bench "artifact locations" (whnf (locatedArtifacts origin) (entryInfo entry))
                    , bench "artifact locations (document)" (whnf (enforcedArtifacts (ebUpstream ecosystem)) (entryInfo entry))
                    , bench "integrity floor (public)" (whnf (floorAdmitted defaultMinIntegrity integrityBelowFloor integrityMissing) (entryInfo entry))
                    , bench "integrity floor (trusted)" (whnf (floorAdmitted defaultMinTrustedIntegrity trustedIntegrityBelowFloor trustedIntegrityMissing) (entryInfo entry))
                    ]
               | entry@LoadedEntry{leSnapshot = document} <- ebCorpus ecosystem
               ]
            <> [ bench
                    "checkNestingDepth (synthetic / 100000)"
                    (whnf (either (const (-1)) (ebNestingDepth ecosystem)) (ebReadDocument ecosystem (ebSynthetic ecosystem 100000)))
               , bench
                    "checkVersionCount (synthetic / 2000)"
                    (whnf (either (const (-1)) versionCountDepth) (syntheticPackageInfo ecosystem 2000))
               ]
  where
    origin = artifactOrigin (upstreamAuthorities (ebUpstream ecosystem)) (upstreamOrigin (ebUpstream ecosystem))

boundedReadDepth :: [ByteString] -> IO Int
boundedReadDepth chunks = do
    cursor <- newIORef chunks
    result <- boundedRead (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (popChunk cursor)
    pure $! either limitErrorCode (BS.length . snd) result
  where
    popChunk cursor = atomicModifyIORef' cursor $ \case
        [] -> ([], BS.empty)
        (c : cs) -> (cs, c)

bodyChunks :: [ByteString]
bodyChunks = replicate 128 (BS.replicate 65536 0x61)

versionCountDepth :: PackageInfo -> Int
versionCountDepth info = either limitErrorCode (const 1) (checkVersionCount defaultLimits info)

limitErrorCode :: LimitError -> Int
limitErrorCode _ = -1

-- The projected artifacts whose URL passes the check against the capture's registry, each URL checked alone and its result forced.
locatedArtifacts :: ArtifactOrigin -> PackageInfo -> Int
locatedArtifacts origin info =
    length [() | details <- Map.elems (infoVersions info), art <- toList (pkgArtifacts details), Right !_ <- [snd (locateArtifact origin noAuthorityVerdict (artUrl art))]]

-- The files and drop records the location check leaves when one call checks the whole document.
enforcedArtifacts :: CaptureUpstream -> PackageInfo -> Int
enforcedArtifacts upstream info = plusKeptFiles (length (infoInvalidEntries enforced)) enforced
  where
    enforced = enforceArtifactLocations (upstreamAuthorities upstream) (upstreamOrigin upstream) info

-- The files, dist-tags and refusals left after the integrity floor, every surviving file walked as a render would.
floorAdmitted :: (IntegrityFloor floor) => floor -> ServeDecision -> ServeDecision -> PackageInfo -> Int
floorAdmitted floorSpec belowFloorRefusal missingRefusal info =
    plusKeptFiles (length refusals + Map.size (infoDistTags admitted)) admitted
  where
    (admitted, refusals) = admitByIntegrity floorSpec belowFloorRefusal missingRefusal info

plusKeptFiles :: Int -> PackageInfo -> Int
plusKeptFiles start info = Map.foldl' (\count details -> count + length (pkgArtifacts details)) start (infoVersions info)
