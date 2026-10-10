-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure filtering, merging, assembly, and serialisation of prepared metadata.
Decoding and fetch-digest construction stay outside the measured operation.
-}
module Ecluse.Core.ServeBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ecluse.Bench.Corpus (benchEvalContext, entryName, syntheticInput)
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Filter (restrictToSurvivors)
import Ecluse.Core.Package.Merge (Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmPacked, npmRendered)
import Ecluse.Core.Registry.Json.Packed (Pieces (ArrayPieces, ObjectPieces), RenderPlan (..))
import Ecluse.Core.Snapshot (Snapshot (snapshotValue))
import Ecluse.Test.Corpus (syntheticProxyBase)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Measure real captures and the growth across synthetic release counts.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "serve (filter + merge-assemble)" $
        [ bench (entryName entry) (whnfAppIO serveDepth (served, info))
        | entry@(_, _, info, served) <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    (syntheticInput ecosystem . fromIntegral)
                    (either (const (pure (-1))) serveDepth)
               ]
            <> [npmAssemblyBenchmarks ecosystem | ebEcosystem ecosystem == Npm]
  where
    serveDepth = serveDocumentSize (ebMetadata ecosystem) benchEvalContext

-- | Assemble each packed full read's listing as production does, before its render.
npmAssemblyBenchmarks :: EcosystemBench -> Benchmark
npmAssemblyBenchmarks ecosystem =
    bgroup
        "prepared npm assembly"
        [ bgroup
            (entryName entry)
            [ bench label (whnf (planDepth . metadataAssemble (ebMetadata ecosystem) syntheticProxyBase (Map.singleton 0 source) plan . Just) (snapshotValue source))
            | (label, survivors) <- [("all", Map.keysSet (infoVersions info)), ("one", Set.fromList (take 1 (Map.keys (infoVersions info))))]
            , Just plan <- [mergePackuments [(GatedSource, restrictToSurvivors survivors info <$ source)]]
            ]
        | entry@(_, _, info, source) <- ebCorpus ecosystem
        , isJust (snd npmPacked (snapshotValue source))
        ]

-- The survivors in the assembled plan, forcing what the render reads: its members, prefix and pieces.
planDepth :: CachedDoc -> Int
planDepth assembled = case snd npmRendered assembled of
    Just plan -> rnf (planMembers plan) `seq` maybe () (`seq` ()) (planPrefix plan) `seq` pieceCount (planPieces plan)
    Nothing -> -1
  where
    pieceCount = \case
        ObjectPieces pieces -> foldl' (\count (key, piece) -> key `seq` piece `seq` count + 1) 0 pieces
        ArrayPieces pieces -> foldl' (\count piece -> piece `seq` count + 1) 0 pieces
