-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure filtering, merging, assembly, and serialisation of prepared metadata.
Decoding and fetch-digest construction stay outside the measured operation.
-}
module Ecluse.Core.ServeBench (benchmarks) where

import Ecluse.Bench.Corpus (benchEvalContext, entryName, syntheticInput)
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Snapshot (Snapshot (Snapshot), digestOf)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | Measure real captures and the growth across synthetic release counts.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "serve (filter + merge-assemble)" $
        [ bench (entryName entry) (whnfAppIO serveDepth (Snapshot (digestOf bytes) document, info))
        | entry@(_, bytes, info, document) <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    (syntheticInput ecosystem . fromIntegral)
                    (either (const (pure (-1))) serveDepth)
               ]
  where
    serveDepth = serveDocumentSize (ebMetadata ecosystem) benchEvalContext
