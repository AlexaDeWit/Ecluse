-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The served form a full read holds against aeson's tree, in one run: each capture's full read
into aeson's tree, and its listing assembled and rendered from each form. The production full read
of the same bytes is the wire bench's full metadata projection.
-}
module Ecluse.Core.PackedBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Ecluse.Core.Package (infoVersions)

import Ecluse.Bench.Corpus (benchEvalContext, entryName, forcedTree, unHeldTree)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry (..))
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Per capture: the full read into aeson's tree, and the listing render from aeson's tree and as served.
benchmarks :: EcosystemBench -> IO Benchmark
benchmarks ecosystem = do
    entries <- traverse captureGroup (ebCorpus ecosystem)
    pure (bgroup "packed served form (full read and listing render)" entries)
  where
    serve = serveDocumentSize (ebMetadata ecosystem) benchEvalContext
    captureGroup entry@LoadedEntry{lePackage = package, leBytes = bytes, leInfo = info, leSnapshot = served} = do
        tree <- unHeldTree <$> forcedTree ecosystem served
        pure $
            bgroup
                (entryName entry)
                [ bench "full read, aeson's tree" (whnf (versionCount . ebTreeRead ecosystem (cpPackage package)) bytes)
                , bench "listing render, aeson's tree" (whnfAppIO serve (tree <$ served, info))
                , bench "listing render, as served" (whnfAppIO serve (served, info))
                ]
    versionCount = either (const (-1)) (Map.size . infoVersions . fst)
