-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The served form a full read holds against aeson's tree, in one run: each capture's full read
into aeson's tree, and its listing assembled and rendered from each form. The production full read
of the same bytes is the wire bench's full metadata projection.
-}
module Ecluse.Core.PackedBench (benchmarks) where

import Data.Map.Strict qualified as Map

import Ecluse.Bench.Corpus (benchEvalContext, entryName, forcedTree, unHeldTree)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo, PackageName, infoVersions)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Per capture: the full read into aeson's tree, and the listing render from aeson's tree and as served.
benchmarks :: EcosystemBench -> IO Benchmark
benchmarks ecosystem = do
    entries <- traverse captureGroup (ebCorpus ecosystem)
    pure (bgroup "packed served form (full read and listing render)" entries)
  where
    serve = serveDocumentSize (ebMetadata ecosystem) benchEvalContext
    captureGroup entry@(package, bytes, info, served) = do
        tree <- unHeldTree <$> forcedTree ecosystem served
        pure $
            bgroup
                (entryName entry)
                [ bench "full read, aeson's tree" (whnf (versionCount . treeRead (cpPackage package)) bytes)
                , bench "listing render, aeson's tree" (whnfAppIO serve (tree <$ served, info))
                , bench "listing render, as served" (whnfAppIO serve (served, info))
                ]
    treeRead :: PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
    treeRead name = case ebEcosystem ecosystem of
        Npm -> fmap (second (fst npmCached)) . projectNpmManifest defaultLimits name
        PyPI -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name
        RubyGems -> const (Left MetadataUndecodable)
    versionCount = either (const (-1)) (Map.size . infoVersions . fst)
