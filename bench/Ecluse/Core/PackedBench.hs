-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The packed served form against aeson's tree, in one run: each capture's full read held both
ways, and its listing assembled and rendered from each. Selected reads still build aeson's tree,
so both reads stay in the product and compare on the same inputs.
-}
module Ecluse.Core.PackedBench (benchmarks) where

import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import UnliftIO.Exception (evaluate)

import Ecluse.Bench.Corpus (benchEvalContext, entryName)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageInfo, PackageName, infoVersions)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Test.Corpus (CorpusPackage (cpPackage))
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Ecluse.Test.Snapshot (digestOf)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Per capture: the full read and the listing render, as aeson's tree and packed.
benchmarks :: EcosystemBench -> IO Benchmark
benchmarks ecosystem = do
    entries <- traverse captureGroup (ebCorpus ecosystem)
    pure (bgroup "packed served form (full read and listing render)" entries)
  where
    serve = serveDocumentSize (ebMetadata ecosystem) benchEvalContext
    captureGroup entry@(package, bytes, info, document) = do
        let name = cpPackage package
            digest = digestOf bytes
        tree <- asTree document
        pure $
            bgroup
                (entryName entry)
                [ bench "full read, aeson's tree" (whnf (versionCount . treeRead name) bytes)
                , bench "full read, packed" (whnfAppIO (fmap versionCount . ebRead ecosystem name) bytes)
                , bench "listing render, aeson's tree" (whnfAppIO serve (Snapshot digest tree, info))
                , bench "listing render, packed" (whnfAppIO serve (Snapshot digest document, info))
                ]
    treeRead :: PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
    treeRead name = case ebEcosystem ecosystem of
        Npm -> fmap (second (fst npmCached)) . projectNpmManifest defaultLimits name
        PyPI -> fmap (second (fst pypiSimpleCached)) . projectPyPIIndex defaultLimits name
        RubyGems -> const (Left MetadataUndecodable)
    -- The document the full read built before packing, forced before any measurement.
    asTree document = do
        let tree = fromMaybe document ((fst npmCached <$> snd npmCached document) <|> (fst pypiSimpleCached <$> snd pypiSimpleCached document))
        _ <- evaluate (BSL.length (metadataSerialise (ebMetadata ecosystem) tree))
        pure tree
    versionCount = either (const (-1)) (Map.size . infoVersions . fst)
