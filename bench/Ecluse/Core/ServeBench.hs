-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure filtering, merging, assembly, and serialisation of prepared metadata.
Decoding and fetch-digest construction stay outside the measured operation.
-}
module Ecluse.Core.ServeBench (benchmarks) where

import Control.Exception (throw)
import Data.Aeson (Encoding, Value (Number, Object, String), eitherDecode, toEncoding, toJSON)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ecluse.Bench.Corpus (benchEvalContext, entryName, syntheticInput)
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), infoVersions)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Package.Filter (restrictToSurvivors)
import Ecluse.Core.Package.Merge (Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmPacked, npmRendered, pypiRendered, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Packed (Pieces (ArrayPieces, ObjectPieces), RenderPlan (..))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument, simpleEncoding, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Filter (serialiseSimpleDocument)
import Ecluse.Core.Snapshot (Snapshot (snapshotValue))
import Ecluse.Test.Corpus (cpName, cpPackage, syntheticProxyBase)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry (..))
import Ecluse.Test.Registry.PyPI.Metadata (simpleValue)
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Measure real captures and the growth across synthetic release counts.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "serve (filter + merge-assemble)" $
        [ bench (entryName entry) (whnfAppIO serveDepth (served, info))
        | entry@LoadedEntry{leInfo = info, leSnapshot = served} <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    (syntheticInput ecosystem . fromIntegral)
                    (either (const (pure (-1))) serveDepth)
               ]
            <> [npmAssemblyBenchmarks ecosystem | ebEcosystem ecosystem == Npm]
            <> [pypiEncodingBenchmarks ecosystem | ebEcosystem ecosystem == PyPI]
            <> [pypiAssemblyBenchmarks ecosystem | ebEcosystem ecosystem == PyPI]
  where
    serveDepth = serveDocumentSize (ebMetadata ecosystem) benchEvalContext

-- Existing serve rows include assembly and do not isolate envelope encoding.
pypiEncodingBenchmarks :: EcosystemBench -> Benchmark
pypiEncodingBenchmarks ecosystem =
    bgroup
        "prepared PyPI encoding"
        [ bgroup
            (entryName entry)
            [ encodingSize previousEncoding document `seq`
                encodingSize simpleEncoding document `seq`
                    bgroup
                        label
                        [ bench "previous encoder" (whnf (encodingSize previousEncoding) document)
                        , bench "direct pairs" (whnf (encodingSize simpleEncoding) document)
                        ]
            | (label, document) <-
                ("retained envelope and files", retained)
                    : [ ("envelope fields " <> show count <> ", files " <> show fileCount, simpleDocument (envelope count) (take fileCount (simpleFiles retained)))
                      | count <- [0, 1, 8, 64, 512]
                      , fileCount <- [0, 1]
                      ]
            ]
        | entry@LoadedEntry{leSnapshot = source} <- ebCorpus ecosystem
        , Just retained <- [snd pypiSimpleCached (snapshotValue source)]
        ]
  where
    envelope count = KeyMap.fromList [(Key.fromText (prefix <> show position), Number (fromIntegral position)) | position <- [0 .. count - 1 :: Int], let prefix = if even position then "a-" else "z-"]

encodingSize :: (SimpleDocument -> Encoding) -> SimpleDocument -> Int64
encodingSize encoder = LBS.length . encodingToLazyByteString . encoder

-- Freeze the base encoder here because the benchmark does not link the mirrored unit spec.
previousEncoding :: SimpleDocument -> Encoding
previousEncoding document =
    Encoding.pairs (KeyMap.foldMapWithKey Encoding.pair (KeyMap.insert "files" files (toEncoding <$> simpleEnvelope document)))
  where
    files = Encoding.list (toEncoding . snd) (simpleFiles document)

-- Assembly must finish before the measured render reads the packed listing.
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
        | entry@LoadedEntry{leInfo = info, leSnapshot = source} <- ebCorpus ecosystem
        , isJust (snd npmPacked (snapshotValue source))
        ]

-- The survivors in the assembled plan, forcing what the render reads: its members, prefix and pieces.
planDepth :: CachedDoc -> Int
planDepth assembled = case snd npmRendered assembled of
    Just plan -> renderPlanDepth plan
    Nothing -> -1

-- Force the fields rendering reads without rendering or encoding the plan.
renderPlanDepth :: RenderPlan -> Int
renderPlanDepth plan = rnf (planMembers plan) `seq` maybe () (`seq` ()) (planPrefix plan) `seq` pieceCount (planPieces plan)
  where
    pieceCount = \case
        ObjectPieces pieces -> foldl' (\count (key, piece) -> key `seq` piece `seq` count + 1) 0 pieces
        ArrayPieces pieces -> foldl' (\count piece -> piece `seq` count + 1) 0 pieces

-- Preparation and native output checks finish before tasty runs either assembly row.
pypiAssemblyBenchmarks :: EcosystemBench -> Benchmark
pypiAssemblyBenchmarks ecosystem =
    bgroup
        "prepared PyPI assembly"
        [ bgroup
            (entryName entry)
            [ rnf (show plan :: Text) `seq`
                pypiPlanDepth (assemble (snapshotValue source)) `seq`
                    checkAssembly label expected assemble `seq`
                        bench label (whnf (pypiPlanDepth . assemble) (snapshotValue source))
            | (label, survivors) <- [("all", Map.keysSet (infoVersions info)), ("one", Set.fromList (take 1 (Map.keys (infoVersions info))))]
            , let plan = fromMaybe (throw (PyPIAssemblyInvalid label)) (mergePackuments [(GatedSource, restrictToSurvivors survivors info <$ source)])
                  assemble = metadataAssemble (ebMetadata ecosystem) syntheticProxyBase (Map.singleton 0 source) plan . Just
                  coordinates = Set.fromList [artEntryKey artifact | details <- Map.elems (Map.restrictKeys (infoVersions info) survivors), artifact <- toList (pkgArtifacts details)]
                  expected = assemblyExpected (cpName (lePackage entry)) (Set.toAscList survivors) coordinates reference
                  checkAssembly row output operation = case fmap eitherDecode (serialiseSimpleDocument (operation (snapshotValue source))) of
                    Right (Right actual) | actual == output -> ()
                    _ -> throw (PyPIAssemblyInvalid row)
            ]
        | entry@LoadedEntry{leInfo = info, leSnapshot = source, leBytes = raw} <- ebCorpus ecosystem
        , let reference = case ebTreeRead ecosystem (cpPackage (lePackage entry)) raw of
                Right (_, document) -> fromMaybe (throw (PyPIAssemblyInvalid (entryName entry))) (snd pypiSimpleCached document)
                Left _ -> throw (PyPIAssemblyInvalid (entryName entry))
        ]

-- Select original coordinates and rewrite URLs without calling production assembly.
assemblyExpected :: Text -> [Text] -> Set EntryKey -> SimpleDocument -> Value
assemblyExpected project versions coordinates reference =
    simpleValue (simpleDocument envelope [(key, rebase value) | (key, value) <- simpleFiles reference, key `Set.member` coordinates])
  where
    envelope = KeyMap.insert "versions" (toJSON versions) (simpleEnvelope reference)
    rebase (Object fields) = case KeyMap.lookup "filename" fields of
        Just (String filename) -> Object (KeyMap.insert "url" (String (syntheticProxyBase <> "/simple/" <> project <> "/" <> filename)) fields)
        _ -> throw (PyPIAssemblyInvalid (toString project))
    rebase _ = throw (PyPIAssemblyInvalid (toString project))

pypiPlanDepth :: CachedDoc -> Int
pypiPlanDepth document = case snd pypiRendered document of
    Just plan -> foldl' (\count table -> table `seq` count + 1) (renderPlanDepth plan) (planTables plan)
    Nothing -> throw (PyPIAssemblyInvalid "missing native render plan")

newtype PyPIAssemblyInvalid = PyPIAssemblyInvalid String
    deriving stock (Show)

instance Exception PyPIAssemblyInvalid
