-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What a codec costs on each capture's served body, beside the render that fills the assembled
cache. A group's name carries the body's size, and a compress row's name its output size.
Setup refuses a codec whose output does not inflate back to the body.
-}
module Ecluse.Core.CompressBench (benchmarks) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Lazy qualified as ZstdLazy
import Codec.Lz4 qualified as Lz4
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import UnliftIO.Exception (evaluate, throwIO)

import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, restrictToSurvivors)
import Ecluse.Core.Package.Merge (MergePlan, Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataAssemble, metadataSerialise))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.ServedDocument (RenderRefused)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Test.Corpus (CaptureUpstream (upstreamAuthorities, upstreamOrigin), syntheticProxyBase)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry)
import Ecluse.Test.Snapshot (digestOf)
import Test.Tasty (localOption, mkTimeout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Per capture: the fill render, one copy of the body, and each codec's compress and inflate.
benchmarks :: EcosystemBench -> IO Benchmark
benchmarks ecosystem = do
    captures <- traverse (captureGroup ecosystem) (ebCorpus ecosystem)
    pure . localOption (mkTimeout 30_000_000) $
        bgroup ("served body codecs: " <> toString (ecosystemName (ebEcosystem ecosystem))) captures

-- The body is the one a single public source serves with every located version admitted.
captureGroup :: EcosystemBench -> LoadedEntry -> IO Benchmark
captureGroup ecosystem entry@(_, raw, projected, document) = do
    plan <- maybe (throwIO (NoMergePlan (entryName entry))) pure (mergePackuments [(GatedSource, Snapshot digest admitted)])
    body <- either throwIO (evaluate . BSL.toStrict) (render plan document)
    rows <- traverse (codecRows (entryName entry) body) codecs
    pure $
        bgroup
            (entryName entry <> " " <> show (BS.length body) <> " B")
            ( bench "render at fill" (whnf (either (const (-1)) (BS.length . BSL.toStrict) . render plan) document)
                : bench "copy to one buffer" (whnf (BS.length . BS.copy) body)
                : concat rows
            )
  where
    digest = digestOf raw
    upstream = ebUpstream ecosystem
    located = enforceArtifactLocations (upstreamAuthorities upstream) (upstreamOrigin upstream) projected
    admitted = restrictToSurvivors (Map.keysSet (infoVersions located)) located
    render :: MergePlan -> CachedDoc -> Either RenderRefused LByteString
    render plan held =
        metadataSerialise (ebMetadata ecosystem) $
            metadataAssemble (ebMetadata ecosystem) syntheticProxyBase (Map.singleton 0 (Snapshot digest held)) plan (Just held)

codecRows :: String -> ByteString -> Codec -> IO [Benchmark]
codecRows capture body codec = do
    entry <- evaluate (codecCompress codec body)
    for_ (codecInflates codec) $ \(form, inflate) ->
        when (BSL.toStrict (inflate (BS.length body) entry) /= body) (throwIO (RoundTripFailed capture (codecLabel codec <> " " <> form)))
    pure $
        bench (codecLabel codec <> " compress to " <> show (BS.length entry) <> " B") (whnf (BS.length . codecCompress codec) body)
            : [bench (codecLabel codec <> " " <> form) (whnf (BSL.length . inflate (BS.length body)) entry) | (form, inflate) <- codecInflates codec]

-- A codec at one setting. An inflate takes the body's length and yields the body as lazy chunks.
data Codec = Codec
    { codecLabel :: String
    , codecCompress :: ByteString -> ByteString
    , codecInflates :: [(String, Int -> ByteString -> LByteString)]
    }

codecs :: [Codec]
codecs = map gzipCodec [1, 6, 9] <> map zstdCodec [1, 3] <> [lz4Codec]

gzipCodec :: Int -> Codec
gzipCodec level =
    Codec
        ("gzip-" <> show level)
        (BSL.toStrict . GZip.compressWith GZip.defaultCompressParams{GZip.compressLevel = GZip.compressionLevel level} . BSL.fromStrict)
        [("inflate streamed", const (GZip.decompress . BSL.fromStrict))]

-- Level 3 also inflates into one buffer of the body's size, the form that holds a whole body per hit.
zstdCodec :: Int -> Codec
zstdCodec level =
    Codec
        ("zstd-" <> show level)
        (Zstd.compress level)
        (("inflate streamed", const (ZstdLazy.decompress . BSL.fromStrict)) : [("inflate to one buffer", const zstdWhole) | level == 3])
  where
    zstdWhole entry = case Zstd.decompress entry of
        Zstd.Decompress body -> BSL.fromStrict body
        _ -> BSL.empty

-- The block form, which needs the body's length to inflate.
lz4Codec :: Codec
lz4Codec = Codec "lz4" Lz4.compressBlock [("inflate to one buffer", \size entry -> BSL.fromStrict (Lz4.decompressBlockSz entry size))]

-- A capture that merges to no plan, or a codec form whose output is not the body it compressed.
data SetupRefused
    = NoMergePlan String
    | RoundTripFailed String String
    deriving stock (Show)

instance Exception SetupRefused
