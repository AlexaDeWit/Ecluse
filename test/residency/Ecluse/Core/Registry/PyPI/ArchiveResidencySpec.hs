-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE PackageImports #-}

-- | Isolated archive probes compare retained heap and native cleanup with the production workspace charges.
module Ecluse.Core.Registry.PyPI.ArchiveResidencySpec (spec, probeMain) where

import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Conduit
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats)
import System.Directory (getFileSize)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import Test.Hspec
import UnliftIO.Exception (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)
import "ecluse-lzma-managed" Codec.Compression.Lzma qualified as ManagedLzma
import "lzma" Codec.Compression.Lzma qualified as Lzma

import Ecluse.Core.NativeProbe (
    NativeMeasurement (..),
    NativeSample,
    checkControl,
    earlyExitProbe,
    haskellControl,
    nativeGrowthLimit,
    nativeMeasurement,
    probeChild,
    sampleWithoutGC,
 )
import Ecluse.Core.Registry.Archive.Inflate (InflateFormat (GzipMembers), withInflatedChunks)
import Ecluse.Core.Registry.PyPI.Archive (archiveMetadata)
import Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks)
import Ecluse.Core.Registry.PyPI.Archive.Lzma (LzmaFormat (XzArchive), withLzmaChunks)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Registry.Archive (tarBytes)
import Ecluse.Test.Registry.PyPI.Archive (metadata, zipBytes)
import Ecluse.Test.Support (expectRight)

-- Heap measurements include the source retained for publication.
data Measurement = Measurement
    { sourceBytes :: Int
    , workspaceBytes :: Int
    , peakHeapBytes :: Integer
    , retainedHeapBytes :: Integer
    , allocatedBytes :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON Measurement
instance FromJSON Measurement

spec :: Spec
spec = describe "archive residency" $ do
    for_ ["zip", "gzip", "bzip", "xz"] $ \format ->
        it (toString format <> " retains source and selected metadata within their charges") $
            withSystemTempDirectory "ecluse-archive-heap" $ \directory -> do
                let path = directory </> "fixture"
                    body = metadata <> BS.replicate (256 * 1024) 97
                    paths = [("example-1/data-" <> show index, BS.replicate 4096 98) | index <- [1 .. 500 :: Int]]
                bytes <-
                    if format == "zip"
                        then zipBytes (("example-1.dist-info/METADATA", body) : [(toText name, value) | (name, value) <- paths])
                        else BSL.toStrict . compressor format <$> tarBytes (("example-1/PKG-INFO", body) : paths)
                BS.writeFile path bytes
                result <- child ("heap-" <> format) path "128k"
                peakHeapBytes result `shouldSatisfy` (<= toInteger (sourceBytes result + workspaceBytes result))
                retainedHeapBytes result `shouldSatisfy` (> 0)
                allocatedBytes result `shouldSatisfy` (> 0)
    for_ ["gzip", "bzip", "xz"] $ \format ->
        for_ ["return", "refusal", "cancel"] $ \ending ->
            it (toString format <> " releases native state on " <> toString ending <> " before a GC") $
                withSystemTempDirectory "ecluse-archive-native" $ \directory -> do
                    let path = directory </> "fixture"
                    BS.writeFile path (BSL.toStrict (compressor format (BSL.replicate (1024 * 1024) 97)))
                    result <- child ("native-" <> format <> "-" <> ending) path "128m"
                    checkControl result
                    nativeOutputCount result `shouldBe` 64
                    nativeAllocatedBytes result `shouldSatisfy` (<= controlAllocatedBytes result)
                    nativeVirtualGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)
                    nativeAllocatorGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)
    it "detects retained LZMA decoders with the same VM and allocator gates" $
        withSystemTempDirectory "ecluse-archive-retained" $ \directory -> do
            let path = directory </> "fixture"
            BS.writeFile path (BSL.toStrict (Lzma.compress (BSL.replicate (1024 * 1024) 97)))
            result <- child "retained" path "128m"
            checkControl result
            nativeOutputCount result `shouldBe` 8
            nativeAllocatedBytes result `shouldSatisfy` (<= controlAllocatedBytes result)
            nativeVirtualGrowthBytes result `shouldSatisfy` (> nativeGrowthLimit)
            nativeAllocatorGrowthBytes result `shouldSatisfy` (> nativeGrowthLimit)

compressor :: Text -> BSL.ByteString -> BSL.ByteString
compressor = \case
    "gzip" -> GZip.compress
    "bzip" -> BZip.compress
    _ -> Lzma.compress

child :: (FromJSON a) => Text -> FilePath -> String -> IO a
child = probeChild "archive"

-- | Parent-authored fixtures keep compression work outside the measured child interval.
probeMain :: String -> FilePath -> IO ()
probeMain rawMode path = do
    output <- case T.splitOn "-" (toText rawMode) of
        ["heap", format] -> encode <$> heapProbe format path
        ["native", format, ending] -> encode <$> nativeProbe format ending path
        ["retained"] -> encode <$> retainedProbe path
        _ -> fail "unknown archive probe mode"
    BS.putStr (BSL.toStrict output)

heapProbe :: Text -> FilePath -> IO Measurement
heapProbe format path = do
    size <- fromIntegral <$> getFileSize path
    performMajorGC
    rtsBefore <- getRTSStats
    bytes <- BS.readFile path
    paid <- newIORef 0
    let charge wanted = modifyIORef' paid (max wanted)
        filename = case format of
            "zip" -> "example-1-py3-none-any.whl"
            "gzip" -> "example-1.tar.gz"
            "bzip" -> "example-1.tar.bz2"
            _ -> "example-1.tar.xz"
    selected <- archiveMetadata defaultLimits charge (unscopedPyPI "example") (pypiVersion "1") filename bytes >>= expectRight
    bracket (newStablePtr (bytes, selected)) freeStablePtr $ \_ -> do
        performMajorGC
        rtsAfter <- getRTSStats
        peak <- readIORef paid
        pure
            Measurement
                { sourceBytes = size
                , workspaceBytes = peak
                , peakHeapBytes = toInteger (max_live_bytes rtsAfter) - toInteger (gcdetails_live_bytes (gc rtsBefore))
                , retainedHeapBytes = toInteger (gcdetails_live_bytes (gc rtsAfter)) - toInteger (gcdetails_live_bytes (gc rtsBefore))
                , allocatedBytes = toInteger (allocated_bytes rtsAfter) - toInteger (allocated_bytes rtsBefore)
                }

nativeProbe :: Text -> Text -> FilePath -> IO NativeMeasurement
nativeProbe format ending path = do
    bytes <- BS.readFile path
    let firstOutput stop output = do
            output `shouldSatisfy` (not . BS.null)
            stop
        consume stop = await >>= maybe (liftIO (fail "decoder produced no output")) (liftIO . firstOutput stop)
        run charge stop = case format of
            "gzip" -> withInflatedChunks (GzipMembers 1) charge bytes (\pull -> pull >>= expectRight >>= firstOutput stop)
            "bzip" -> withBzipChunks 1 charge bytes (\source -> void (runExceptT (runConduit (source .| consume stop)) >>= expectRight))
            _ -> withLzmaChunks XzArchive charge [bytes] (\source -> void (runExceptT (runConduit (source .| consume stop)) >>= expectRight))
    earlyExitProbe ending run

retainedProbe :: FilePath -> IO NativeMeasurement
retainedProbe path = do
    bytes <- BS.readFile path
    replicateM_ 4 $
        void (withLzmaChunks XzArchive (const (pure ())) [bytes] (runExceptT . runConduit . (.| await)) >>= expectRight)
    control <- haskellControl
    outputs <- newIORef 0
    sampled <- sampleWithoutGC (withRetained bytes outputs 8)
    count <- readIORef outputs
    count `shouldBe` 8
    pure (nativeMeasurement control sampled (8 * 12 * 1024 * 1024) count)
  where
    withRetained :: ByteString -> IORef Int -> Int -> IO NativeSample -> IO NativeSample
    withRetained _ _ 0 sample = sample
    withRetained bytes outputs remaining sample =
        ManagedLzma.withDecompressIO ManagedLzma.defaultDecompressParams{ManagedLzma.decompressMemLimit = 12 * 1024 * 1024} $ \stream -> do
            firstOutput bytes stream
            modifyIORef' outputs (+ 1)
            withRetained bytes outputs (remaining - 1) sample
    firstOutput input = \case
        ManagedLzma.DecompressInputRequired supply -> supply input >>= firstOutput BS.empty
        ManagedLzma.DecompressOutputAvailable output _ -> output `shouldSatisfy` (not . BS.null)
        _ -> fail "retained decoder did not reach native output"
