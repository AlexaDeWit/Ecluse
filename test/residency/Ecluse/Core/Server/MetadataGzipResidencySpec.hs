-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Isolated deflater probes: the native workspace against its allowance, release on every exit, and retained output.
module Ecluse.Core.Server.MetadataGzipResidencySpec (spec, probeMain) where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (gc, max_live_bytes), getRTSStats)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import Test.Hspec
import UnliftIO.Exception (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.NativeProbe (
    NativeMeasurement (..),
    allocatorSnapshot,
    allocatorTotal,
    checkControl,
    earlyExitProbe,
    nativeGrowthLimit,
    probeChild,
 )
import Ecluse.Core.Server.MetadataGzip (GzipBody (..), gzipBufferChargeBytes, gzipMetadata, gzipWorkspaceBytes)
import Ecluse.Test.Support (incompressibleBytes)

-- Allocator figures are glibc arena and mapped bytes, measured from a warmed baseline.
data WorkspaceMeasurement = WorkspaceMeasurement
    { workspaceAllowanceBytes :: Int
    , bufferChargeBytes :: Int
    , nativePeakBytes :: Integer
    , nativeAfterReleaseBytes :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON WorkspaceMeasurement
instance FromJSON WorkspaceMeasurement

-- Heap figures are live bytes above the baseline taken before the source was read.
data HeapMeasurement = HeapMeasurement
    { sourceBytes :: Int
    , outputBytes :: Int
    , chargedBytes :: Int
    , outputChargeBytes :: Int
    , peakHeapBytes :: Integer
    , retainedHeapBytes :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON HeapMeasurement
instance FromJSON HeapMeasurement

spec :: Spec
spec = describe "metadata gzip residency" $ do
    it "holds the native deflater and one output buffer within the workspace allowance" $
        withFixture (BS.replicate (1024 * 1024) 97) $ \path -> do
            result <- child "workspace" path "128m"
            nativePeakBytes result `shouldSatisfy` (> 128 * 1024)
            nativePeakBytes result + toInteger (bufferChargeBytes result) `shouldSatisfy` (<= toInteger (workspaceAllowanceBytes result))
            nativeAfterReleaseBytes result `shouldSatisfy` (< 64 * 1024)
    it "retains its output within the buffer charges" $
        withFixture (incompressibleBytes (1024 * 1024)) $ \path -> do
            result <- child "heap" path "128k"
            retainedHeapBytes result `shouldSatisfy` (<= toInteger (sourceBytes result + outputChargeBytes result))
            peakHeapBytes result `shouldSatisfy` (<= toInteger (sourceBytes result + chargedBytes result))
            outputBytes result `shouldSatisfy` (> sourceBytes result)
    for_ ["return", "refusal", "cancel"] $ \ending ->
        it ("releases the native deflater on " <> toString ending <> " before a GC") $
            withFixture (BS.replicate (1024 * 1024) 97) $ \path -> do
                result <- child ("native-" <> ending) path "128m"
                checkControl result
                nativeOutputCount result `shouldBe` 64
                nativeAllocatedBytes result `shouldSatisfy` (<= controlAllocatedBytes result)
                nativeVirtualGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)
                nativeAllocatorGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)

withFixture :: ByteString -> (FilePath -> IO a) -> IO a
withFixture bytes use =
    withSystemTempDirectory "ecluse-gzip-residency" $ \directory -> do
        let path = directory </> "body"
        BS.writeFile path bytes
        use path

child :: (FromJSON a) => Text -> FilePath -> String -> IO a
child = probeChild "gzip"

-- | Parent-authored bodies keep fixture work outside the measured child interval.
probeMain :: String -> FilePath -> IO ()
probeMain mode path = do
    output <- case mode of
        "workspace" -> encode <$> workspaceProbe path
        "heap" -> encode <$> heapProbe path
        "native-return" -> encode <$> nativeProbe "return" path
        "native-refusal" -> encode <$> nativeProbe "refusal" path
        "native-cancel" -> encode <$> nativeProbe "cancel" path
        _ -> fail "unknown gzip probe mode"
    BS.putStr (BSL.toStrict output)

-- Every charge after the workspace's runs with the deflater open, so its allocator samples bound the native peak.
workspaceProbe :: FilePath -> IO WorkspaceMeasurement
workspaceProbe path = do
    bytes <- BS.readFile path
    replicateM_ 4 (void (gzipMetadata (const pass) bytes))
    performMajorGC
    baseline <- allocatorTotal <$> allocatorSnapshot
    highest <- newIORef 0
    let sample _ = allocatorSnapshot >>= \now -> modifyIORef' highest (max (allocatorTotal now - baseline))
    void (gzipMetadata sample bytes)
    released <- allocatorTotal <$> allocatorSnapshot
    peak <- readIORef highest
    pure (WorkspaceMeasurement gzipWorkspaceBytes gzipBufferChargeBytes peak (released - baseline))

heapProbe :: FilePath -> IO HeapMeasurement
heapProbe path = do
    performMajorGC
    rtsBefore <- getRTSStats
    bytes <- BS.readFile path
    paid <- newIORef 0
    compressed <- gzipMetadata (\cost -> modifyIORef' paid (+ cost)) bytes
    bracket (newStablePtr (bytes, compressed)) freeStablePtr $ \_ -> do
        performMajorGC
        rtsAfter <- getRTSStats
        charged <- readIORef paid
        let live = toInteger . gcdetails_live_bytes . gc
        pure
            HeapMeasurement
                { sourceBytes = BS.length bytes
                , outputBytes = gzipLength compressed
                , chargedBytes = charged
                , outputChargeBytes = charged - gzipWorkspaceBytes
                , peakHeapBytes = toInteger (max_live_bytes rtsAfter) - live rtsBefore
                , retainedHeapBytes = live rtsAfter - live rtsBefore
                }

-- An attempt stops at its first output buffer: its deflater is open and its workspace paid.
nativeProbe :: Text -> FilePath -> IO NativeMeasurement
nativeProbe ending path = do
    bytes <- BS.readFile path
    let run :: (Int -> IO ()) -> IO () -> IO ()
        run charge stop = do
            unreached <- newIORef True
            let hooked cost = do
                    charge cost
                    when (cost == gzipBufferChargeBytes) $
                        atomicModifyIORef' unreached (False,) >>= (`when` stop)
            void (gzipMetadata hooked bytes)
    earlyExitProbe ending run
