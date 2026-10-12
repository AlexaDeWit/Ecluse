-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

-- | Isolated archive probes compare retained heap and native cleanup with the production workspace charges.
module Ecluse.Core.Registry.PyPI.ArchiveResidencySpec (spec, probeMain) where

import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Conduit
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bits (finiteBitSize)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Conc (getAllocationCounter)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, gcs, max_live_bytes), getRTSStats)
import System.Directory (getFileSize)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Async (AsyncCancelled (..), cancel, race_, wait, waitCatch, withAsync)
import UnliftIO.Exception (bracket, evaluate, finally, throwIO, try)
import UnliftIO.Temporary (withSystemTempDirectory)
import "ecluse-lzma-managed" Codec.Compression.Lzma qualified as ManagedLzma
import "lzma" Codec.Compression.Lzma qualified as Lzma

import Ecluse.Core.Registry.PyPI.Archive (archiveMetadata)
import Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks)
import Ecluse.Core.Registry.PyPI.Archive.Inflate (InflateFormat (GzipMembers), withInflatedChunks)
import Ecluse.Core.Registry.PyPI.Archive.Lzma (LzmaFormat (XzArchive), withLzmaChunks)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Archive (metadata, tarBytes, zipBytes)
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

-- Native VM growth is checked separately from uncollected Haskell output pages.
data NativeMeasurement = NativeMeasurement
    { nativeWorkspaceBytes :: Int
    , nativeAllocatedBytes :: Integer
    , nativeVirtualGrowthBytes :: Integer
    , nativeResidentGrowthBytes :: Integer
    , controlAllocatedBytes :: Integer
    , controlVirtualGrowthBytes :: Integer
    , controlResidentGrowthBytes :: Integer
    , nativeOutputCount :: Int
    }
    deriving stock (Generic, Show)

instance ToJSON NativeMeasurement
instance FromJSON NativeMeasurement

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
                    nativeVirtualGrowthBytes result `shouldSatisfy` (<= virtualSlackBytes)
                    nativeResidentGrowthBytes result `shouldSatisfy` (<= nativeAllocatedBytes result + virtualSlackBytes)
    it "detects retained LZMA decoders with the same virtual-memory gate" $
        withSystemTempDirectory "ecluse-archive-retained" $ \directory -> do
            let path = directory </> "fixture"
            BS.writeFile path (BSL.toStrict (Lzma.compress (BSL.replicate (1024 * 1024) 97)))
            result <- child "retained" path "128m"
            checkControl result
            nativeOutputCount result `shouldBe` 8
            nativeAllocatedBytes result `shouldSatisfy` (<= controlAllocatedBytes result)
            nativeVirtualGrowthBytes result `shouldSatisfy` (> virtualSlackBytes)

-- GHC 9.10 reserves the heap range before committing pages. The control checks that one megablock of slack suffices.
virtualSlackBytes :: Integer
virtualSlackBytes = 1024 * 1024

checkControl :: NativeMeasurement -> Expectation
checkControl result = do
    controlAllocatedBytes result `shouldSatisfy` (>= 16 * 1024 * 1024)
    controlVirtualGrowthBytes result `shouldSatisfy` (<= virtualSlackBytes)

compressor :: Text -> BSL.ByteString -> BSL.ByteString
compressor = \case
    "gzip" -> GZip.compress
    "bzip" -> BZip.compress
    _ -> Lzma.compress

child :: (FromJSON a) => Text -> FilePath -> String -> IO a
child mode path nursery = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable ["--archive-probe", toString mode, path, "+RTS", "-T", "-N1", "-F1", "-A" <> nursery, "-RTS"] ""
    unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
    putStrLn ("archive-residency " <> output)
    expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))

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
    before <- getRTSStats
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
        after <- getRTSStats
        peak <- readIORef paid
        pure
            Measurement
                { sourceBytes = size
                , workspaceBytes = peak
                , peakHeapBytes = toInteger (max_live_bytes after) - toInteger (gcdetails_live_bytes (gc before))
                , retainedHeapBytes = toInteger (gcdetails_live_bytes (gc after)) - toInteger (gcdetails_live_bytes (gc before))
                , allocatedBytes = toInteger (allocated_bytes after) - toInteger (allocated_bytes before)
                }

data NativeSample = NativeSample
    { sampledAllocated :: Integer
    , sampledVirtual :: Integer
    , sampledResident :: Integer
    }

nativeProbe :: Text -> Text -> FilePath -> IO NativeMeasurement
nativeProbe format ending path = do
    bytes <- BS.readFile path
    paid <- newIORef 0
    childAllocated <- newIORef 0
    outputCount <- newIORef 0
    refuseCharge <- newIORef False
    let charge wanted = do
            refused <- readIORef refuseCharge
            when refused (throwIO NativeAdmissionRefused)
            modifyIORef' paid (max wanted)
        counted action = do
            start <- getAllocationCounter
            finally action $ do
                end <- getAllocationCounter
                modifyIORef' childAllocated (+ (toInteger start - toInteger end))
        onOutput output stop = do
            output `shouldSatisfy` (not . BS.null)
            modifyIORef' outputCount (+ 1)
            stop
        consume stop = await >>= maybe (liftIO (fail "decoder produced no output")) (\output -> liftIO (onOutput output stop))
        run stop = case format of
            "gzip" -> withInflatedChunks (GzipMembers 1) charge bytes (\pull -> pull >>= expectRight >>= (`onOutput` stop))
            "bzip" -> withBzipChunks 1 charge bytes (\source -> void (runExceptT (runConduit (source .| consume stop)) >>= expectRight))
            _ -> withLzmaChunks XzArchive charge [bytes] (\source -> void (runExceptT (runConduit (source .| consume stop)) >>= expectRight))
        attempt = do
            writeIORef refuseCharge False
            case ending of
                "refusal" -> do
                    result <- try (run (writeIORef refuseCharge True >> readIORef paid >>= charge . (+ 1)))
                    result `shouldBe` Left NativeAdmissionRefused
                "cancel" -> do
                    ready <- MVar.newEmptyMVar
                    blocked <- MVar.newEmptyMVar
                    withAsync (counted (run (MVar.putMVar ready () >> MVar.takeMVar blocked))) $ \worker -> do
                        race_ (MVar.takeMVar ready) (wait worker)
                        cancel worker
                        result <- waitCatch worker
                        case result of
                            Left fault -> (fromException fault :: Maybe AsyncCancelled) `shouldBe` Just AsyncCancelled
                            Right _ -> expectationFailure "native decoder swallowed cancellation"
                _ -> run (pure ())
    replicateM_ 4 attempt
    control <- haskellControl
    writeIORef childAllocated 0
    writeIORef outputCount 0
    sampled <- withoutGC (replicateM_ 64 attempt)
    asynchronousAllocated <- readIORef childAllocated
    peak <- readIORef paid
    count <- readIORef outputCount
    pure (nativeMeasurement control sampled{sampledAllocated = sampledAllocated sampled + asynchronousAllocated} peak count)

nativeMeasurement :: NativeSample -> NativeSample -> Int -> Int -> NativeMeasurement
nativeMeasurement control sampled workspace count =
    NativeMeasurement
        { nativeWorkspaceBytes = workspace
        , nativeAllocatedBytes = sampledAllocated sampled
        , nativeVirtualGrowthBytes = sampledVirtual sampled
        , nativeResidentGrowthBytes = sampledResident sampled
        , controlAllocatedBytes = sampledAllocated control
        , controlVirtualGrowthBytes = sampledVirtual control
        , controlResidentGrowthBytes = sampledResident control
        , nativeOutputCount = count
        }

haskellControl :: IO NativeSample
haskellControl = withoutGC $ do
    chunks <- traverse (evaluate . BS.replicate (256 * 1024)) [0 .. 63]
    bracket (newStablePtr chunks) freeStablePtr $ \_ -> do
        sum (map BS.length chunks) `shouldBe` 16 * 1024 * 1024

withoutGC :: IO () -> IO NativeSample
withoutGC action = sampleWithoutGC (action >>)

-- The callback samples before retained native scopes close. The GC check includes the memory-counter reads.
sampleWithoutGC :: (IO NativeSample -> IO NativeSample) -> IO NativeSample
sampleWithoutGC run = do
    finiteBitSize (0 :: Int) `shouldBe` 64
    performMajorGC
    (virtualBefore, residentBefore) <- processMemory
    before <- getRTSStats
    allocationBefore <- getAllocationCounter
    run $ do
        allocationAfter <- getAllocationCounter
        (virtualAfter, residentAfter) <- processMemory
        after <- getRTSStats
        gcs after `shouldBe` gcs before
        pure
            NativeSample
                { sampledAllocated = toInteger allocationBefore - toInteger allocationAfter
                , sampledVirtual = virtualAfter - virtualBefore
                , sampledResident = residentAfter - residentBefore
                }

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

processMemory :: IO (Integer, Integer)
processMemory = do
    rows <- T.lines . decodeUtf8 <$> BS.readFile "/proc/self/status"
    let value key = case [amount | row <- rows, [label, amount, "kB"] <- [T.words row], label == key] of
            [amount] -> maybe (fail "invalid process memory counter") (pure . (* 1024)) (readMaybe (toString amount))
            _ -> fail "missing process memory counter"
    (,) <$> value "VmSize:" <*> value "VmRSS:"

data NativeAdmissionRefused = NativeAdmissionRefused deriving stock (Eq, Show)
instance Exception NativeAdmissionRefused
