-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Isolated archive probes compare retained heap and native cleanup with the production workspace charges.
module Ecluse.Core.Registry.PyPI.ArchiveResidencySpec (spec, probeMain) where

import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as Lzma
import Conduit
import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Conc (getAllocationCounter)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, gcs, max_live_bytes), getRTSStats)
import System.Directory (getFileSize)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Async (AsyncCancelled (..), cancel, race_, wait, waitCatch, withAsync)
import UnliftIO.Exception (bracket, finally, throwIO, try)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Registry.PyPI.Archive (archiveMetadata)
import Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks)
import Ecluse.Core.Registry.PyPI.Archive.Inflate (InflateFormat (GzipMembers), withInflatedChunks)
import Ecluse.Core.Registry.PyPI.Archive.Lzma (LzmaFormat (XzArchive), withLzmaChunks)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Archive (metadata, tarBytes, zipBytes)
import Ecluse.Test.Support (expectRight)

-- | Heap and process counters have different scopes and remain separate in the report.
data Measurement = Measurement
    { sourceBytes :: Int
    , workspaceBytes :: Int
    , peakHeapBytes :: Integer
    , retainedHeapBytes :: Integer
    , allocatedBytes :: Integer
    , virtualGrowthBytes :: Integer
    , residentGrowthBytes :: Integer
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
        for_ ["return", "failure", "cancel"] $ \ending ->
            it (toString format <> " releases native state on " <> toString ending <> " before a GC") $
                withSystemTempDirectory "ecluse-archive-native" $ \directory -> do
                    let path = directory </> "fixture"
                    BS.writeFile path (BSL.toStrict (compressor format (BSL.replicate (1024 * 1024) 97)))
                    result <- child ("native-" <> format <> "-" <> ending) path "128m"
                    let bound = toInteger (workspaceBytes result) + allocatedBytes result + 65536
                    virtualGrowthBytes result `shouldSatisfy` (<= bound)
                    residentGrowthBytes result `shouldSatisfy` (<= bound)

compressor :: Text -> BSL.ByteString -> BSL.ByteString
compressor = \case
    "gzip" -> GZip.compress
    "bzip" -> BZip.compress
    _ -> Lzma.compress

child :: Text -> FilePath -> String -> IO Measurement
child mode path nursery = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable ["--archive-probe", toString mode, path, "+RTS", "-T", "-N1", "-F1", "-A" <> nursery, "-RTS"] ""
    unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
    putStrLn ("archive-residency " <> output)
    expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))

-- | Parent-authored fixtures keep compression work outside the measured child interval.
probeMain :: String -> FilePath -> IO ()
probeMain rawMode path = do
    measured <- case T.splitOn "-" (toText rawMode) of
        ["heap", format] -> heapProbe format path
        ["native", format, ending] -> nativeProbe format ending path
        _ -> fail "unknown archive probe mode"
    BS.putStr (BSL.toStrict (encode measured))

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
                , virtualGrowthBytes = 0
                , residentGrowthBytes = 0
                }

nativeProbe :: Text -> Text -> FilePath -> IO Measurement
nativeProbe format ending path = do
    bytes <- BS.readFile path
    paid <- newIORef 0
    childAllocated <- newIORef 0
    let charge wanted = modifyIORef' paid (max wanted)
        counted action = do
            start <- getAllocationCounter
            finally action $ do
                end <- getAllocationCounter
                modifyIORef' childAllocated (+ (toInteger start - toInteger end))
        run stop = case format of
            "gzip" -> withInflatedChunks (GzipMembers 1) charge bytes (\pull -> void (pull >>= expectRight) >> stop)
            "bzip" -> withBzipChunks 1 charge bytes (\source -> void (runExceptT (runConduit (source .| (await >> liftIO stop))) >>= expectRight))
            _ -> withLzmaChunks XzArchive charge [bytes] (\source -> void (runExceptT (runConduit (source .| (await >> liftIO stop))) >>= expectRight))
        attempt = case ending of
            "failure" -> do
                result <- try (run (throwIO NativeConsumerStopped))
                result `shouldBe` Left NativeConsumerStopped
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
    performMajorGC
    (virtualBefore, residentBefore) <- processMemory
    before <- getRTSStats
    writeIORef childAllocated 0
    allocationBefore <- getAllocationCounter
    replicateM_ 64 attempt
    allocationAfter <- getAllocationCounter
    asynchronousAllocated <- readIORef childAllocated
    after <- getRTSStats
    gcs after `shouldBe` gcs before
    (virtualAfter, residentAfter) <- processMemory
    peak <- readIORef paid
    pure
        Measurement
            { sourceBytes = BS.length bytes
            , workspaceBytes = peak
            , peakHeapBytes = 0
            , retainedHeapBytes = 0
            , allocatedBytes = toInteger allocationBefore - toInteger allocationAfter + asynchronousAllocated
            , virtualGrowthBytes = virtualAfter - virtualBefore
            , residentGrowthBytes = residentAfter - residentBefore
            }

processMemory :: IO (Integer, Integer)
processMemory = do
    rows <- T.lines . decodeUtf8 <$> BS.readFile "/proc/self/status"
    let value key = case [amount | row <- rows, [label, amount, "kB"] <- [T.words row], label == key] of
            [amount] -> maybe (fail "invalid process memory counter") (pure . (* 1024)) (readMaybe (toString amount))
            _ -> fail "missing process memory counter"
    (,) <$> value "VmSize:" <*> value "VmRSS:"

data NativeConsumerStopped = NativeConsumerStopped deriving stock (Eq, Show)
instance Exception NativeConsumerStopped
