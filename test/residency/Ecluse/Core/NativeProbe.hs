-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Isolated child probes for native codec state: VM and glibc allocator growth across early exits
with no collection, beside a Haskell control that must pass the same gates. The archive decoders
and the metadata deflater share them.
-}
module Ecluse.Core.NativeProbe (
    -- * Child processes
    probeChild,

    -- * Early-exit release
    NativeMeasurement (..),
    earlyExitProbe,
    checkControl,
    nativeGrowthLimit,
    NativeAdmissionRefused (..),

    -- * Samples without a collection
    NativeSample (..),
    AllocatorSnapshot (..),
    sampleWithoutGC,
    haskellControl,
    nativeMeasurement,
    allocatorSnapshot,
    allocatorTotal,
    processMemory,
) where

import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Bits (finiteBitSize)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import Foreign.Storable (peek)
import GHC.Conc (getAllocationCounter)
import GHC.Stats (RTSStats (gcs), getRTSStats)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Async (AsyncCancelled (..), cancel, race_, wait, waitCatch, withAsync)
import UnliftIO.Exception (bracket, evaluate, finally, throwIO, try)

import Ecluse.Test.Support (expectRight)

-- RSS includes cached and uncollected pages. VM and allocator retention have independent gates.
data NativeMeasurement = NativeMeasurement
    { nativeWorkspaceBytes :: Int
    , nativeAllocatedBytes :: Integer
    , nativeVirtualGrowthBytes :: Integer
    , nativeResidentGrowthBytes :: Integer
    , controlAllocatedBytes :: Integer
    , controlVirtualGrowthBytes :: Integer
    , controlResidentGrowthBytes :: Integer
    , nativeOutputCount :: Int
    , nativeAllocatorBefore :: AllocatorSnapshot
    , nativeAllocatorAfter :: AllocatorSnapshot
    , nativeAllocatorGrowthBytes :: Integer
    , controlAllocatorBefore :: AllocatorSnapshot
    , controlAllocatorAfter :: AllocatorSnapshot
    , controlAllocatorGrowthBytes :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON NativeMeasurement
instance FromJSON NativeMeasurement

-- | Glibc counts freed thread-cache chunks here, so this is retention rather than exact live ownership.
data AllocatorSnapshot = AllocatorSnapshot
    { allocatorArenaBytes :: Integer
    , allocatorMappedBytes :: Integer
    }
    deriving stock (Generic, Show)

instance ToJSON AllocatorSnapshot
instance FromJSON AllocatorSnapshot

-- | The counters one interval moved, with the allocator on both sides of it.
data NativeSample = NativeSample
    { sampledAllocated :: Integer
    , sampledVirtual :: Integer
    , sampledResident :: Integer
    , sampledAllocatorBefore :: AllocatorSnapshot
    , sampledAllocatorAfter :: AllocatorSnapshot
    }

-- | The independent VM and allocator gates each allow one MiB after warmup.
nativeGrowthLimit :: Integer
nativeGrowthLimit = 1024 * 1024

-- | The Haskell control must allocate and still pass both native gates.
checkControl :: NativeMeasurement -> Expectation
checkControl result = do
    controlAllocatedBytes result `shouldSatisfy` (>= 16 * 1024 * 1024)
    controlVirtualGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)
    controlAllocatorGrowthBytes result `shouldSatisfy` (<= nativeGrowthLimit)

-- | Run this executable as a @--\<subject\>-probe@ child and decode the JSON it prints.
probeChild :: (FromJSON a) => String -> Text -> FilePath -> String -> IO a
probeChild subject mode path nursery = do
    executable <- getExecutablePath
    (status, output, errors) <- readProcessWithExitCode executable ["--" <> subject <> "-probe", toString mode, path, "+RTS", "-T", "-N1", "-F1", "-A" <> nursery, "-RTS"] ""
    unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
    putStrLn (subject <> "-residency " <> toString mode <> " " <> output)
    expectRight (eitherDecodeStrict (encodeUtf8 (toText output)))

-- | Four warm-ups, then 64 attempts with no collection, each ending at the codec's first native output as named.
earlyExitProbe :: Text -> ((Int -> IO ()) -> IO () -> IO ()) -> IO NativeMeasurement
earlyExitProbe ending run = do
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
        reached stop = modifyIORef' outputCount (+ 1) >> stop
        attempt = do
            writeIORef refuseCharge False
            case ending of
                "refusal" -> do
                    result <- try (run charge (reached (writeIORef refuseCharge True >> readIORef paid >>= charge . (+ 1))))
                    result `shouldBe` Left NativeAdmissionRefused
                "cancel" -> do
                    ready <- MVar.newEmptyMVar
                    blocked <- MVar.newEmptyMVar
                    withAsync (counted (run charge (reached (MVar.putMVar ready () >> MVar.takeMVar blocked)))) $ \worker -> do
                        race_ (MVar.takeMVar ready) (wait worker)
                        cancel worker
                        result <- waitCatch worker
                        case result of
                            Left fault -> (fromException fault :: Maybe AsyncCancelled) `shouldBe` Just AsyncCancelled
                            Right _ -> expectationFailure "native codec swallowed cancellation"
                _ -> run charge (reached pass)
    replicateM_ 4 attempt
    control <- haskellControl
    writeIORef childAllocated 0
    writeIORef outputCount 0
    sampled <- sampleWithoutGC (replicateM_ 64 attempt >>)
    asynchronousAllocated <- readIORef childAllocated
    peak <- readIORef paid
    count <- readIORef outputCount
    pure (nativeMeasurement control sampled{sampledAllocated = sampledAllocated sampled + asynchronousAllocated} peak count)

-- | Pair the native interval with its control, under the workspace the probe paid and its output count.
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
        , nativeAllocatorBefore = sampledAllocatorBefore sampled
        , nativeAllocatorAfter = sampledAllocatorAfter sampled
        , nativeAllocatorGrowthBytes = allocatorGrowth sampled
        , controlAllocatorBefore = sampledAllocatorBefore control
        , controlAllocatorAfter = sampledAllocatorAfter control
        , controlAllocatorGrowthBytes = allocatorGrowth control
        }

-- | Allocate 16 MiB of Haskell chunks in one interval, which must leave both native gates level.
haskellControl :: IO NativeSample
haskellControl = sampleWithoutGC (holdChunks >>)
  where
    holdChunks = do
        chunks <- traverse (evaluate . BS.replicate (256 * 1024)) [0 .. 63]
        bracket (newStablePtr chunks) freeStablePtr $ \_ ->
            sum (map BS.length chunks) `shouldBe` 16 * 1024 * 1024

-- | The callback samples before retained native scopes close. The GC check includes the memory-counter reads.
sampleWithoutGC :: (IO NativeSample -> IO NativeSample) -> IO NativeSample
sampleWithoutGC run = do
    finiteBitSize (0 :: Int) `shouldBe` 64
    performMajorGC
    (virtualBefore, residentBefore) <- processMemory
    rtsBefore <- getRTSStats
    allocatorBefore <- allocatorSnapshot
    allocationBefore <- getAllocationCounter
    run $ do
        allocationAfter <- getAllocationCounter
        (virtualAfter, residentAfter) <- processMemory
        allocatorAfter <- allocatorSnapshot
        rtsAfter <- getRTSStats
        gcs rtsAfter `shouldBe` gcs rtsBefore
        pure
            NativeSample
                { sampledAllocated = toInteger allocationBefore - toInteger allocationAfter
                , sampledVirtual = virtualAfter - virtualBefore
                , sampledResident = residentAfter - residentBefore
                , sampledAllocatorBefore = allocatorBefore
                , sampledAllocatorAfter = allocatorAfter
                }

allocatorGrowth :: NativeSample -> Integer
allocatorGrowth sample = allocatorTotal (sampledAllocatorAfter sample) - allocatorTotal (sampledAllocatorBefore sample)

-- | Arena and mapped bytes together, the figure both allocator gates compare.
allocatorTotal :: AllocatorSnapshot -> Integer
allocatorTotal snapshot = allocatorArenaBytes snapshot + allocatorMappedBytes snapshot

-- | The glibc allocator's arena and mapped bytes. Unsupported platforms fail rather than report a passing zero.
allocatorSnapshot :: IO AllocatorSnapshot
allocatorSnapshot = alloca $ \arena -> alloca $ \mapped -> do
    supported <- c_allocatorSnapshot arena mapped
    unless (supported == 1) (fail "native allocator residency requires Linux with glibc mallinfo2")
    arenaBytes <- toInteger <$> peek arena
    mappedBytes <- toInteger <$> peek mapped
    pure (AllocatorSnapshot arenaBytes mappedBytes)

foreign import ccall unsafe "ecluse_native_allocator_snapshot"
    c_allocatorSnapshot :: Ptr CSize -> Ptr CSize -> IO CInt

-- | Read actual virtual and resident process bytes from Linux procfs.
processMemory :: IO (Integer, Integer)
processMemory = do
    rows <- T.lines . decodeUtf8 <$> BS.readFile "/proc/self/status"
    let value key = case [amount | row <- rows, [label, amount, "kB"] <- [T.words row], label == key] of
            [amount] -> maybe (fail "invalid process memory counter") (pure . (* 1024)) (readMaybe (toString amount))
            _ -> fail "missing process memory counter"
    (,) <$> value "VmSize:" <*> value "VmRSS:"

-- | The charge a refusal-ending attempt raises once its codec reaches native output.
data NativeAdmissionRefused = NativeAdmissionRefused deriving stock (Eq, Show)

instance Exception NativeAdmissionRefused
