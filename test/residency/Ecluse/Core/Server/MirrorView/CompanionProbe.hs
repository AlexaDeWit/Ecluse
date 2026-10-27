-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | External native compact objects use their own read and validation reservation.
module Ecluse.Core.Server.MirrorView.CompanionProbe (CompanionMeasurement (..), probeMain) where

import Control.Concurrent.MVar qualified as MVar
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import System.FilePath ((</>))
import System.IO (withBinaryFile)
import UnliftIO.Async (cancel, waitCatch, waitEither, withAsync)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Package (artFilename)
import Ecluse.Core.Registry.Exchange (boundedChargedRead)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, MirrorInventory, expectedArtifact, inventoryFiles)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.Publish (MirrorCompanion (mcValidate))
import Ecluse.Core.Registry.RubyGems.Mirror (rubyMirrorCompanion)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), defaultLimits)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MemoryTicket, charge, meterSnapshot, newChargeRegion, takeLargestCharge, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.MemoryModel.HeapSample qualified as Heap
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Mirror.RubyGems.Residency
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (expectRight, testMemoryMeter)

-- | Byte counts, memory charges and actual live heap describe separate quantities.
data CompanionMeasurement = CompanionMeasurement
    { cmBaselineLive :: Word64
    , cmHeldLive :: Word64
    , cmReleasedLive :: Word64
    , cmSampledPeakLiveGrowth :: Integer
    , cmAllocatedBytes :: Integer
    , cmReferenceBytes :: Int
    , cmReturnedBytes :: Int
    , cmReadBytes :: Int
    , cmReadChunks :: Int
    , cmRequestedReadCharge :: Int
    , cmPeakTicketCharge :: Int
    , cmChargeDuringRequest :: Maybe Int
    , cmProcessMaxLiveBytes :: Word64
    , cmChargeAfterRelease :: Int
    , cmOutcome :: Text
    }
    deriving stock (Generic, Show)

instance ToJSON CompanionMeasurement
instance FromJSON CompanionMeasurement

data Context = Context
    { ctxView :: MirrorView
    , ctxRecord :: ProjectMirrorRecord
    , ctxVersion :: Version
    , ctxFilename :: Text
    , ctxReference :: CompanionRef
    , ctxInventory :: MirrorInventory
    , ctxFile :: ExpectedMirrorFile
    , ctxMeter :: MemoryMeter
    , ctxMode :: IORef Text
    , ctxReadBytes :: IORef Int
    , ctxReadChunks :: IORef Int
    , ctxReadCharge :: IORef Int
    , ctxPeak :: IORef Integer
    , ctxBaseline :: IORef Word64
    , ctxReached :: MVar.MVar ()
    , ctxBlocked :: MVar.MVar ()
    }

-- | Every child reopens native state, then allocates object bytes only inside their reservation.
probeMain :: String -> FilePath -> IO ()
probeMain mode directory = do
    unless (mode `elem` ["RubyCompanionRead", "RubyCompanionValidate", "RubyCompanionCorrupt", "RubyCompanionCancel", "RubyCompanionCancelValidation"]) (fail "unknown native companion probe mode")
    context <- newContext directory
    void (performRead (if mode `elem` ["RubyCompanionValidate", "RubyCompanionCancelValidation"] then "RubyCompanionValidate" else "RubyCompanionRead") context)
    void (takeLargestCharge (ctxMeter context))
    writeIORef (ctxReadBytes context) 0
    writeIORef (ctxReadChunks context) 0
    writeIORef (ctxReadCharge context) 0
    writeIORef (ctxPeak context) 0
    baseline <- Heap.sample
    writeIORef (ctxBaseline context) (Heap.live baseline)
    writeIORef (ctxMode context) (toText mode)
    (held, returned, during, result) <- performRead (toText mode) context
    released <- Heap.sample
    paid <- takeLargestCharge (ctxMeter context)
    after <- snChargedBytes <$> meterSnapshot (ctxMeter context)
    readBytes <- readIORef (ctxReadBytes context)
    chunks <- readIORef (ctxReadChunks context)
    requested <- readIORef (ctxReadCharge context)
    peak <- readIORef (ctxPeak context)
    LBS.putStr . encode $
        CompanionMeasurement
            (Heap.live baseline)
            (Heap.live held)
            (Heap.live released)
            (max peak (toInteger (Heap.live held) - toInteger (Heap.live baseline)))
            (toInteger (Heap.sampleAllocated held) - toInteger (Heap.sampleAllocated baseline))
            (companionSize (ctxReference context))
            returned
            readBytes
            chunks
            requested
            paid
            during
            (Heap.samplePeakLive held)
            after
            result

newContext :: FilePath -> IO Context
newContext directory = do
    record <- readRubyStateRecord directory
    (target, _) <- readRubyStateIdentity directory
    (rawVersion, version) <- maybe (fail "native state has no retained version") pure (Map.lookupMin (recordVersions record))
    (filename, reference) <- maybe (fail "native state has no retained companion") pure (listToMaybe [(name, ref) | (name, CompanionRetained ref) <- Map.toList (versionCompanions version)])
    fragment <- maybe (fail "native state has no source fragment") pure (listToMaybe (toList (versionFragments version)))
    inventory <- expectRight (restoreMirrorInventory (fragmentDetails fragment) (fragmentReadData fragment))
    file <- maybe (fail "native companion has no matching checked artifact") pure (find ((== filename) . artFilename . expectedArtifact) (toList (inventoryFiles inventory)))
    mode <- newIORef "RubyCompanionRead"
    consumed <- newIORef 0
    chunks <- newIORef 0
    requested <- newIORef 0
    peak <- newIORef 0
    baseline <- newIORef 0
    reached <- MVar.newEmptyMVar
    blocked <- MVar.newEmptyMVar
    meter <- testMemoryMeter
    pool <- newLocalPool 1 (256 * 1024 * 1024)
    retention <- newMirrorRetention pool 0 (StoreBudget 0 0)
    let sample = samplePeak baseline peak
        load pay ref = withBinaryFile (directory </> "objects" </> toString (companionDigest ref)) ReadMode $ \handle -> do
            let next = do
                    current <- readIORef consumed
                    selected <- readIORef mode
                    when (selected == "RubyCompanionCancel" && current >= 64 * 1024) (MVar.putMVar reached () >> MVar.takeMVar blocked)
                    bytes <- BS.hGetSome handle (32 * 1024)
                    unless (BS.null bytes) $ modifyIORef' chunks (+ 1)
                    modifyIORef' consumed (+ BS.length bytes)
                    pure $ if selected == "RubyCompanionCorrupt" && current == 0 then corrupt bytes else bytes
                observe cost = modifyIORef' requested (max cost) >> pay cost >> sample
            first StateBound . fmap snd <$> boundedChargedRead observe (MetadataBodyLimit (companionSize ref)) next
        store = MirrorState (\_ _ _ -> pure (Left StateMissingCompanion)) (\_ _ _ _ -> pure (Left StateRetryExhausted)) (\_ _ -> pure (Left StateMissingCompanion)) load
    pure (Context (newMirrorView defaultLimits target store retention) record (mkVersion RubyGems rawVersion) filename reference inventory file meter mode consumed chunks requested peak baseline reached blocked)

corrupt :: ByteString -> ByteString
corrupt bytes = case BS.uncons bytes of
    Nothing -> bytes
    Just (firstByte, rest) -> BS.cons (firstByte `xor` 1) rest

performRead :: Text -> Context -> IO (Heap.HeapSample, Int, Maybe Int, Text)
performRead mode context
    | mode `elem` ["RubyCompanionCancel", "RubyCompanionCancelValidation"] = withAsync operation $ \worker -> withAsync (MVar.takeMVar (ctxReached context)) $ \signal -> do
        waitEither signal worker >>= \case
            Right _ -> fail "native object read ended before the cancellation checkpoint"
            Left () -> pure ()
        cancel worker
        waitCatch worker >>= \case
            Right _ -> fail "native object read completed before cancellation"
            Left _ -> do
                measured <- Heap.sample
                pure (measured, 0, Nothing, "cancelled")
    | otherwise = operation
  where
    operation = withMemoryEntry noopMetricsPort (ctxMeter context) (readAndHold mode context) >>= expectRight

-- Only scalar samples escape the body root and its live request reservation.
{-# NOINLINE readAndHold #-}
readAndHold :: Text -> Context -> MemoryTicket -> IO (Heap.HeapSample, Int, Maybe Int, Text)
readAndHold mode context ticket = do
    attempted <- readMirrorCompanion (ctxView context) ticket (ctxRecord context) (ctxVersion context) (ctxFilename context)
    checked <- case attempted of
        Left fault -> pure (Left (show fault))
        Right bytes | mode `elem` ["RubyCompanionValidate", "RubyCompanionCancelValidation"] -> do
            region <- newChargeRegion (charge ticket)
            let pay cost = do
                    region cost
                    samplePeak (ctxBaseline context) (ctxPeak context)
                    when (mode == "RubyCompanionCancelValidation") (MVar.putMVar (ctxReached context) () >> MVar.takeMVar (ctxBlocked context))
            fmap (first show) (mcValidate rubyMirrorCompanion defaultLimits pay (ctxInventory context) (ctxFile context) bytes)
        Right bytes -> pure (Right bytes)
    case checked of
        Left fault -> do
            measured <- Heap.sample
            during <- snChargedBytes <$> meterSnapshot (ctxMeter context)
            pure (measured, 0, Just during, fault)
        Right bytes -> bracket (newStablePtr bytes) freeStablePtr $ \root -> do
            measured <- Heap.sample
            held <- deRefStablePtr root
            size <- evaluate (BS.length held)
            during <- snChargedBytes <$> meterSnapshot (ctxMeter context)
            pure (measured, size, Just during, "verified")

samplePeak :: IORef Word64 -> IORef Integer -> IO ()
samplePeak baseline highest = do
    before <- readIORef baseline
    when (before > 0) $ Heap.sample >>= \now -> modifyIORef' highest (max (toInteger (Heap.live now) - toInteger before))
