-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The sampler behind feedback memory admission ("Ecluse.Core.Server.Admission.Memory"). Every
'sampleIntervalMicros' it reads two memory views and the collector's counters, steps the
collector brake, and publishes the reading to the gate.

The heap view is the RTS's megablock total against the heap ceiling @-M@: the figure the RTS
itself compares with @-M@, refreshed at every collection. The kernel view is each limiting
cgroup's @memory.current@ less its reclaimable @inactive_file@ pages, against its @memory.max@:
what the kernel counts toward an OOM kill, including memory the RTS does not see. A view is
present only when its ceiling is. The RTS figures need @-T@. Without it only the kernel view and
no brake remain.
-}
module Ecluse.Rts.Sampler (
    runMemorySampler,

    -- * Pure parsing (exported for its spec)
    parseInactiveFile,
    parseByteCount,
) where

import Data.Text qualified as T
import GHC.RTS.Flags (GCFlags (oldGenFactor, pcFreeHeap), getGCFlags)
import GHC.Stats (GCDetails (gcdetails_mem_in_use_bytes), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import UnliftIO (tryIO)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Server.Admission.Memory (GateStats (..), MemoryAdmission, publishReading, readGateStats, renderGateStats)
import Ecluse.Core.Server.Admission.Memory.Brake (
    BrakeState (BrakeEngaged),
    CollectorReading (..),
    CollectorSample (..),
    CollectorState,
    HeapGeometry (..),
    collectorBrake,
    defaultBrakeThresholds,
    initialCollectorState,
    stepCollector,
    ticksSinceMajor,
 )
import Ecluse.Core.Server.Admission.Memory.Gate (
    GateCore,
    GateState (GateClosed),
    MemoryView,
    Reading (Reading),
    bindingView,
    coreReading,
    coreState,
    mkMemoryView,
    mvUsedBytes,
    rdViews,
    refreshDue,
    refreshTicks,
    reservedBytes,
    sampleIntervalMicros,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Rts (RtsPosture (rpAllocAreaBytes, rpCapabilities, rpMaxHeapBytes), cgroupMemoryLimits, currentRtsPosture)

-- What the sampler reads from, fixed when it starts.
data MemoryProbe = MemoryProbe
    { probeStats :: Bool
    , probeHeapCeiling :: Maybe Int
    , probeGeometry :: Maybe HeapGeometry
    , probeCgroups :: [(FilePath, Int)]
    }

{- | Sample and publish forever, logging the probe once and the gate's counters whenever the gate
held work since the last summary. The probe is fixed at start, so a supervisor restart re-reads
the posture and the cgroup limits.
-}
runMemorySampler :: (Text -> IO ()) -> MetricsPort -> MemoryAdmission -> IO ()
runMemorySampler logLine metrics gate = do
    probe <- newMemoryProbe
    logLine (describeProbe probe)
    let loop n st reported = do
            st' <- sampleOnce metrics gate probe st
            reported' <- if n `mod` summarySamples == 0 then summarise logLine gate reported else pure reported
            threadDelay sampleIntervalMicros
            loop (n + 1) st' reported'
    loop (1 :: Int) initialCollectorState Nothing

-- Samples between counter summaries: ten seconds at the sampling interval.
summarySamples :: Int
summarySamples = 100

summarise :: (Text -> IO ()) -> MemoryAdmission -> Maybe GateStats -> IO (Maybe GateStats)
summarise logLine gate reported = do
    stats <- readGateStats gate
    when (maybe True (heldSince stats) reported) (logLine (renderGateStats stats))
    pure (Just stats)

-- Whether the gate held, shed, or braked anything between two summaries.
heldSince :: GateStats -> GateStats -> Bool
heldSince now before =
    gsWaited now /= gsWaited before
        || gsShedMemory now /= gsShedMemory before
        || gsShedBrake now /= gsShedBrake before
        || gsClosedSamples now /= gsClosedSamples before
        || gsBrakeSamples now /= gsBrakeSamples before

describeProbe :: MemoryProbe -> Text
describeProbe probe =
    "memory admission: RTS statistics "
        <> (if probeStats probe then "on" else "off (build or run with +RTS -T for the heap view and the collector brake)")
        <> ", heap view "
        <> maybe "absent (no -M)" (\bytes -> "against -M " <> show bytes) (probeHeapCeiling probe <* guard (probeStats probe))
        <> ", kernel views "
        <> (if null (probeCgroups probe) then "absent (no cgroup memory.max)" else T.intercalate ", " [toText dir <> " " <> show limit | (dir, limit) <- probeCgroups probe])

newMemoryProbe :: IO MemoryProbe
newMemoryProbe = do
    statsOn <- getRTSStatsEnabled
    posture <- currentRtsPosture
    flags <- getGCFlags
    cgroups <- cgroupMemoryLimits
    let heapCeiling = rpMaxHeapBytes posture
        -- The collector's reserve for the allocation area: the -m floor or the nursery, whichever is larger.
        nursery heap = max (round (pcFreeHeap flags * fromIntegral heap / 200)) (rpAllocAreaBytes posture * rpCapabilities posture)
        geometry = (\heap -> HeapGeometry heap (nursery heap) (oldGenFactor flags)) <$> heapCeiling
    pure (MemoryProbe statsOn heapCeiling (geometry <* guard statsOn) cgroups)

sampleOnce :: MetricsPort -> MemoryAdmission -> MemoryProbe -> CollectorState -> IO CollectorState
sampleOnce metrics gate probe st = do
    stats <- if probeStats probe then Just <$> getRTSStats else pure Nothing
    kernel <- catMaybes <$> traverse kernelView (probeCgroups probe)
    let heap = do
            s <- stats
            limit <- probeHeapCeiling probe
            mkMemoryView (fromIntegral (gcdetails_mem_in_use_bytes (gc s))) limit
        (reading, st') = case stats of
            Just s -> stepCollector defaultBrakeThresholds (probeGeometry probe) (collectorSample s) st
            Nothing -> (CollectorReading Nothing Nothing Nothing (collectorBrake st), st)
    core <- publishReading gate (Reading (maybeToList heap <> kernel) (crBrake reading))
    recordGauges metrics core reading
    when (isJust stats && refreshDue refreshTicks (coreState core) (crBrake reading) (ticksSinceMajor st')) performMajorGC
    pure st'

collectorSample :: RTSStats -> CollectorSample
collectorSample s =
    CollectorSample
        { csCpuNs = fromIntegral (cpu_ns s)
        , csGcCpuNs = fromIntegral (gc_cpu_ns s)
        , csMajorGcs = fromIntegral (major_gcs s)
        , csCumulativeLiveBytes = fromIntegral (cumulative_live_bytes s)
        }

-- One limiting cgroup's working set against its limit. An unreadable file drops the view for this sample.
kernelView :: (FilePath, Int) -> IO (Maybe MemoryView)
kernelView (dir, limit) = do
    current <- (>>= parseByteCount) <$> readSmallFile (dir <> "/memory.current")
    inactive <- (>>= parseInactiveFile) <$> readSmallFile (dir <> "/memory.stat")
    pure (current >>= \used -> mkMemoryView (used - fromMaybe 0 inactive) limit)

-- A cgroup file can vanish or refuse a read mid-run. Either reads as absent, never as a fault.
readSmallFile :: FilePath -> IO (Maybe Text)
readSmallFile path = rightToMaybe <$> tryIO (decodeUtf8 <$> readFileBS path)

-- | Parse a cgroup byte counter such as @memory.current@.
parseByteCount :: Text -> Maybe Int
parseByteCount body = do
    n <- readMaybe (toString (T.strip body))
    n <$ guard (n >= 0)

-- | The @inactive_file@ line of a cgroup-v2 @memory.stat@ body: page cache and lazily freed pages the kernel reclaims before an OOM kill.
parseInactiveFile :: Text -> Maybe Int
parseInactiveFile body = listToMaybe (mapMaybe field (lines body))
  where
    field line = case T.words line of
        ["inactive_file", value] -> parseByteCount value
        _ -> Nothing

recordGauges :: MetricsPort -> GateCore -> CollectorReading -> IO ()
recordGauges metrics core reading = do
    for_ (bindingView (rdViews (coreReading core))) (mpMemoryAdmissionMeasuredBytes metrics . mvUsedBytes)
    mpMemoryAdmissionReservedBytes metrics (reservedBytes core)
    mpMemoryAdmissionGateClosed metrics (coreState core == GateClosed)
    mpMemoryAdmissionBrakeEngaged metrics (crBrake reading == BrakeEngaged)
    for_ (crGcShare reading) (mpMemoryAdmissionGcCpuPermille metrics . permille)
    for_ (crLiveAtMajor reading) (mpMemoryAdmissionLiveBytes metrics)
    for_ (crReclaim reading) (mpMemoryAdmissionReclaimPermille metrics . permille)
  where
    permille share = round (share * 1000)
