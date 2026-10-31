-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared load-spec pod shapes, floors, enforcement, scenario reports, and the sections that render them.
module Ecluse.BenchLoad.Support (
    twoCores,
    floorsAtTwoCores,
    calibrationRunIds,
    slowNetwork,
    scenarioReport,
    loadedSection,
    proxyWithPeak,
    placedWindow,
    rtsSnapshot,
) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.CpuUse (CpuTicks (CpuTicks), CpuWindow (..))
import Ecluse.BenchLoad.Exposition (summariseGauge)
import Ecluse.BenchLoad.Floors (Cell (Floored, ReportOnlyCell, Uncalibrated), Enforcement (NotHeld), FloorKey, Pass (ConcurrencyOne, Loaded), Unheld (SettingsDiffer))
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), LoadSummary (..), ProxyFigures (..), ScenarioReport (..), defaultLoadKnobs)
import Ecluse.BenchLoad.Latency (Percentiles (Percentiles))
import Ecluse.BenchLoad.Pod (CgroupReading (CgroupReading), PodShape (Limited), parseCpuList)
import Ecluse.BenchLoad.Report (Section (..))
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..))
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown))
import Ecluse.Core.Ecosystem (Ecosystem)

-- | A two-CPU quota and a 1 GiB memory bound.
twoCores :: PodShape
twoCores = Limited 2 (1024 * 1024 * 1024)

-- | Example npm and PyPI cells for the bounded pod: four floors at 60% of their medians over runs 1 to 3, a report-only herd, and an uncalibrated count.
floorsAtTwoCores :: Map FloorKey Cell
floorsAtTwoCores =
    Map.fromList
        [ (("npm/merge-cold", Loaded), Floored 338 564 calibrationRunIds)
        , (("npm/merge-cold", ConcurrencyOne), Floored 46 77 calibrationRunIds)
        , (("npm/herd", Loaded), ReportOnlyCell "its burst successes split between admission levels")
        , (("pypi/index-cold", Loaded), Floored 964 1607 calibrationRunIds)
        , (("pypi/index-cold", ConcurrencyOne), Floored 134 224 calibrationRunIds)
        , (("pypi/slow-readers", Loaded), Uncalibrated)
        ]

-- | The IDs of the three calibration runs the example floors come from.
calibrationRunIds :: NonEmpty Integer
calibrationRunIds = 1 :| [2, 3]

-- | A run whose injected latency differs from the calibrated 20 ms, so it is not held.
slowNetwork :: Enforcement
slowNetwork = NotHeld (SettingsDiffer ("upstreamLatencyMs" :| []) :| [])

-- | Build a loaded report with successful p50 and p99 latency in milliseconds.
scenarioReport :: Text -> Int -> Int -> Double -> Double -> ScenarioReport
scenarioReport name successes refusals p50 p99 =
    ScenarioReport
        { srName = name
        , srDescription = "A scenario."
        , srPublicLegUnavailableReason = Nothing
        , srShape = "2cpu-1gib"
        , srLoad = window
        , srCompanion = Nothing
        , srDiagnosticBurst = False
        , srSteps = []
        , srReplayTotals = Nothing
        , srRtsSource = "the proxy"
        , srRtsWindow = Nothing
        , srRtsEnd = Nothing
        , srRetainedBytes = Nothing
        , srProxy = Nothing
        , srEvidence = ""
        }
  where
    window =
        LoadSummary
            { lsLabel = "window"
            , lsConnections = 100
            , lsElapsedSeconds = 30
            , lsCompleted = successes + refusals
            , lsSuccesses = successes
            , lsRefusals = refusals
            , lsOtherStatuses = 0
            , lsTransportFailures = 0
            , lsDeadlineAborts = 0
            , lsFirstByteLatency = Nothing
            , lsLatency = Percentiles (Just p50) Nothing Nothing (Just p99) Nothing
            , lsTransferredBodyBytes = Nothing
            , lsNote = ""
            }

-- | A loaded-pass section under 'twoCores' and 'slowNetwork', with this injected latency in microseconds.
loadedSection :: Ecosystem -> Int -> [ScenarioReport] -> Section
loadedSection ecosystem latencyMicros loaded =
    Section
        { sectionKnobs = defaultLoadKnobs{lkUpstreamLatencyMicros = latencyMicros}
        , sectionCapabilities = 3
        , sectionProcessors = 4
        , sectionShape = twoCores
        , sectionEcosystem = ecosystem
        , sectionEnforcement = slowNetwork
        , sectionConcurrencyOne = []
        , sectionLoaded = loaded
        }

-- | A proxy that ended in a clean shutdown with this cgroup memory peak in bytes, under a 1 GiB limit.
proxyWithPeak :: Int -> ProxyFigures
proxyWithPeak peak =
    ProxyFigures
        { pfIdleRts = Nothing
        , pfIdleCgroupBytes = Nothing
        , pfCgroup = Just (CgroupReading (Just (1024 * 1024 * 1024)) (Just peak) Nothing mempty mempty mempty Nothing)
        , pfWindowCgroup = Nothing
        , pfWindowThrottledUsec = Nothing
        , pfCpu = Nothing
        , pfEnding = CleanShutdown
        , pfExitedEarly = False
        , pfStderrTail = ""
        , pfBootLines = []
        , pfRuleLines = []
        , pfBootRetries = []
        , pfInFlight = summariseGauge []
        , pfTasks = summariseGauge []
        , pfMemoryChargeBytes = Nothing
        , pfAdmissionSeries = []
        , pfCacheWindow = Nothing
        , pfBurstMemory = Nothing
        }

-- | A 30 s window with the proxy on CPUs 2-3, the harness cgroup at half of CPUs 0-1, and CPU 0 this many of 2000 ticks busy.
placedWindow :: Int -> CpuWindow
placedWindow busy = CpuWindow 30 (rightToMaybe (parseCpuList "2-3")) (Just 45) (rightToMaybe (parseCpuList "0-1")) (Just 30) (Map.fromList [(0, CpuTicks busy 2_000), (2, CpuTicks 1_600 2_000), (3, CpuTicks 1_600 2_000)]) 0.01

-- | A scalar runtime fixture for window arithmetic and evidence availability controls.
rtsSnapshot :: RtsSnapshot
rtsSnapshot =
    RtsSnapshot
        { rsAllocatedBytes = 1_000
        , rsGcs = 10
        , rsMajorGcs = 2
        , rsGcCpuNs = 100
        , rsCpuNs = 1_000
        , rsGcElapsedNs = 50
        , rsMaxLiveBytes = 0
        , rsMaxMemInUseBytes = 0
        , rsMaxLargeObjectsBytes = 0
        , rsCumulativeLiveBytes = 0
        , rsLiveBytes = 0
        , rsMemInUseBytes = 0
        , rsCapabilities = 2
        , rsProcessors = 4
        , rsMaxHeapBytes = Nothing
        , rsAllocAreaBytes = 0
        , rsCompactAlways = False
        , rsCompactThresholdPercent = 30
        }
