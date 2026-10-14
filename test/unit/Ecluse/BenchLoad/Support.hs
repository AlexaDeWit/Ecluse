-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared load-spec pod shapes, floors, enforcement, and scenario reports.
module Ecluse.BenchLoad.Support (
    twoCores,
    floorsAtTwoCores,
    slowNetwork,
    scenarioReport,
) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Floors (Enforcement (NotHeld), FloorKey, Pass (ConcurrencyOne, Loaded), Unheld (LatencyAboveCeiling))
import Ecluse.BenchLoad.Harness (LoadSummary (..), ScenarioReport (..))
import Ecluse.BenchLoad.Latency (Percentiles (Percentiles))
import Ecluse.BenchLoad.Pod (PodShape (Limited))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

-- | A two-CPU quota and a 1 GiB memory bound.
twoCores :: PodShape
twoCores = Limited 2 (1024 * 1024 * 1024)

-- | Example npm and PyPI floors for the bounded pod.
floorsAtTwoCores :: Map FloorKey Int
floorsAtTwoCores =
    Map.fromList
        [ (("npm/merge-cold", Loaded), 338)
        , (("npm/merge-cold", ConcurrencyOne), 46)
        , (("npm/herd", Loaded), 10)
        , (("pypi/index-cold", Loaded), 964)
        , (("pypi/index-cold", ConcurrencyOne), 134)
        ]

-- | A run whose latency exceeds its calibrated ceiling.
slowNetwork :: Enforcement
slowNetwork = NotHeld (LatencyAboveCeiling Npm 444 210 :| [])

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
            , lsLatency = Percentiles (Just p50) Nothing (Just p99) Nothing
            , lsNote = ""
            }
