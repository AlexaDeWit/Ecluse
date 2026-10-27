-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load observations preserve floor enforcement without changing the run's verdict.
module Ecluse.BenchLoad.Evidence (
    loadEvidence,
    VerifiedRelayMeasurement (..),
    VerifiedResponseCounts (..),
    loadEvidenceWithVerified,
    verifiedRelayRequiredRows,
    verifiedRelayProblems,
) where

import Data.Aeson (ToJSON (toJSON), Value (Null), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.Contract (LoadLaw (VerifiedKnownLengthArtifact, VerifiedSlowArtifact), loadScenarioName)
import Ecluse.BenchLoad.Exposition (ArtifactWindow (..), GaugeSummary (..))
import Ecluse.BenchLoad.Floors (Enforcement (Enforced, NotHeld), FloorKey, floorCheck, passKey)
import Ecluse.BenchLoad.Latency (Percentiles (..))
import Ecluse.BenchLoad.Pod (CgroupReading)
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (rsMaxLiveBytes), RtsWindow (rwAllocatedBytes))
import Ecluse.BenchLoad.Selection (scenarioKey)
import Ecluse.BenchLoad.Verdict (RunEvidence, runViolations)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))

-- | Record actual enforcement after measurement, including every reason a diagnostic run is not held.
loadEvidence :: Map Text Text -> Enforcement -> [Text] -> [(FloorKey, Either Text RunEvidence)] -> Value
loadEvidence = loadEvidenceWithVerified []

-- | Bind native resource observations to their exact scenario and pass without replacing workload counts.
loadEvidenceWithVerified :: [VerifiedRelayMeasurement] -> Map Text Text -> Enforcement -> [Text] -> [(FloorKey, Either Text RunEvidence)] -> Value
loadEvidenceWithVerified measured conditions enforcement violations outcomes =
    object
        [ "outcome" .= verdict (null violations && null (verifiedRelayProblems measured (map fst outcomes)))
        , "conditions" .= Map.union floorConditions conditions
        , "verifiedRelayMeasurements" .= measured
        , "observations"
            .= ( [ object
                    [ "id" .= (scenario <> "/" <> passKey whichPass)
                    , "outcome" .= verdict (either (const False) (null . runViolations (floorCheck enforcement key)) result)
                    ]
                 | (key@(scenario, whichPass), result) <- outcomes
                 ]
                    <> [ object
                            [ "id" .= verifiedRow key
                            , "outcome" .= verdict (null (problemsFor measured key))
                            , "values" .= maybe Null toJSON (find ((== key) . vrmKey) measured)
                            ]
                       | key <- map fst outcomes
                       , required key
                       ]
               )
        ]
  where
    floorConditions = case enforcement of
        Enforced _ _ -> Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")]
        NotHeld reasons -> Map.fromList [("floorEnforcement", "not-held"), ("floorEnforcementReasons", T.intercalate ", " (map show (toList reasons)))]
    verdict passed = if passed then ("passed" :: Text) else "failed"

-- | Actual completed statuses and transport failures for one measured load population.
data VerifiedResponseCounts = VerifiedResponseCounts
    { vrcCompleted :: Int
    , vrcSuccessful :: Int
    , vrcRefused :: Int
    , vrcOtherStatuses :: Int
    , vrcTransportFailures :: Int
    , vrcDeadlineAborts :: Int
    }
    deriving stock (Generic)

instance ToJSON VerifiedResponseCounts

-- | Wire reservations and global serve charges remain distinct from physical runtime observations.
data VerifiedRelayMeasurement = VerifiedRelayMeasurement
    { vrmKey :: FloorKey
    , vrmExpectedBodyBytes :: Int
    , vrmElapsedSeconds :: Double
    , vrmMeasuredResponses :: VerifiedResponseCounts
    , vrmCompanionResponses :: Maybe VerifiedResponseCounts
    , vrmWarmupSettled :: Maybe Bool
    , vrmCounters :: Maybe ArtifactWindow
    , vrmWireReservation :: Maybe GaugeSummary
    , vrmAllServeMemoryCharges :: Maybe GaugeSummary
    , vrmFirstBodyByteAllResponses :: Maybe Percentiles
    , vrmSuccessfulDuration :: Percentiles
    , vrmAllocationWindow :: Maybe RtsWindow
    , vrmProcessRtsAtEnd :: Maybe RtsSnapshot
    , vrmRetainedHeapAfterGc :: Maybe Word64
    , vrmCgroupAtEnd :: Maybe CgroupReading
    }

instance ToJSON VerifiedRelayMeasurement where
    toJSON value =
        object
            [ "scenario" .= fst (vrmKey value)
            , "pass" .= passKey (snd (vrmKey value))
            , "ownedFixtureBodyBytes" .= vrmExpectedBodyBytes value
            , "measuredElapsedSeconds" .= vrmElapsedSeconds value
            , "measuredResponseCounts" .= vrmMeasuredResponses value
            , "companionResponseCounts" .= vrmCompanionResponses value
            , "warmupVerifiedReservationsSettled" .= vrmWarmupSettled value
            , "counterDeltas" .= vrmCounters value
            , "sampledWireReservationBytes" .= vrmWireReservation value
            , "sampledAllServeMemoryChargeBytes" .= vrmAllServeMemoryCharges value
            , "firstBodyByteAllCompletedResponsesIncludingRefusals" .= vrmFirstBodyByteAllResponses value
            , "measuredSuccessfulDuration" .= vrmSuccessfulDuration value
            , "allocationWindow" .= vrmAllocationWindow value
            , "processRuntimeAtWindowEnd" .= vrmProcessRtsAtEnd value
            , "retainedHeapBytesAfterGc" .= vrmRetainedHeapAfterGc value
            , "cgroupAtWindowEnd" .= vrmCgroupAtEnd value
            ]

-- | The reviewed known-length and slow-client cases require resource evidence beside successful counts.
verifiedRelayRequiredRows :: [FloorKey] -> [Text]
verifiedRelayRequiredRows keys = [verifiedRow key | key <- keys, required key]

-- | Missing or reset observations fail after actual workload results remain available for review.
verifiedRelayProblems :: [VerifiedRelayMeasurement] -> [FloorKey] -> [Text]
verifiedRelayProblems measured keys = concat [map ((verifiedRow key <> ": ") <>) (problemsFor measured key) | key <- keys, required key]

required :: FloorKey -> Bool
required (name, _) = name `elem` [scenarioKey RubyGems native | law <- [VerifiedKnownLengthArtifact, VerifiedSlowArtifact], native <- maybeToList (loadScenarioName RubyGems law)]

verifiedRow :: FloorKey -> Text
verifiedRow (name, whichPass) = name <> "/" <> passKey whichPass <> "/verified-relay-observations"

problemsFor :: [VerifiedRelayMeasurement] -> FloorKey -> [Text]
problemsFor measured key = case filter ((== key) . vrmKey) measured of
    [] -> ["missing native verified-relay observations"]
    [value] ->
        ["verified reservation warm-up did not settle" | vrmWarmupSettled value /= Just True]
            <> ["missing measured elapsed time" | not (positive (Just (vrmElapsedSeconds value)))]
            <> ["inconsistent measured response population" | not (validResponses (vrmMeasuredResponses value))]
            <> ["missing successful slow-reader companion" | isSlow key && not (maybe False (\counts -> validResponses counts && vrcSuccessful counts > 0) (vrmCompanionResponses value))]
            <> counterProblems value (vrmCounters value)
            <> ["missing sampled wire reservation" | not (observedGauge (vrmWireReservation value))]
            <> ["missing sampled global serve memory charges" | not (observedGauge (vrmAllServeMemoryCharges value))]
            <> ["missing all-response first-body-byte percentiles" | not (observedLatency (vrmFirstBodyByteAllResponses value))]
            <> ["missing successful-duration p95/p99" | not (valid (pP95Ms (vrmSuccessfulDuration value)) && valid (pP99Ms (vrmSuccessfulDuration value)))]
            <> ["missing positive measured allocation window" | maybe True ((== 0) . rwAllocatedBytes) (vrmAllocationWindow value)]
            <> ["missing positive actual process heap observation" | maybe True ((== 0) . rsMaxLiveBytes) (vrmProcessRtsAtEnd value)]
            <> ["missing retained heap observation" | isNothing (vrmRetainedHeapAfterGc value)]
    _ -> ["duplicate native verified-relay observations"]
  where
    isSlow (name, _) = Just name == (scenarioKey RubyGems <$> loadScenarioName RubyGems VerifiedSlowArtifact)
    validResponses counts =
        all (>= 0) [vrcCompleted counts, vrcSuccessful counts, vrcRefused counts, vrcOtherStatuses counts, vrcTransportFailures counts, vrcDeadlineAborts counts]
            && vrcCompleted counts == vrcSuccessful counts + vrcRefused counts + vrcOtherStatuses counts
    counterProblems value = \case
        Nothing -> ["missing actual read/hash/verification counters"]
        Just counters ->
            [label | (label, observed) <- [("missing positive source-byte delta", awReadBytes counters), ("missing positive completed-hash-byte delta", awHashBytes counters), ("missing positive verified-body delta", awVerified counters)], not (positive observed)]
                <> [label | (label, observed) <- [("missing observed queue delta", awQueued counters), ("missing observed shed delta", awShed counters)], not (valid observed)]
                <> ["verified body count is below actual successful responses" | maybe True (< fromIntegral (successful value)) (awVerified counters)]
                <> ["completed read/hash bytes are below the successful owned payloads" | vrmExpectedBodyBytes value <= 0 || any (maybe True (< fromInteger (toInteger (vrmExpectedBodyBytes value) * toInteger (successful value)))) [awReadBytes counters, awHashBytes counters]]
    successful value = vrcSuccessful (vrmMeasuredResponses value) + maybe 0 vrcSuccessful (vrmCompanionResponses value)
    valid = maybe False (\value -> value >= 0 && not (isNaN value || isInfinite value))
    positive value = valid value && maybe False (> 0) value
    observedGauge = maybe False (\gauge -> gsSamples gauge > 0 && valid (gsMax gauge))
    observedLatency = maybe False (\latency -> all valid [pP50Ms latency, pP95Ms latency, pP99Ms latency])
