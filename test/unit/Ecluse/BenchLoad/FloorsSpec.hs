-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The floors file's reader, what holds a run to the floors or keeps it off them, and what a held run fails closed on.
module Ecluse.BenchLoad.FloorsSpec (spec) where

import Data.Aeson (Value (Null), encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Calibration (..),
    CalibrationRun (CalibrationRun),
    Enforcement (Enforced, NotHeld),
    FloorCheck (AtLeast, NoFloor, Unchecked),
    FloorKey,
    Floors (..),
    LatencyCeiling (LatencyCeiling),
    OperatingPoint (..),
    Pass (ConcurrencyOne, Loaded),
    RunFacts (..),
    RunnerKind (RunnerKind),
    Trigger (OnDemand, Scheduled),
    Unheld (LatencyAboveCeiling, OtherRunner, SettingsDiffer, ShapeNotCalibrated),
    decodeFloors,
    describeEnforcement,
    enforce,
    floorCheck,
    latencyCeilingMs,
    patternOverridesIn,
    runnerIn,
    staleFloorViolations,
    triggerIn,
    unheldViolations,
 )
import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited))

spec :: Spec
spec = do
    describe "decodeFloors" $ do
        it "reads the calibration, and each floor by pod shape, scenario, and pass" $
            decodeFloors (document (calibrationValue []) twoCoreFloors) `shouldBe` Right sample
        it "reads a null bound as the proxy's computed default, and lists as a scenario subset and pattern overrides" $
            (calOperatingPoint . floorsCalibration <$> decodeFloors (document (calibrationValue ["operatingPoint" .= operatingPointValue ["serveMaxInFlight" .= (30 :: Int), "scenarios" .= ["pypi/index-cold" :: Text], "patternOverrides" .= ["BENCH_PATTERN_NAMES" :: Text]]]) twoCoreFloors))
                `shouldBe` Right calibrated{opServeMaxInFlight = Just 30, opScenarios = Just ["pypi/index-cold"], opPatternOverrides = ["BENCH_PATTERN_NAMES"]}
        it "refuses an operating point that leaves any setting out, a nullable one included" $
            for_ (map fst operatingPointPairs) $ \missing ->
                decodeFloors (document (calibrationValue ["operatingPoint" .= object (filter ((/= missing) . fst) operatingPointPairs)]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a calibration without its rule, its runner, or its runs" $
            for_ [["rule" .= ("" :: Text)], ["runner" .= ("" :: Text)], ["runnerOs" .= ("" :: Text)], ["runnerArch" .= ("" :: Text)], ["runs" .= ([] :: [Text])]] $ \override ->
                decodeFloors (document (calibrationValue override) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a run without its URL or its commit" $
            for_ [object ["url" .= ("" :: Text), "commit" .= commit], object ["url" .= firstRun, "commit" .= ("" :: Text)], object ["url" .= firstRun]] $ \run ->
                decodeFloors (document (calibrationValue ["runs" .= [run]]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a latency ceiling that is not what the rule gives for the highest latency" $
            for_ [(165, 240), (165, 260), (0, 0)] $ \(highest, most) ->
                decodeFloors (document (calibrationValue ["npmInjectedLatency" .= object ["rule" .= ceilingRule, "highestMs" .= (highest :: Int), "ceilingMs" .= (most :: Int)]]) twoCoreFloors)
                    `shouldSatisfy` isLeft
        it "refuses a pod shape it cannot read, and one not in its rendered form" $
            for_ ["2cpu", "2CPU-1GIB", "2cpu-1024mib"] $ \shape ->
                decodeFloors (document (calibrationValue []) [shape .= object ["npm/merge-cold" .= object ["loaded" .= (1 :: Int)]]]) `shouldSatisfy` isLeft
        it "refuses an unknown pass" $
            decodeFloors (document (calibrationValue []) ["2cpu-1gib" .= object ["pypi/index-cold" .= object ["warm" .= (1 :: Int)]]]) `shouldSatisfy` isLeft
        it "refuses a floor below 1" $
            decodeFloors (document (calibrationValue []) ["2cpu-1gib" .= object ["npm/merge-cold" .= object ["loaded" .= (0 :: Int)]]]) `shouldSatisfy` isLeft

    describe "latencyCeilingMs" $
        it "is one and a half times the highest latency, rounded up to 10 ms" $
            map latencyCeilingMs [165, 160, 161, 100] `shouldBe` [250, 240, 250, 150]

    describe "what a run brings" $ do
        it "reads a scheduled run from GitHub's event name, and any other run as on demand" $ do
            triggerIn (Map.singleton "GITHUB_EVENT_NAME" "schedule") `shouldBe` Scheduled
            triggerIn (Map.singleton "GITHUB_EVENT_NAME" "workflow_dispatch") `shouldBe` OnDemand
            triggerIn Map.empty `shouldBe` OnDemand
        it "reads the runner only on GitHub Actions, and only when it names its system and architecture" $ do
            runnerIn (Map.fromList githubRunner) `shouldBe` Just calibratedRunner
            runnerIn (Map.fromList (drop 1 githubRunner)) `shouldBe` Nothing
            runnerIn (Map.fromList (take 2 githubRunner)) `shouldBe` Nothing
        it "names the request-pattern variables an environment sets, and no other" $
            patternOverridesIn (Map.fromList [("BENCH_PATTERN_ROUNDS", "8"), ("BENCH_LOAD_CONCURRENCY", "1"), ("BENCH_PATTERN_NAMES", "1")])
                `shouldBe` ["BENCH_PATTERN_NAMES", "BENCH_PATTERN_ROUNDS"]

    describe "enforce" $ do
        it "holds a run that matches the calibration to its pod shape's floors" $
            enforce sample held `shouldBe` Enforced twoCores floorsAtTwoCores
        it "holds a run whose npm fixture injects the ceiling, and not one a millisecond above it" $ do
            enforce sample held{rfNpmLatencyMs = 250} `shouldBe` Enforced twoCores floorsAtTwoCores
            enforce sample held{rfNpmLatencyMs = 251} `shouldBe` NotHeld (LatencyAboveCeiling 251 250 :| [])
        it "does not hold a run that differs in any setting, and names the setting" $
            for_ offCalibration $ \(setting, ran) ->
                enforce sample held{rfSettings = ran} `shouldBe` NotHeld (SettingsDiffer (setting :| []) :| [])
        it "does not hold a run off the calibrated runner, on GitHub Actions or not" $
            for_ [Nothing, Just (RunnerKind "Linux" "X64")] $ \runner ->
                enforce sample held{rfRunner = runner} `shouldBe` NotHeld (OtherRunner "ubuntu-26.04-arm" calibratedRunner :| [])
        it "does not hold a run under a pod shape the floors do not hold, and names the shape once" $
            enforce sample held{rfShape = Unlimited} `shouldBe` NotHeld (ShapeNotCalibrated Unlimited :| [])
        it "names every reason a run is not held" $
            enforce sample RunFacts{rfSettings = calibrated{opDurationSeconds = 10, opScenarios = Just ["pypi/index-cold"]}, rfRunner = Nothing, rfShape = Unlimited, rfNpmLatencyMs = 444}
                `shouldBe` NotHeld (ShapeNotCalibrated Unlimited :| [SettingsDiffer ("durationSeconds" :| ["scenarios"]), OtherRunner "ubuntu-26.04-arm" calibratedRunner, LatencyAboveCeiling 444 250])

    describe "floorCheck" $ do
        it "gives a held run each count's floor, for npm and PyPI alike, and none for a count the floors do not cover" $ do
            floorCheck (Enforced twoCores floorsAtTwoCores) ("npm/merge-cold", Loaded) `shouldBe` AtLeast 338
            floorCheck (Enforced twoCores floorsAtTwoCores) ("npm/merge-cold", ConcurrencyOne) `shouldBe` AtLeast 46
            floorCheck (Enforced twoCores floorsAtTwoCores) ("pypi/index-cold", Loaded) `shouldBe` AtLeast 964
            floorCheck (Enforced twoCores floorsAtTwoCores) ("pypi/index-cold", ConcurrencyOne) `shouldBe` AtLeast 134
            floorCheck (Enforced twoCores floorsAtTwoCores) ("npm/herd", ConcurrencyOne) `shouldBe` NoFloor ConcurrencyOne
        it "checks nothing for a run that is not held" $
            floorCheck slowNetwork ("pypi/index-cold", Loaded) `shouldBe` Unchecked

    describe "staleFloorViolations" $ do
        it "passes floors that match the counts the run checks" $
            staleFloorViolations (Enforced twoCores floorsAtTwoCores) (Map.keysSet floorsAtTwoCores) `shouldBe` []
        it "fails a held run on a floor for a count it does not check" $
            staleFloorViolations (Enforced twoCores floorsAtTwoCores) (Set.delete ("pypi/index-cold", ConcurrencyOne) (Map.keysSet floorsAtTwoCores))
                `shouldBe` ["bench/load/floors.json: the concurrencyOne floor for pypi/index-cold under 2cpu-1gib names no count this run checks"]
        it "checks nothing for a run that is not held" $
            staleFloorViolations slowNetwork Set.empty `shouldBe` []

    describe "unheldViolations" $ do
        it "fails a scheduled run that its settings, its runner, or its pod shape keep off the floors, in one line" $
            unheldViolations Scheduled (NotHeld (ShapeNotCalibrated Unlimited :| [SettingsDiffer ("durationSeconds" :| ["scenarios"]), OtherRunner "ubuntu-26.04-arm" calibratedRunner, LatencyAboveCeiling 444 250]))
                `shouldBe` [ "a scheduled run must be held to the success floors, and this one is not: the floors hold no entry for the pod shape unlimited, and it differs from the calibrated operating point in durationSeconds, scenarios, and it does not run on the calibrated runner (ubuntu-26.04-arm: GitHub Actions on Linux ARM64)"
                           ]
        it "passes a scheduled run that only a slow network keeps off the floors" $
            unheldViolations Scheduled slowNetwork `shouldBe` []
        it "passes a scheduled run that is held, and any run on demand" $ do
            unheldViolations Scheduled (Enforced twoCores floorsAtTwoCores) `shouldBe` []
            unheldViolations OnDemand (NotHeld (SettingsDiffer ("concurrency" :| []) :| [])) `shouldBe` []

    describe "describeEnforcement" $ do
        it "names the file, the runner, the runs, their commits, the rule, and the latency ceiling for a held run" $
            describeEnforcement calibration (Enforced twoCores floorsAtTwoCores)
                `shouldBe` "This run is held to the success floors in `bench/load/floors.json`, calibrated on ubuntu-26.04-arm from 2 runs at abc123. Half the lowest night. A run whose npm fixture injects more than 250 ms of upstream latency is not held."
        it "gives the injected latency and the ceiling for a run a slow network keeps off the floors" $
            describeEnforcement calibration slowNetwork
                `shouldBe` "This run is not held to the success floors in `bench/load/floors.json`: its npm fixture injects 444 ms of upstream latency, above the ceiling of 250 ms."
        it "names every reason a run is not held" $
            describeEnforcement calibration (NotHeld (SettingsDiffer ("durationSeconds" :| ["scenarios"]) :| [OtherRunner "ubuntu-26.04-arm" calibratedRunner]))
                `shouldBe` "This run is not held to the success floors in `bench/load/floors.json`: it differs from the calibrated operating point in durationSeconds, scenarios, and it does not run on the calibrated runner (ubuntu-26.04-arm: GitHub Actions on Linux ARM64)."

rule, ceilingRule, commit, firstRun, secondRun :: Text
rule = "Half the lowest night."
ceilingRule = "One and a half times the highest."
commit = "abc123"
firstRun = "https://example.test/runs/1"
secondRun = "https://example.test/runs/2"

calibrated :: OperatingPoint
calibrated = OperatingPoint 30 100 363520 5 3 64 Nothing Nothing Nothing Nothing []

calibratedRunner :: RunnerKind
calibratedRunner = RunnerKind "Linux" "ARM64"

-- What GitHub Actions sets on the calibrated runner.
githubRunner :: [(Text, Text)]
githubRunner = [("GITHUB_ACTIONS", "true"), ("RUNNER_OS", "Linux"), ("RUNNER_ARCH", "ARM64")]

calibration :: Calibration
calibration =
    Calibration
        { calRule = rule
        , calRunner = "ubuntu-26.04-arm"
        , calRunnerKind = calibratedRunner
        , calRuns = CalibrationRun firstRun commit :| [CalibrationRun secondRun commit]
        , calOperatingPoint = calibrated
        , calNpmLatency = LatencyCeiling ceilingRule 165 250
        }

twoCores :: PodShape
twoCores = Limited 2 (1024 * 1024 * 1024)

floorsAtTwoCores :: Map FloorKey Int
floorsAtTwoCores =
    Map.fromList
        [ (("npm/merge-cold", Loaded), 338)
        , (("npm/merge-cold", ConcurrencyOne), 46)
        , (("npm/herd", Loaded), 10)
        , (("pypi/index-cold", Loaded), 964)
        , (("pypi/index-cold", ConcurrencyOne), 134)
        ]

sample :: Floors
sample = Floors calibration (Map.singleton twoCores floorsAtTwoCores)

-- A run that matches the sample's calibration under its one pod shape.
held :: RunFacts
held = RunFacts{rfSettings = calibrated, rfRunner = Just calibratedRunner, rfShape = twoCores, rfNpmLatencyMs = 165}

slowNetwork :: Enforcement
slowNetwork = NotHeld (LatencyAboveCeiling 444 250 :| [])

-- One run per setting, each differing from the calibrated operating point in that setting alone.
offCalibration :: [(Text, OperatingPoint)]
offCalibration =
    [ ("durationSeconds", calibrated{opDurationSeconds = 10})
    , ("concurrency", calibrated{opConcurrency = 50})
    , ("payloadBytes", calibrated{opPayloadBytes = 1024})
    , ("upstreamLatencyMs", calibrated{opUpstreamLatencyMs = 50})
    , ("cacheMaxEntries", calibrated{opCacheMaxEntries = 8})
    , ("workingSet", calibrated{opWorkingSet = 3})
    , ("serveMaxInFlight", calibrated{opServeMaxInFlight = Just 30})
    , ("publicConnectionsPerHost", calibrated{opPublicConnectionsPerHost = Just 8})
    , ("privateConnectionsPerHost", calibrated{opPrivateConnectionsPerHost = Just 8})
    , ("scenarios", calibrated{opScenarios = Just ["pypi/index-cold"]})
    , ("patternOverrides", calibrated{opPatternOverrides = ["BENCH_PATTERN_NAMES"]})
    ]

twoCoreFloors :: [Pair]
twoCoreFloors =
    [ "2cpu-1gib"
        .= object
            [ "npm/merge-cold" .= object ["loaded" .= (338 :: Int), "concurrencyOne" .= (46 :: Int)]
            , "npm/herd" .= object ["loaded" .= (10 :: Int)]
            , "pypi/index-cold" .= object ["loaded" .= (964 :: Int), "concurrencyOne" .= (134 :: Int)]
            ]
    ]

-- The sample's calibration record, with each override in place of the field of its key.
calibrationValue :: [Pair] -> Value
calibrationValue overrides =
    overriding
        overrides
        [ "rule" .= rule
        , "runner" .= ("ubuntu-26.04-arm" :: Text)
        , "runnerOs" .= ("Linux" :: Text)
        , "runnerArch" .= ("ARM64" :: Text)
        , "runs" .= [object ["url" .= run, "commit" .= commit] | run <- [firstRun, secondRun]]
        , "operatingPoint" .= operatingPointValue []
        , "npmInjectedLatency" .= object ["rule" .= ceilingRule, "highestMs" .= (165 :: Int), "ceilingMs" .= (250 :: Int)]
        ]

-- The calibrated operating point, with each override in place of the setting of its key.
operatingPointValue :: [Pair] -> Value
operatingPointValue overrides = overriding overrides operatingPointPairs

operatingPointPairs :: [Pair]
operatingPointPairs =
    [ "durationSeconds" .= (30 :: Int)
    , "concurrency" .= (100 :: Int)
    , "payloadBytes" .= (363520 :: Int)
    , "upstreamLatencyMs" .= (5 :: Int)
    , "cacheMaxEntries" .= (3 :: Int)
    , "workingSet" .= (64 :: Int)
    , "serveMaxInFlight" .= Null
    , "publicConnectionsPerHost" .= Null
    , "privateConnectionsPerHost" .= Null
    , "scenarios" .= Null
    , "patternOverrides" .= ([] :: [Text])
    ]

overriding :: [Pair] -> [Pair] -> Value
overriding overrides base = object (Map.toList (Map.union (Map.fromList overrides) (Map.fromList base)))

document :: Value -> [Pair] -> LByteString
document calibrationRecord shapes = encode (object ["calibration" .= calibrationRecord, "floors" .= object shapes])
