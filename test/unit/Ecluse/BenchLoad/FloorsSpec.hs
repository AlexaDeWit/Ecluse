-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The floors file's reader, what holds a run to the floors or keeps it off them, and what a held run fails closed on.
module Ecluse.BenchLoad.FloorsSpec (spec) where

import Data.Aeson (Key, Value (Null), encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Calibration (..),
    CalibrationRun (CalibrationRun),
    Cell (Floored, Uncalibrated),
    Enforcement (Enforced, NotHeld),
    FloorCheck (AtLeast, NoFloor, ReportOnly, Unchecked),
    Floors (..),
    OperatingPoint (..),
    Pass (ConcurrencyOne, Loaded),
    PassSelection (EveryPass, LoadedPassOnly),
    RunFacts (..),
    RunnerKind (RunnerKind),
    RunnerReading (RunnerReading),
    Trigger (OnDemand, Scheduled),
    Unheld (ConditionsDiffer, OtherRunner, SettingsDiffer, ShapeNotCalibrated, ThrashProbe),
    checkedFloor,
    decodeFloors,
    describeEnforcement,
    enforce,
    floorCheck,
    floorFromMedian,
    medianCount,
    patternOverridesIn,
    renderFloorsFile,
    runnerIn,
    staleFloorViolations,
    triggerIn,
    unheldViolations,
 )
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), ProxyTuning (..), noTuning, parseCpuList)
import Ecluse.BenchLoad.Support (calibrationRunIds, floorsAtTwoCores, otherLatency, twoCores)

spec :: Spec
spec = do
    describe "decodeFloors" $ do
        it "reads the calibration, and each cell by pod shape, scenario, and pass" $
            decodeFloors (document (calibrationValue []) twoCoreFloors) `shouldBe` Right sample
        it "reads a null bound as the proxy's computed default, and lists as a scenario subset and pattern overrides" $
            (calOperatingPoint . floorsCalibration <$> decodeFloors (document (calibrationValue ["operatingPoint" .= operatingPointValue ["serveMaxInFlight" .= (30 :: Int), "scenarios" .= ["pypi/index-cold" :: Text], "patternOverrides" .= ["BENCH_PATTERN_NAMES" :: Text]]]) twoCoreFloors))
                `shouldBe` Right calibrated{opServeMaxInFlight = Just 30, opScenarios = Just ["pypi/index-cold"], opPatternOverrides = ["BENCH_PATTERN_NAMES"]}
        it "refuses an operating point that leaves any setting out, a nullable one included" $
            for_ (map fst operatingPointPairs) $ \missing ->
                decodeFloors (document (calibrationValue ["operatingPoint" .= object (filter ((/= missing) . fst) operatingPointPairs)]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a calibration that leaves any key out" $
            for_ (map fst calibrationPairs) $ \missing ->
                decodeFloors (document (object (filter ((/= missing) . fst) calibrationPairs)) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a calibration without its measurement conditions or its runner, a floor fraction outside (0, 1], and negative confirmation re-runs" $
            for_ [["measurementConditions" .= ("" :: Text)], ["runner" .= ("" :: Text)], ["runnerOs" .= ("" :: Text)], ["runnerArch" .= ("" :: Text)], ["floorFraction" .= (0 :: Int)], ["floorFraction" .= (1.1 :: Double)], ["confirmationReruns" .= (-1 :: Int)]] $ \override ->
                decodeFloors (document (calibrationValue override) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a run without its ID, its URL, its commit, or its event, and a run listed twice" $ do
            for_ [runValue 0 firstRun, object ["id" .= (1 :: Int), "url" .= ("" :: Text), "commit" .= commit, "event" .= event], object ["id" .= (1 :: Int), "url" .= firstRun, "event" .= event], object ["id" .= (1 :: Int), "url" .= firstRun, "commit" .= commit, "event" .= ("" :: Text)]] $ \run ->
                decodeFloors (document (calibrationValue ["runs" .= (run : drop 1 runValues)]) twoCoreFloors) `shouldSatisfy` isLeft
            decodeFloors (document (calibrationValue ["runs" .= (runValues <> take 1 runValues)]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a pod shape it cannot read, and one not in its rendered form" $
            for_ ["2 cpus", "2CPU-1GIB", "2cpu-1024mib"] $ \shape ->
                decodeFloors (document (calibrationValue ["runs" .= ([] :: [Value])]) [shape .= object ["npm/merge-cold" .= object ["loaded" .= object ["floor" .= Null]]]]) `shouldSatisfy` isLeft
        it "refuses an unknown pass" $
            decodeFloors (document (calibrationValue []) (withCell "pypi/index-cold" "warm" (floorValue 964 1607))) `shouldSatisfy` isLeft
        it "refuses a floor below 1" $
            for_ [(0, 0), (0, 1)] $ \(least, median) ->
                decodeFloors (document (calibrationValue []) (withCell "npm/merge-cold" "loaded" (floorValue least median))) `shouldSatisfy` isLeft
        it "refuses a floor that is not 60% of its median, rounded down" $
            for_ [(339, 564), (337, 564), (338, 565)] $ \(least, median) ->
                decodeFloors (document (calibrationValue []) (withCell "npm/merge-cold" "loaded" (floorValue least median))) `shouldSatisfy` isLeft
        it "reads each floor at the calibration's floor fraction" $ do
            let atHalf = calibrationValue ["floorFraction" .= (0.5 :: Double)]
            fmap (Map.lookup ("npm/merge-cold", Loaded) <=< Map.lookup twoCores . floorsByShape) (decodeFloors (document atHalf [twoCoreKey .= object ["npm/merge-cold" .= object ["loaded" .= floorValue 282 564]]]))
                `shouldSatisfy` either (const False) (== Just (Floored 282 564 calibrationRunIds))
            decodeFloors (document atHalf twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a floor without its median or its runs, and a cell with a floor and a report-only reason"
            $ for_
                [ object ["floor" .= (338 :: Int), "from" .= calibrationRunIds]
                , object ["floor" .= (338 :: Int), "median" .= (564 :: Int)]
                , object ["floor" .= (338 :: Int), "median" .= (564 :: Int), "from" .= ([] :: [Int])]
                , object ["floor" .= (338 :: Int), "median" .= (564 :: Int), "from" .= calibrationRunIds, "reportOnly" .= ("noisy" :: Text)]
                , object ["floor" .= Null, "median" .= (677 :: Int)]
                , object []
                ]
            $ \cell -> decodeFloors (document (calibrationValue []) (withCell "npm/merge-cold" "loaded" cell)) `shouldSatisfy` isLeft
        it "refuses a floor that names a run the calibration does not list, or a run twice, and a listed run no floor names" $ do
            decodeFloors (document (calibrationValue []) (withCell "npm/merge-cold" "loaded" (object ["floor" .= (338 :: Int), "median" .= (564 :: Int), "from" .= [1, 2, 4 :: Int]]))) `shouldSatisfy` isLeft
            decodeFloors (document (calibrationValue []) (withCell "npm/merge-cold" "loaded" (object ["floor" .= (338 :: Int), "median" .= (564 :: Int), "from" .= [1, 2, 2 :: Int]]))) `shouldSatisfy` isLeft
            decodeFloors (document (calibrationValue ["runs" .= (runValues <> [runValue 4 "https://example.test/runs/4"])]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a report-only cell without its reason" $
            decodeFloors (document (calibrationValue []) (withCell "npm/herd" "loaded" (object ["floor" .= Null, "reportOnly" .= ("" :: Text)]))) `shouldSatisfy` isLeft
        it "reads uncalibrated cells with no calibration run" $
            fmap floorsByShape (decodeFloors (document (calibrationValue ["runs" .= ([] :: [Value])]) [twoCoreKey .= object ["npm/merge-cold" .= object ["loaded" .= object ["floor" .= Null]]]]))
                `shouldBe` Right (Map.singleton twoCores (Map.singleton ("npm/merge-cold", Loaded) Uncalibrated))

    describe "floorFromMedian" $ do
        it "is 60% of the median count, rounded down, and at least 1" $
            map (floorFromMedian 0.6) [564, 677, 1000, 2, 1] `shouldBe` [338, 406, 600, 1, 1]
        it "takes the floor fraction it is given" $
            map (floorFromMedian (1 / 2)) [677, 1000, 1] `shouldBe` [338, 500, 1]
        it "takes the middle count of the runs, the lower middle of an even number" $
            map medianCount [3 :| [1, 2], 2410 :| [1980, 2205], 5 :| [9, 1, 7]] `shouldBe` [2, 2205, 5]

    describe "renderFloorsFile" $
        it "writes floors that read back as the same floors" $
            decodeFloors (encodeUtf8 (renderFloorsFile sample)) `shouldBe` Right sample

    describe "what a run brings" $ do
        it "reads a scheduled run from GitHub's event name, and any other run as on demand" $ do
            triggerIn (Map.singleton "GITHUB_EVENT_NAME" "schedule") `shouldBe` Scheduled
            triggerIn (Map.singleton "GITHUB_EVENT_NAME" "workflow_dispatch") `shouldBe` OnDemand
            triggerIn Map.empty `shouldBe` OnDemand
        it "reads each of GitHub's runner variables, set or not" $ do
            runnerIn (Map.fromList [("GITHUB_ACTIONS", "true"), ("RUNNER_OS", "Linux"), ("RUNNER_ARCH", "ARM64")]) `shouldBe` onCalibratedRunner
            runnerIn (Map.fromList [("RUNNER_OS", "Linux")]) `shouldBe` RunnerReading Nothing (Just "Linux") Nothing
            runnerIn Map.empty `shouldBe` offGitHub
        it "names the request-pattern variables an environment sets, and no other" $
            patternOverridesIn (Map.fromList [("BENCH_PATTERN_ROUNDS", "8"), ("BENCH_LOAD_CONCURRENCY", "1"), ("BENCH_PATTERN_NAMES", "1")])
                `shouldBe` ["BENCH_PATTERN_NAMES", "BENCH_PATTERN_ROUNDS"]

    describe "enforce" $ do
        it "holds a run that matches the calibration to its pod shape's cells and confirmation re-runs" $
            enforce sample held `shouldBe` Enforced twoCores 1 floorsAtTwoCores
        it "does not hold a run at another injected latency, a millisecond away included" $
            for_ [19, 21, 5] $ \latency ->
                enforce sample held{rfSettings = calibrated{opUpstreamLatencyMs = latency}} `shouldBe` NotHeld (SettingsDiffer ("upstreamLatencyMs" :| []) :| [])
        it "does not hold a run under other measurement conditions, and fails it when scheduled" $ do
            enforce sample held{rfConditions = "unified-v2"} `shouldBe` NotHeld (ConditionsDiffer "unified-v3" "unified-v2" :| [])
            unheldViolations OnDemand (enforce sample held{rfConditions = "unified-v2"}) `shouldBe` []
            unheldViolations Scheduled (enforce sample held{rfConditions = "unified-v2"}) `shouldSatisfy` (not . null)
        it "does not hold a run that differs in any setting, and names the setting" $
            for_ offCalibration $ \(setting, ran) ->
                enforce sample held{rfSettings = ran} `shouldBe` NotHeld (SettingsDiffer (setting :| []) :| [])
        it "does not hold a run off the calibrated runner, and keeps what the run read" $
            for_ [offGitHub, onX64, RunnerReading Nothing (Just "Linux") (Just "ARM64")] $ \reading ->
                enforce sample held{rfRunner = reading} `shouldBe` NotHeld (OtherRunner "ubuntu-26.04-arm" calibratedRunner reading :| [])
        it "does not hold a run under a pod shape the floors do not hold, and names the shape once" $
            enforce sample held{rfShape = Unlimited} `shouldBe` NotHeld (ShapeNotCalibrated Unlimited :| [])
        it "names every reason a run is not held" $
            enforce sample RunFacts{rfSettings = calibrated{opDurationSeconds = 10, opScenarios = Just ["pypi/index-cold"]}, rfRunner = offGitHub, rfShape = Unlimited, rfConditions = "unified-v2"}
                `shouldBe` NotHeld (ShapeNotCalibrated Unlimited :| [SettingsDiffer ("durationSeconds" :| ["scenarios"]), OtherRunner "ubuntu-26.04-arm" calibratedRunner offGitHub, ConditionsDiffer "unified-v3" "unified-v2"])

    describe "floorCheck" $ do
        it "gives a held run each count's floor, for npm and PyPI alike, and none for a count the floors do not cover" $ do
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("npm/merge-cold", Loaded) `shouldBe` AtLeast 338 564
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("npm/merge-cold", ConcurrencyOne) `shouldBe` AtLeast 46 77
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("pypi/index-cold", Loaded) `shouldBe` AtLeast 964 1607
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("pypi/index-cold", ConcurrencyOne) `shouldBe` AtLeast 134 224
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("npm/herd", ConcurrencyOne) `shouldBe` NoFloor ConcurrencyOne
        it "gives an uncalibrated cell and a report-only cell no floor, and names the report-only reason" $ do
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("pypi/slow-readers", Loaded) `shouldBe` ReportOnly Nothing
            floorCheck (Enforced twoCores 1 floorsAtTwoCores) ("npm/herd", Loaded) `shouldBe` ReportOnly (Just "its burst successes split between admission levels")
            map (checkedFloor . floorCheck (Enforced twoCores 1 floorsAtTwoCores)) [("pypi/slow-readers", Loaded), ("npm/herd", Loaded), ("npm/merge-cold", Loaded)] `shouldBe` [Nothing, Nothing, Just 338]
        it "checks nothing for a run that is not held" $
            floorCheck otherLatency ("pypi/index-cold", Loaded) `shouldBe` Unchecked

    describe "staleFloorViolations" $ do
        it "passes floors that match the counts the run checks" $
            staleFloorViolations (Enforced twoCores 1 floorsAtTwoCores) (Map.keysSet floorsAtTwoCores) `shouldBe` []
        it "fails a held run on a floor for a count it does not check" $
            staleFloorViolations (Enforced twoCores 1 floorsAtTwoCores) (Set.delete ("pypi/index-cold", ConcurrencyOne) (Map.keysSet floorsAtTwoCores))
                `shouldBe` ["bench/load/floors.json: the concurrencyOne cell for pypi/index-cold under 2cpu-1gib names no count this run checks"]
        it "checks nothing for a run that is not held" $
            staleFloorViolations otherLatency Set.empty `shouldBe` []

    describe "unheldViolations" $ do
        it "fails a scheduled run that its settings, its runner, or its pod shape keep off the floors, in one line" $
            unheldViolations Scheduled (NotHeld (ShapeNotCalibrated Unlimited :| [SettingsDiffer ("durationSeconds" :| ["scenarios"]), OtherRunner "ubuntu-26.04-arm" calibratedRunner onX64]))
                `shouldBe` [ "a scheduled run must be held to the success floors, and this one is not: the floors hold no entry for the pod shape unlimited, and it differs from the calibrated operating point in durationSeconds, scenarios, and it does not run on the calibrated runner (ubuntu-26.04-arm: GitHub Actions on Linux ARM64), and the harness read GITHUB_ACTIONS=true, RUNNER_OS=Linux, RUNNER_ARCH=X64"
                           ]
        it "fails a scheduled run of the GC-thrash probe, which is never held" $
            unheldViolations Scheduled (NotHeld (ThrashProbe :| []))
                `shouldBe` ["a scheduled run must be held to the success floors, and this one is not: it runs the GC-thrash probe in place of the passes"]
        it "fails a scheduled run at another injected latency" $
            unheldViolations Scheduled otherLatency
                `shouldBe` ["a scheduled run must be held to the success floors, and this one is not: it differs from the calibrated operating point in upstreamLatencyMs"]
        it "passes a scheduled run that is held, and any run on demand" $ do
            unheldViolations Scheduled (Enforced twoCores 1 floorsAtTwoCores) `shouldBe` []
            unheldViolations OnDemand (NotHeld (SettingsDiffer ("concurrency" :| []) :| [])) `shouldBe` []
            unheldViolations OnDemand (NotHeld (ThrashProbe :| [])) `shouldBe` []

    describe "describeEnforcement" $ do
        it "names the file, the runner, the conditions, the runs' commits, and the cells under the shape for a held run" $
            describeEnforcement calibration (Enforced twoCores 1 floorsAtTwoCores)
                `shouldBe` "This run is held to the success floors in `bench/load/floors.json`, calibrated on ubuntu-26.04-arm under measurement conditions unified-v3, from 3 calibration runs at abc123. Under 2cpu-1gib, 4 cells hold a floor, 1 are uncalibrated, and 1 are report-only."
        it "says a held run before the first calibration has no calibration run" $
            describeEnforcement calibration{calRuns = []} (Enforced twoCores 1 (Map.singleton ("npm/merge-cold", Loaded) Uncalibrated))
                `shouldBe` "This run is held to the success floors in `bench/load/floors.json`, calibrated on ubuntu-26.04-arm under measurement conditions unified-v3, from no calibration run yet. Under 2cpu-1gib, 0 cells hold a floor, 1 are uncalibrated, and 0 are report-only."
        it "names the setting for a run at another injected latency" $
            describeEnforcement calibration otherLatency
                `shouldBe` "This run is not held to the success floors in `bench/load/floors.json`: it differs from the calibrated operating point in upstreamLatencyMs."
        it "names every reason a run is not held, with what the harness read of the runner" $
            describeEnforcement calibration (NotHeld (SettingsDiffer ("durationSeconds" :| ["scenarios"]) :| [OtherRunner "ubuntu-26.04-arm" calibratedRunner offGitHub]))
                `shouldBe` "This run is not held to the success floors in `bench/load/floors.json`: it differs from the calibrated operating point in durationSeconds, scenarios, and it does not run on the calibrated runner (ubuntu-26.04-arm: GitHub Actions on Linux ARM64), and the harness read GITHUB_ACTIONS unset, RUNNER_OS unset, RUNNER_ARCH unset."

commit, event, firstRun :: Text
commit = "abc123"
event = "workflow_dispatch"
firstRun = "https://example.test/runs/1"

calibrated :: OperatingPoint
calibrated = OperatingPoint 30 100 363520 20 3 64 Nothing Nothing Nothing Nothing [] EveryPass noTuning

calibratedRunner :: RunnerKind
calibratedRunner = RunnerKind "Linux" "ARM64"

-- What the harness reads on the calibrated runner, on another architecture, and off GitHub Actions.
onCalibratedRunner, onX64, offGitHub :: RunnerReading
onCalibratedRunner = RunnerReading (Just "true") (Just "Linux") (Just "ARM64")
onX64 = RunnerReading (Just "true") (Just "Linux") (Just "X64")
offGitHub = RunnerReading Nothing Nothing Nothing

calibration :: Calibration
calibration =
    Calibration
        { calConditions = "unified-v3"
        , calRunner = "ubuntu-26.04-arm"
        , calRunnerKind = calibratedRunner
        , calOperatingPoint = calibrated
        , calFloorFraction = 0.6
        , calConfirmationReruns = 1
        , calRuns = [CalibrationRun runNumber ("https://example.test/runs/" <> show runNumber) commit event | runNumber <- toList calibrationRunIds]
        }

sample :: Floors
sample = Floors "Example floors." calibration (Map.singleton twoCores floorsAtTwoCores)

-- A run that matches the sample's calibration under its one pod shape.
held :: RunFacts
held = RunFacts{rfSettings = calibrated, rfRunner = onCalibratedRunner, rfShape = twoCores, rfConditions = "unified-v3"}

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
    , ("passes", calibrated{opPasses = LoadedPassOnly})
    , ("proxyCpus", calibrated{opProxyTuning = noTuning{ptCpus = rightToMaybe (parseCpuList "2-3")}})
    , ("proxyCores", calibrated{opProxyTuning = noTuning{ptCores = Just 2}})
    , ("proxyMaxHeapBytes", calibrated{opProxyTuning = noTuning{ptMaxHeapBytes = Just 872415232}})
    , ("proxyAllocAreaMib", calibrated{opProxyTuning = noTuning{ptAllocAreaMib = Just 32}})
    ]

twoCoreKey :: Key
twoCoreKey = "2cpu-1gib"

-- The sample's cells as the file writes them.
twoCoreFloors :: [Pair]
twoCoreFloors = [twoCoreKey .= object scenarioPairs]

scenarioPairs :: [Pair]
scenarioPairs =
    [ "npm/merge-cold" .= object ["loaded" .= floorValue 338 564, "concurrencyOne" .= floorValue 46 77]
    , "npm/herd" .= object ["loaded" .= object ["floor" .= Null, "reportOnly" .= ("its burst successes split between admission levels" :: Text)]]
    , "pypi/index-cold" .= object ["loaded" .= floorValue 964 1607, "concurrencyOne" .= floorValue 134 224]
    , "pypi/slow-readers" .= object ["loaded" .= object ["floor" .= Null]]
    ]

-- The sample's cells with one pass of one scenario in place of its own.
withCell :: Key -> Key -> Value -> [Pair]
withCell scenario whichPass cell = [twoCoreKey .= object (scenario .= object [whichPass .= cell] : filter ((/= scenario) . fst) scenarioPairs)]

-- A floor with its median, from the three sample runs.
floorValue :: Int -> Int -> Value
floorValue least median = object ["floor" .= least, "median" .= median, "from" .= calibrationRunIds]

runValue :: Int -> Text -> Value
runValue runNumber url = object ["id" .= runNumber, "url" .= url, "commit" .= commit, "event" .= event]

runValues :: [Value]
runValues = [runValue runNumber ("https://example.test/runs/" <> show runNumber) | runNumber <- [1, 2, 3]]

-- The sample's calibration record, with each override in place of the field of its key.
calibrationValue :: [Pair] -> Value
calibrationValue overrides = overriding overrides calibrationPairs

calibrationPairs :: [Pair]
calibrationPairs =
    [ "measurementConditions" .= ("unified-v3" :: Text)
    , "runner" .= ("ubuntu-26.04-arm" :: Text)
    , "runnerOs" .= ("Linux" :: Text)
    , "runnerArch" .= ("ARM64" :: Text)
    , "operatingPoint" .= operatingPointValue []
    , "floorFraction" .= (0.6 :: Double)
    , "confirmationReruns" .= (1 :: Int)
    , "runs" .= runValues
    ]

-- The calibrated operating point, with each override in place of the setting of its key.
operatingPointValue :: [Pair] -> Value
operatingPointValue overrides = overriding overrides operatingPointPairs

operatingPointPairs :: [Pair]
operatingPointPairs =
    [ "durationSeconds" .= (30 :: Int)
    , "concurrency" .= (100 :: Int)
    , "payloadBytes" .= (363520 :: Int)
    , "upstreamLatencyMs" .= (20 :: Int)
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
document calibrationRecord shapes = encode (object ["_note" .= ("Example floors." :: Text), "calibration" .= calibrationRecord, "floors" .= object shapes])
