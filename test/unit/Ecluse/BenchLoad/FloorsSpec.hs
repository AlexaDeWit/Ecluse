-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The floors file's reader, the operating point that holds a run to the floors, and what a held run fails closed on.
module Ecluse.BenchLoad.FloorsSpec (spec) where

import Data.Aeson (Value (Null), encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Calibration (..),
    Enforcement (Enforced, OffCalibration),
    FloorCheck (AtLeast, NoFloor, Unchecked),
    FloorKey,
    Floors (..),
    OperatingPoint (..),
    Pass (ConcurrencyOne, Loaded),
    decodeFloors,
    describeEnforcement,
    enforce,
    floorCheck,
    staleFloorViolations,
 )
import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited))

spec :: Spec
spec = do
    describe "decodeFloors" $ do
        it "reads the calibration, and each floor by pod shape, scenario, and pass" $
            decodeFloors (document (calibrationValue []) twoCoreFloors) `shouldBe` Right sample
        it "reads a null bound as the proxy's computed default, and a list as a scenario subset" $
            (calOperatingPoint . floorsCalibration <$> decodeFloors (document (calibrationValue ["operatingPoint" .= operatingPointValue ["serveMaxInFlight" .= (30 :: Int), "scenarios" .= ["npm/herd" :: Text]]]) twoCoreFloors))
                `shouldBe` Right calibrated{opServeMaxInFlight = Just 30, opScenarios = Just ["npm/herd"]}
        it "refuses an operating point that leaves a setting out" $
            decodeFloors (document (calibrationValue ["operatingPoint" .= object ["durationSeconds" .= (30 :: Int)]]) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a calibration without its rule, runner, commit, or runs" $
            for_ [["rule" .= ("" :: Text)], ["runner" .= ("" :: Text)], ["commit" .= ("" :: Text)], ["runs" .= ([] :: [Text])]] $ \override ->
                decodeFloors (document (calibrationValue override) twoCoreFloors) `shouldSatisfy` isLeft
        it "refuses a pod shape it cannot read, and one not in its rendered form" $
            for_ ["2cpu", "2CPU-1GIB", "2cpu-1024mib"] $ \shape ->
                decodeFloors (document (calibrationValue []) [shape .= object ["npm/merge-cold" .= object ["loaded" .= (1 :: Int)]]]) `shouldSatisfy` isLeft
        it "refuses an unknown pass" $
            decodeFloors (document (calibrationValue []) ["2cpu-1gib" .= object ["npm/merge-cold" .= object ["warm" .= (1 :: Int)]]]) `shouldSatisfy` isLeft
        it "refuses a floor below 1" $
            decodeFloors (document (calibrationValue []) ["2cpu-1gib" .= object ["npm/merge-cold" .= object ["loaded" .= (0 :: Int)]]]) `shouldSatisfy` isLeft

    describe "enforce" $ do
        it "holds a run at the calibrated operating point to its pod shape's floors" $
            enforce sample calibrated twoCores `shouldBe` Enforced floorsAtTwoCores
        it "holds a run under a pod shape without floors to none, so every count fails closed" $ do
            enforce sample calibrated Unlimited `shouldBe` Enforced Map.empty
            floorCheck (enforce sample calibrated Unlimited) ("npm/merge-cold", Loaded) `shouldBe` NoFloor
        it "does not hold a run that differs in any setting, and names the setting" $
            for_ offCalibration $ \(setting, ran) ->
                enforce sample ran twoCores `shouldBe` OffCalibration (setting :| [])
        it "names every setting that differs" $
            enforce sample calibrated{opDurationSeconds = 10, opScenarios = Just ["npm/herd"]} twoCores
                `shouldBe` OffCalibration ("durationSeconds" :| ["scenarios"])

    describe "floorCheck" $ do
        it "gives a held run each count's floor, and none for a count the floors do not cover" $ do
            floorCheck (Enforced floorsAtTwoCores) ("npm/merge-cold", Loaded) `shouldBe` AtLeast 338
            floorCheck (Enforced floorsAtTwoCores) ("npm/merge-cold", ConcurrencyOne) `shouldBe` AtLeast 46
            floorCheck (Enforced floorsAtTwoCores) ("npm/herd", ConcurrencyOne) `shouldBe` NoFloor
        it "checks nothing for a run off the calibrated operating point" $
            floorCheck (OffCalibration ("concurrency" :| [])) ("npm/merge-cold", Loaded) `shouldBe` Unchecked

    describe "staleFloorViolations" $ do
        it "passes floors that match the counts the run checks" $
            staleFloorViolations twoCores (Enforced floorsAtTwoCores) (Map.keysSet floorsAtTwoCores) `shouldBe` []
        it "fails a held run on a floor for a count it does not check" $
            staleFloorViolations twoCores (Enforced floorsAtTwoCores) (Set.fromList [("npm/merge-cold", Loaded), ("npm/herd", Loaded)])
                `shouldBe` ["bench/load/floors.json: the concurrencyOne floor for npm/merge-cold under 2cpu-1gib names no count this run checks"]
        it "checks nothing for a run off the calibrated operating point" $
            staleFloorViolations twoCores (OffCalibration ("scenarios" :| [])) Set.empty `shouldBe` []

    describe "describeEnforcement" $ do
        it "names the file, the runner, the commit, the run count, and the rule for a held run" $
            describeEnforcement calibration (Enforced floorsAtTwoCores)
                `shouldBe` "This run is held to the success floors in `bench/load/floors.json`, calibrated on ubuntu-26.04-arm at abc123 from 1 runs. Half the lowest night."
        it "names the settings that put a run off the calibrated operating point" $
            describeEnforcement calibration (OffCalibration ("durationSeconds" :| ["scenarios"]))
                `shouldBe` "This run is not held to the success floors in `bench/load/floors.json`: it differs from the operating point they were calibrated at in durationSeconds, scenarios."

rule, runner, commit, runUrl :: Text
rule = "Half the lowest night."
runner = "ubuntu-26.04-arm"
commit = "abc123"
runUrl = "https://example.test/runs/1"

calibrated :: OperatingPoint
calibrated = OperatingPoint 30 100 363520 5 3 64 Nothing Nothing Nothing Nothing

calibration :: Calibration
calibration = Calibration rule runner commit (runUrl :| []) calibrated

twoCores :: PodShape
twoCores = Limited 2 (1024 * 1024 * 1024)

floorsAtTwoCores :: Map FloorKey Int
floorsAtTwoCores = Map.fromList [(("npm/merge-cold", Loaded), 338), (("npm/merge-cold", ConcurrencyOne), 46), (("npm/herd", Loaded), 10)]

sample :: Floors
sample = Floors calibration (Map.singleton twoCores floorsAtTwoCores)

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
    , ("scenarios", calibrated{opScenarios = Just ["npm/merge-cold"]})
    ]

twoCoreFloors :: [Pair]
twoCoreFloors =
    [ "2cpu-1gib"
        .= object
            [ "npm/merge-cold" .= object ["loaded" .= (338 :: Int), "concurrencyOne" .= (46 :: Int)]
            , "npm/herd" .= object ["loaded" .= (10 :: Int)]
            ]
    ]

-- The sample's calibration record, with each override in place of the field of its key.
calibrationValue :: [Pair] -> Value
calibrationValue overrides =
    overriding overrides ["rule" .= rule, "runner" .= runner, "commit" .= commit, "runs" .= [runUrl], "operatingPoint" .= operatingPointValue []]

-- The calibrated operating point, with each override in place of the setting of its key.
operatingPointValue :: [Pair] -> Value
operatingPointValue overrides =
    overriding
        overrides
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
        ]

overriding :: [Pair] -> [Pair] -> Value
overriding overrides base = object (Map.toList (Map.union (Map.fromList overrides) (Map.fromList base)))

document :: Value -> [Pair] -> LByteString
document calibrationRecord shapes = encode (object ["calibration" .= calibrationRecord, "floors" .= object shapes])
