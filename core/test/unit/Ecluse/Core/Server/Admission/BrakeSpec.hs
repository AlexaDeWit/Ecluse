-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.BrakeSpec (spec) where

import Hedgehog (Gen, assert, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.Admission.Brake (
    BrakeBounds (..),
    BrakeLevel (..),
    BrakeMarks (..),
    BrakeState (..),
    CollectorReading (..),
    GcSample (..),
    brakeStep,
    defaultBrakeMarks,
    gcSharePermille,
    initialBrakeState,
    newSampleWindow,
    windowSample,
 )

spec :: Spec
spec = describe "Ecluse.Core.Server.Admission.Brake" $ do
    shareSpec
    windowSpec
    stepSpec

shareSpec :: Spec
shareSpec = describe "gcSharePermille" $ do
    it "reads the collector's share of the CPU between two readings" $
        gcSharePermille (reading 1_000 100 0) (reading 3_000 600 0) `shouldBe` Just 250

    it "reads nothing when no CPU passed" $
        gcSharePermille (reading 1_000 100 0) (reading 1_000 100 0) `shouldBe` Nothing

windowSpec :: Spec
windowSpec = describe "windowSample" $ do
    it "averages the collector's share over the window, dropping the oldest reading" $ do
        let readings = [reading (1_000 * n) (100 * n + if n >= 3 then 500 * (n - 2) else 0) 1 | n <- [1 .. 4]]
            (samples, _) = foldl' feed ([], newSampleWindow 2) readings
        -- A two-period window spans three readings: the last sample reads the second to the fourth.
        map gsGcSharePermille (reverse samples) `shouldBe` [Nothing, Just 100, Just 350, Just 600]

    it "reports live data only after a major collection since the previous reading" $ do
        let opening = reading 1_000 100 3
            (_, window) = windowSample (newSampleWindow 10) (Just opening) 0 Nothing
            (sameMajor, window') = windowSample window (Just opening{crCpuNs = 2_000, crLiveBytes = 70}) 0 Nothing
            (nextMajor, _) = windowSample window' (Just opening{crCpuNs = 3_000, crMajorCollections = 4, crLiveBytes = 90}) 0 Nothing
        gsLiveAfterMajor sameMajor `shouldBe` Nothing
        gsLiveAfterMajor nextMajor `shouldBe` Just 90

    it "leaves the collector's rules out without statistics, and keeps the meter and kernel readings" $ do
        let (sample, _) = windowSample (newSampleWindow 10) Nothing 42 (Just 500)
        sample `shouldBe` GcSample{gsGcSharePermille = Nothing, gsLiveAfterMajor = Nothing, gsChargedBytes = 42, gsKernelPermille = Just 500}
  where
    feed (samples, window) next =
        let (sample, window') = windowSample window (Just next) 0 Nothing
         in (sample : samples, window')

reading :: Int64 -> Int64 -> Word32 -> CollectorReading
reading cpu gcCpu majors = CollectorReading{crCpuNs = cpu, crGcCpuNs = gcCpu, crMajorCollections = majors, crLiveBytes = 0}

stepSpec :: Spec
stepSpec = describe "brakeStep" $ do
    it "halves the budget when the collector takes too much of the CPU" $ do
        let stepped = brakeStep defaultBrakeMarks bounds start (calm{gsGcSharePermille = Just 700})
        bsBudget stepped `shouldBe` 50 * unit
        bsLevel stepped `shouldBe` Braking

    it "halves the budget when live data nears the copying overflow point" $ do
        let stepped = brakeStep defaultBrakeMarks bounds start (calm{gsLiveAfterMajor = Just (900 * unit)})
        bsLevel stepped `shouldBe` Braking

    it "halves the budget when the cgroup nears its limit" $
        bsLevel (brakeStep defaultBrakeMarks bounds start (calm{gsKernelPermille = Just 950})) `shouldBe` Braking

    it "shrinks by the live data the charges do not explain" $ do
        -- 20 units explained, 10 charged, 50 live: 20 units nobody accounted for.
        let stepped = brakeStep defaultBrakeMarks bounds start (calm{gsLiveAfterMajor = Just (50 * unit), gsChargedBytes = 10 * unit})
        bsCorrection stepped `shouldBe` 20 * unit
        bsBudget stepped `shouldBe` 80 * unit

    it "grows by a step only after a calm stretch, and never past the cap" $ do
        let run = iterate (\s -> brakeStep defaultBrakeMarks bounds s calm) start
            afterStretch = run !!? bmCalmSamples defaultBrakeMarks
            afterMany = run !!? 1000
        (bsBudget <$> run !!? 1) `shouldBe` Just (100 * unit)
        (bsBudget <$> afterStretch) `shouldBe` Just (112 * unit + unit `div` 2)
        (bsBudget <$> afterMany) `shouldBe` Just (bbCapBytes bounds)

    it "holds the budget between the marks" $ do
        let stepped = brakeStep defaultBrakeMarks bounds start (calm{gsGcSharePermille = Just 400})
        bsBudget stepped `shouldBe` 100 * unit
        bsLevel stepped `shouldBe` Holding

    it "keeps the budget within the floor and the cap for any samples (property)" $ hedgehog $ do
        samples <- forAll (Gen.list (Range.linear 1 200) genSample)
        let states = scanl (brakeStep defaultBrakeMarks bounds) start samples
        assert (all (\s -> bsBudget s >= bbFloorBytes bounds && bsBudget s <= bbCapBytes bounds) states)
  where
    unit = 1024 * 1024
    bounds =
        BrakeBounds
            { bbBootBytes = 100 * unit
            , bbFloorBytes = 16 * unit
            , bbCapBytes = 150 * unit
            , bbExplainedBytes = 20 * unit
            , bbOverflowLiveBytes = Just (1000 * unit)
            , bbGrowFloorBytes = unit
            }
    start = initialBrakeState bounds
    calm = GcSample{gsGcSharePermille = Just 100, gsLiveAfterMajor = Nothing, gsChargedBytes = 0, gsKernelPermille = Nothing}

genSample :: Gen GcSample
genSample =
    GcSample
        <$> Gen.maybe (Gen.int (Range.linear 0 1000))
        <*> Gen.maybe (Gen.int (Range.linear 0 (2048 * 1024 * 1024)))
        <*> Gen.int (Range.linear 0 (512 * 1024 * 1024))
        <*> Gen.maybe (Gen.int (Range.linear 0 1000))
