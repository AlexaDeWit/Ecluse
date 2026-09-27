-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.Memory.BrakeSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.Admission.Memory.Brake

-- | The collector brake: GC share over a window, the modelled reclaim share, and hysteresis.
spec :: Spec
spec = do
    reclaimSpec
    stepSpec
    propertySpec

reclaimSpec :: Spec
reclaimSpec = describe "reclaimRatio" $ do
    it "frees half the old generation in the normal regime (F = 2)" $
        reclaimRatio geometry (10 * mib) `shouldBe` 0.5

    it "still frees half at the point where the heap cap starts to govern" $
        -- (M - nursery) / 4 = (1024 - 128) / 4 MiB
        reclaimRatio geometry (224 * mib) `shouldBe` 0.5

    it "frees nothing where the copying collector overflows" $
        -- (M - nursery) / 2
        reclaimRatio geometry (448 * mib) `shouldBe` 0

    it "falls between the cap point and overflow" $
        reclaimRatio geometry (336 * mib) `shouldBe` 0.25

stepSpec :: Spec
stepSpec = describe "stepCollector" $ do
    it "measures no GC share until the window holds enough process CPU" $ do
        let (reading, _) = steps [sample 0 0 0 0, sample 10_000_000 9_000_000 0 0]
        crGcShare reading `shouldBe` Nothing
        crBrake reading `shouldBe` BrakeReleased

    it "engages at the GC-share mark and releases only below the release mark" $ do
        let heavy = [sample (n * 100_000_000) (n * 60_000_000) 0 0 | n <- [0 .. 10]]
            (engaged, st) = steps heavy
        crBrake engaged `shouldBe` BrakeEngaged
        -- Forty per cent over the window: between the marks, so the brake holds.
        let middle = [sample (1_000_000_000 + n * 100_000_000) (600_000_000 + n * 40_000_000) 0 0 | n <- [1 .. 10]]
            (holding, st') = stepMany st middle
        crBrake holding `shouldBe` BrakeEngaged
        let calm = [sample (2_000_000_000 + n * 100_000_000) (1_000_000_000 + n * 10_000_000) 0 0 | n <- [1 .. 10]]
            (released, _) = stepMany st' calm
        crBrake released `shouldBe` BrakeReleased

    it "averages live bytes over the majors between two samples and carries them while none run" $ do
        let (reading, st) = steps [sample 0 0 0 0, sample 1 0 2 (300 * mib)]
        crLiveAtMajor reading `shouldBe` Just (150 * mib)
        ticksSinceMajor st `shouldBe` 0
        let (carried, st') = stepMany st [sample 2 0 2 (300 * mib)]
        crLiveAtMajor carried `shouldBe` Just (150 * mib)
        ticksSinceMajor st' `shouldBe` 1

    it "engages on a low modelled reclaim with the heap ceiling known" $ do
        let (reading, _) = stepCollector defaultBrakeThresholds (Just geometry) (sample 1 0 1 (420 * mib)) (snd (steps [sample 0 0 0 0]))
        ((< engageReclaim) <$> crReclaim reading) `shouldBe` Just True
        crBrake reading `shouldBe` BrakeEngaged

    it "has no reclaim signal without a heap ceiling" $ do
        let (reading, _) = steps [sample 0 0 0 0, sample 1 0 1 (900 * mib)]
        crReclaim reading `shouldBe` Nothing

propertySpec :: Spec
propertySpec = describe "properties" $ do
    it "holds its state for any GC share strictly between the marks" $
        hedgehog $ do
            current <- forAll (Gen.element [BrakeReleased, BrakeEngaged])
            share <- forAll (Gen.double (Range.linearFrac releaseGcShare engageGcShare))
            when (share > releaseGcShare && share < engageGcShare) $
                settleBrake defaultBrakeThresholds current (Just share) Nothing === current

    it "engages on any GC share at or above the engage mark, whatever the reclaim reads" $
        hedgehog $ do
            share <- forAll (Gen.double (Range.linearFrac engageGcShare 1))
            reclaim <- forAll (Gen.maybe (Gen.double (Range.linearFrac 0 1)))
            settleBrake defaultBrakeThresholds BrakeReleased (Just share) reclaim === BrakeEngaged

-- Step the collector from its initial state, returning the last reading.
steps :: [CollectorSample] -> (CollectorReading, CollectorState)
steps = stepMany initialCollectorState

stepMany :: CollectorState -> [CollectorSample] -> (CollectorReading, CollectorState)
stepMany st = foldl' (\(_, s) x -> stepCollector defaultBrakeThresholds Nothing x s) (CollectorReading Nothing Nothing Nothing (collectorBrake st), st)

sample :: Int -> Int -> Int -> Int -> CollectorSample
sample = CollectorSample

geometry :: HeapGeometry
geometry = HeapGeometry{hgMaxHeapBytes = 1024 * mib, hgNurseryBytes = 128 * mib, hgOldGenFactor = 2}

mib :: Int
mib = 1024 * 1024
