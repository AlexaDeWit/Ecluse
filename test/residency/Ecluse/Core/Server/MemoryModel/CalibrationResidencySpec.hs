-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Calibration rounding, and production charges that match the listing maxima.
module Ecluse.Core.Server.MemoryModel.CalibrationResidencySpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Metadata.EntryFloor (entryFloorBytes, releaseFloorBytes)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Server.MemoryModel.Calibration
import Ecluse.Core.Server.MemoryModel.Probe (Residency (..), requireResidency)
import Ecluse.Test.Corpus.Merge (MergeShape (Disjoint, Identical, Overlapping, PublishOrder))
import Ecluse.Test.Server.Mount (requiredAdapter)

-- | Check exact rounding, each current charge and the production factors.
spec :: Spec
spec = do
    describe "calibration rounding" $ do
        it "keeps an exact tenth and rounds the next rational upward" $ do
            marginCharge (4 % 5) `shouldBe` 1000
            marginCharge (800001 % 1000000) `shouldBe` 1100
        it "adds 3% to a held figure and rounds the next rational upward to a permille" $ do
            heldLimit 1 `shouldBe` 1030
            heldLimit (1000001 % 1000000) `shouldBe` 1031
        it "adds 4% to a read peak and rounds the next rational upward to a permille" $ do
            readPeakLimit (1 % 2) `shouldBe` 520
            readPeakLimit (500001 % 1000000) `shouldBe` 521
        it "adds a quarter to the largest dense figure and rounds up to 64 bytes" $ do
            floorRule (Map.singleton "kind" (EntryFigures 512 0)) `shouldBe` 640
            floorRule (Map.singleton "kind" (EntryFigures 0 (512001 % 1000))) `shouldBe` 704
        it "covers the all-shape maximum when it exceeds the margin" $ do
            fullReadCharge npmCalibration{realisticReadMaximum = 4 % 5, allReadMaximum = 11 % 10} `shouldBe` 1100
            fullReadCharge npmCalibration{realisticReadMaximum = 4 % 5, allReadMaximum = 1100001 % 1000000} `shouldBe` 1200
    describe "current calibration" $ do
        it "pins npm charges" $ do
            fullReadCharge npmCalibration `shouldBe` 1100
            marginCharge (outputMaximum npmCalibration) `shouldBe` 2000
        it "pins PyPI charges" $ do
            fullReadCharge pypiCalibration `shouldBe` 3500
            marginCharge (outputMaximum pypiCalibration) `shouldBe` 1900
        it "pins Cargo charges" $ do
            fullReadCharge cargoCalibration `shouldBe` 1700
            marginCharge (outputMaximum cargoCalibration) `shouldBe` 2600
        it "pins combined ARM64 RubyGems charges from exact native ratios" $ do
            realisticReadMaximum rubyGemsCalibration `shouldBe` 810584 % 56335
            allReadMaximum rubyGemsCalibration `shouldBe` 810584 % 56335
            outputMaximum rubyGemsCalibration `shouldBe` 4185840 % 1157711
            fullReadCharge rubyGemsCalibration `shouldBe` 18000
            marginCharge (outputMaximum rubyGemsCalibration) `shouldBe` 4600
        it "pins the labelled ARM64 synthetic listing ratios and regression limits" $ do
            figures <- maybe (fail "missing native synthetic listing figures") pure rubyGemsSyntheticFigures
            syntheticReadPeak figures `shouldBe` 15217776 % 1157711
            syntheticSingleOutput figures `shouldBe` 3277080 % 1157711
            syntheticMergedOutputs figures `shouldBe` Map.fromList [(Identical, 4185840 % 1157711), (Overlapping, 6627408 % 2315417), (Disjoint, 2878856 % 1157713), (PublishOrder, 3327300 % 1157711)]
            readPeakLimit (syntheticReadPeak figures) `shouldBe` 13671
            heldLimit (syntheticSingleOutput figures) `shouldBe` 2916
            Map.map heldLimit (syntheticMergedOutputs figures) `shouldBe` Map.fromList [(Identical, 3725), (Overlapping, 2949), (Disjoint, 2562), (PublishOrder, 2961)]
            readPeakLimit (syntheticReadPeak figures) `shouldSatisfy` (< fullReadCharge rubyGemsCalibration)
            map heldLimit (syntheticSingleOutput figures : Map.elems (syntheticMergedOutputs figures)) `shouldSatisfy` all (< marginCharge (outputMaximum rubyGemsCalibration))
        for_ [(Npm, npmCalibration), (PyPI, pypiCalibration), (Cargo, cargoCalibration), (RubyGems, rubyGemsCalibration)] $ \(ecosystem, calibration) ->
            it (show ecosystem <> " matches its adapter") $ do
                adapter <- requiredAdapter ecosystem
                let factors = metadataChargeFactors (adapterMetadata adapter)
                toInteger (cfFullReadPermille factors) `shouldBe` fullReadCharge calibration
                toInteger (cfOutputPermille factors) `shouldBe` marginCharge (outputMaximum calibration)
        it "pins the kept charges" $ do
            keptCharge npmFigures `shouldBe` 900
            keptCharge pypiFigures `shouldBe` 3300
            keptCharge cargoFigures `shouldBe` 1500
            keptCharge rubyGemsFigures `shouldBe` 17300
        it "pins the entry floor" $ do
            entryFloor `shouldBe` 512
            toInteger entryFloorBytes `shouldBe` entryFloor
        it "derives the release floor from the version and file figures" $ do
            releaseFloor `shouldBe` 1152
            toInteger releaseFloorBytes `shouldBe` releaseFloor
        it "keeps each version and file figure's regression limit below the release floor" $
            for_ releaseFloorFigures $ \figures -> do
                readPeakLimit (entryReadPeak figures) `shouldSatisfy` (< 1000 * releaseFloor)
                heldLimit (entryHeld figures) `shouldSatisfy` (< 1000 * releaseFloor)
        it "keeps each other entry figure's regression limit below the entry floor" $
            for_ entryFloorFigures $ \figures -> do
                readPeakLimit (entryReadPeak figures) `shouldSatisfy` (< 1000 * entryFloor)
                heldLimit (entryHeld figures) `shouldSatisfy` (< 1000 * entryFloor)
        for_ [Npm, PyPI, Cargo, RubyGems] $ \ecosystem ->
            it (show ecosystem <> " keeps its adapter's kept charge") $ do
                record <- requireResidency ecosystem
                adapter <- requiredAdapter ecosystem
                toInteger (cfKeptPermille (metadataChargeFactors (adapterMetadata adapter))) `shouldBe` keptCharge (residencyFigures record)
