-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Calibration rounding, and production charges that match the listing maxima.
module Ecluse.Core.Server.MemoryModel.CalibrationResidencySpec (spec) where

import Data.Ratio ((%))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Server.MemoryModel.Calibration
import Ecluse.Core.Server.MemoryModel.Probe (Residency (..), requireResidency)

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
        it "pins conservative initial RubyGems charges from exact native ratios" $ do
            realisticReadMaximum rubyGemsCalibration `shouldBe` 810584 % 56335
            allReadMaximum rubyGemsCalibration `shouldBe` 810584 % 56335
            outputMaximum rubyGemsCalibration `shouldBe` 161036 % 56335
            fullReadCharge rubyGemsCalibration `shouldBe` 18000
            marginCharge (outputMaximum rubyGemsCalibration) `shouldBe` 3600
        for_ [(Npm, npmCalibration), (PyPI, pypiCalibration), (Cargo, cargoCalibration), (RubyGems, rubyGemsCalibration)] $ \(ecosystem, calibration) ->
            it (show ecosystem <> " matches its adapter") $ do
                adapter <- maybe (fail "missing registered adapter") pure (adapterFor ecosystem)
                let factors = metadataChargeFactors (adapterMetadata adapter)
                toInteger (cfFullReadPermille factors) `shouldBe` fullReadCharge calibration
                toInteger (cfOutputPermille factors) `shouldBe` marginCharge (outputMaximum calibration)
        it "pins the kept charges" $ do
            keptCharge npmFigures `shouldBe` 900
            keptCharge pypiFigures `shouldBe` 3300
            keptCharge cargoFigures `shouldBe` 1500
            keptCharge rubyGemsFigures `shouldBe` 17300
        for_ [Npm, PyPI, Cargo, RubyGems] $ \ecosystem ->
            it (show ecosystem <> " keeps its adapter's kept charge") $ do
                record <- requireResidency ecosystem
                adapter <- maybe (fail "missing registered adapter") pure (adapterFor ecosystem)
                toInteger (cfKeptPermille (metadataChargeFactors (adapterMetadata adapter))) `shouldBe` keptCharge (residencyFigures record)
