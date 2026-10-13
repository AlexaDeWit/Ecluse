-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Calibration rules pin existing factors and limits without measuring new floors.
module Ecluse.Core.Server.MemoryModel.CalibrationResidencySpec (spec) where

import Data.Ratio ((%))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Server.MemoryModel.Calibration
import Ecluse.Core.Server.MemoryModel.Probe (PeakLimits (..), Residency (..), Shape (..), requireResidency)

-- | Check exact rounding, each current literal and the production factors.
spec :: Spec
spec = do
    describe "calibration rounding" $ do
        it "keeps an exact tenth and rounds the next rational upward" $ do
            marginCharge (4 % 5) `shouldBe` 1000
            marginCharge (800001 % 1000000) `shouldBe` 1100
        it "keeps an exact quarter and rounds the next rational upward" $ do
            regressionLimit (25 % 27) `shouldBe` 1000
            regressionLimit (25000001 % 27000000) `shouldBe` 1250
        it "covers the all-shape maximum when it exceeds the margin" $ do
            fullReadCharge npmCalibration{realisticReadMaximum = 4 % 5, allReadMaximum = 11 % 10} `shouldBe` 1100
            fullReadCharge npmCalibration{realisticReadMaximum = 4 % 5, allReadMaximum = 1100001 % 1000000} `shouldBe` 1200
    describe "current calibration" $ do
        it "pins npm charges and limits" $ do
            fullReadCharge npmCalibration `shouldBe` 1100
            marginCharge (outputMaximum npmCalibration) `shouldBe` 2000
            [regressionLimit (maximumOf npmCalibration) | maximumOf <- [singleReadMaximum, outputMaximum, wireMaximum, rawMaximum, typedMaximum, sharedMaximum]]
                `shouldBe` [1000, 1750, 1250, 7000, 500, 1000]
        it "pins PyPI charges and limits with its raw-tree exception" $ do
            fullReadCharge pypiCalibration `shouldBe` 3500
            marginCharge (outputMaximum pypiCalibration) `shouldBe` 1900
            [regressionLimit (maximumOf pypiCalibration) | maximumOf <- [singleReadMaximum, outputMaximum, wireMaximum, typedMaximum, sharedMaximum]]
                `shouldBe` [3250, 1750, 1250, 1750, 3500]
            pypiRawLimit `shouldBe` 7000
            regressionLimit (rawMaximum pypiCalibration) `shouldBe` 4500
        for_ [(Npm, npmCalibration, [1250, 7000, 500, 1000], 1000), (PyPI, pypiCalibration, [1250, 7000, 1750, 3500], 3250)] $ \(ecosystem, calibration, envelopes, readLimit) ->
            it (show ecosystem <> " matches its adapter and residency record") $ do
                record <- requireResidency ecosystem
                let factors = metadataChargeFactors (residencyMetadata record)
                toInteger (cfFullReadPermille factors) `shouldBe` fullReadCharge calibration
                toInteger (cfOutputPermille factors) `shouldBe` marginCharge (outputMaximum calibration)
                map (residencyEnvelope record) [Wire, Raw, Typed, Shared] `shouldBe` envelopes
                readPeakLimit (residencyPeakLimits record) `shouldBe` readLimit
                outputLimit (residencyPeakLimits record) `shouldBe` 1750
