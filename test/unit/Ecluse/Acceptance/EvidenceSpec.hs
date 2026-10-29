-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Receipt observations preserve actual native verdicts and missing legs.
module Ecluse.Acceptance.EvidenceSpec (spec) where

import Data.Aeson (Result (Success), fromJSON)
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Acceptance
import Ecluse.Acceptance.Evidence
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Test.Contract.Performance

spec :: Spec
spec = do
    describe "capturesEvidence" $ do
        it "records only the measured leg, even when another leg is required" $
            fromJSON (capturesEvidence operatingPoint (report Within))
                `shouldBe` Success (NativePerformance NativePassed [PerformanceObservation "npm/react/full" NativePassed] conditions)
        for_ [Breached 1, NoBudget] $ \verdict ->
            it ("preserves " <> show verdict) $
                (npObservations <$> fromJSON (capturesEvidence operatingPoint (report verdict)))
                    `shouldBe` Success [PerformanceObservation "npm/react/full" NativeFailed]
    describe "liveEvidence" $ do
        it "keeps unavailable documents distinct from successful process exit" $
            fromJSON (liveEvidence operatingPoint [(Npm, [Unavailable "react" "HTTP 503"])])
                `shouldBe` Success (NativePerformance NativeUnavailable [PerformanceObservation "npm/react" NativeUnavailable] (Map.insert "mode" "live" conditions))
        it "keeps a refused document failed" $
            (npOutcome <$> fromJSON (liveEvidence operatingPoint [(Npm, [Failed "react" "refused"])])) `shouldBe` Success NativeFailed

operatingPoint :: OperatingPoint
operatingPoint = OperatingPoint 5 3 67108864

conditions :: Map Text Text
conditions = Map.fromList [("mode", "captures"), ("passes", "5"), ("capabilities", "3"), ("allocationAreaBytes", "67108864")]

report :: Verdict -> CapturesReport
report verdict = CapturesReport calibration [CapturesSection Npm [Assessed (AssessedLeg "react" 10 FullDocument (Measurement 100 99 101 1) (Just 100) verdict)]] []
  where
    calibration = Calibration "aarch64" "runner" "source" ("run" :| []) 10 mempty
