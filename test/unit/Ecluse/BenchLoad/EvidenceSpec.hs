-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Diagnostic native success cannot certify calibrated load completion.
module Ecluse.BenchLoad.EvidenceSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Evidence (loadEvidence)
import Ecluse.BenchLoad.Floors (Enforcement (Enforced, NotHeld), FloorKey, Pass (Loaded), Unheld (SettingsDiffer))
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown), RunEvidence (RunEvidence))
import Ecluse.BenchReport.Performance (PerformanceRequest (..), makePerformanceReceipt)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (ExecutionIdentity))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "loadEvidence" $ do
    it "retains diagnostic success and actual NotHeld reasons but refuses completion" $ do
        native <- emitted (NotHeld (SettingsDiffer ("durationSeconds" :| []) :| []))
        npOutcome native `shouldBe` NativePassed
        Map.lookup "floorEnforcement" (npConditions native) `shouldBe` Just "not-held"
        Map.lookup "floorEnforcementReasons" (npConditions native) `shouldSatisfy` maybe False (T.isInfixOf "durationSeconds")
        audit native `shouldContain` ["load floors were not held: load-foundation/ecosystem-pypi"]
    it "accepts a held run with actual successful floor checks" $ do
        native <- emitted (Enforced Unlimited (Map.singleton key 5))
        audit native `shouldBe` []
    it "rejects old native output even when the caller asserts held floors" $ do
        native <- emitted (Enforced Unlimited (Map.singleton key 5))
        audit native{npConditions = Map.delete "floorEnforcement" (npConditions native)} `shouldContain` ["load floors were not held: load-foundation/ecosystem-pypi"]
    it "rejects contradictory held output with reasons" $ do
        native <- emitted (Enforced Unlimited (Map.singleton key 5))
        audit native{npConditions = Map.insert "floorEnforcementReasons" "SettingsDiffer" (npConditions native)} `shouldContain` ["load floors were not held: load-foundation/ecosystem-pypi"]
  where
    emitted enforcement = expectRight (eitherDecode (encode (loadEvidence conditions enforcement [] [(key, Right healthy)])))
    audit native = auditPerformanceReceipts runIdentity "1" [obligation] [makePerformanceReceipt request artifacts native]
    request = PerformanceRequest runIdentity "1" obligation [] NativePassed (Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")])
    obligation = PerformanceObligation "load-foundation" "ecosystem-pypi" ["pypi/index-cold/loaded"] conditions
    conditions = Map.singleton "durationSeconds" "30"
    runIdentity = ExecutionIdentity "source" "not-applicable" "arm64" "plan" "run"
    artifacts = Map.singleton "load-observations.json" (T.replicate 64 "a")
    healthy = RunEvidence "pypi/index-cold" [("", 10)] 0 (Just CleanShutdown) False

key :: FloorKey
key = ("pypi/index-cold", Loaded)
