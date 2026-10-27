-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Diagnostic native success cannot certify calibrated load completion.
module Ecluse.BenchLoad.EvidenceSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Evidence (VerifiedRelayMeasurement (..), VerifiedResponseCounts (..), loadEvidence, loadEvidenceWithVerified, verifiedRelayProblems, verifiedRelayRequiredRows)
import Ecluse.BenchLoad.Exposition (ArtifactWindow (..), summariseGauge)
import Ecluse.BenchLoad.Floors (Enforcement (Enforced, NotHeld), FloorKey, Pass (Loaded), Unheld (SettingsDiffer))
import Ecluse.BenchLoad.Latency (percentiles)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), rtsWindow)
import Ecluse.BenchLoad.Support (rtsSnapshot)
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown), RunEvidence (RunEvidence))
import Ecluse.BenchReport.Performance (PerformanceRequest (..), makePerformanceReceipt)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (ExecutionIdentity))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "loadEvidence" $ do
    it "requires verified observation rows independently of supplied measurements" $
        verifiedRelayRequiredRows [("rubygems/gem-verified-known-length", Loaded), ("rubygems/gem-onboarding", Loaded), ("rubygems/gem-verified-slow-readers", Loaded)]
            `shouldBe` ["rubygems/gem-verified-known-length/loaded/verified-relay-observations", "rubygems/gem-verified-slow-readers/loaded/verified-relay-observations"]
    it "retains successful raw counts before missing verified observations fail" $ do
        let which = ("rubygems/gem-verified-known-length", Loaded)
            actual = RunEvidence "known length" [("", 10)] 0 (Just CleanShutdown) False
        native <- expectRight (eitherDecode (encode (loadEvidence conditions (Enforced Unlimited (Map.singleton which 5)) [] [(which, Right actual)])))
        npOutcome native `shouldBe` NativeFailed
        npObservations native `shouldBe` [PerformanceObservation "rubygems/gem-verified-known-length/loaded" NativePassed, PerformanceObservation "rubygems/gem-verified-known-length/loaded/verified-relay-observations" NativeFailed]
    it "publishes actual verified values beside their successful workload count" $ do
        let which = vrmKey measuredRelay
            actual = RunEvidence "known length" [("", 10)] 0 (Just CleanShutdown) False
        native <- expectRight (eitherDecode (encode (loadEvidenceWithVerified [measuredRelay] conditions (Enforced Unlimited (Map.singleton which 5)) [] [(which, Right actual)])))
        npOutcome native `shouldBe` NativePassed
        npObservations native `shouldBe` [PerformanceObservation "rubygems/gem-verified-known-length/loaded" NativePassed, PerformanceObservation "rubygems/gem-verified-known-length/loaded/verified-relay-observations" NativePassed]
    it "rejects each missing actual resource observation and an unsettled warm-up" $ do
        let original = measuredRelay
        verifiedRelayProblems [original] [vrmKey original] `shouldBe` []
        for_ [original{vrmWarmupSettled = Nothing}, original{vrmCounters = Nothing}, original{vrmWireReservation = Nothing}, original{vrmAllServeMemoryCharges = Nothing}, original{vrmFirstBodyByteAllResponses = Nothing}, original{vrmAllocationWindow = Nothing}, original{vrmProcessRtsAtEnd = Nothing}, original{vrmRetainedHeapAfterGc = Nothing}] $ \missing ->
            verifiedRelayProblems [missing] [vrmKey missing] `shouldSatisfy` (not . null)
        for_ [ArtifactWindow Nothing (Just 1024) (Just 1) (Just 0) (Just 0), ArtifactWindow (Just 1024) Nothing (Just 1) (Just 0) (Just 0), ArtifactWindow (Just 1024) (Just 1024) Nothing (Just 0) (Just 0), ArtifactWindow (Just 1024) (Just 1024) (Just 1) Nothing (Just 0), ArtifactWindow (Just 1024) (Just 1024) (Just 1) (Just 0) Nothing] $ \counters ->
            verifiedRelayProblems [original{vrmCounters = Just counters}] [vrmKey original] `shouldSatisfy` (not . null)
    it "requires the real slow-reader companion rather than a renamed fast load" $ do
        let slow = measuredRelay{vrmKey = ("rubygems/gem-verified-slow-readers", Loaded)}
        verifiedRelayProblems [slow] [vrmKey slow] `shouldSatisfy` (not . null)
        verifiedRelayProblems [slow{vrmCompanionResponses = Just (VerifiedResponseCounts 1 1 0 0 0 0), vrmCounters = Just (ArtifactWindow (Just 11264) (Just 11264) (Just 11) (Just 0) (Just 0))}] [vrmKey slow] `shouldBe` []
    it "requires one real verification and complete body/hash work per successful response" $ do
        let cached = measuredRelay{vrmCounters = Just (ArtifactWindow (Just 1024) (Just 1024) (Just 1) (Just 0) (Just 0))}
        verifiedRelayProblems [cached] [vrmKey cached] `shouldSatisfy` (not . null)
    it "keeps observed zero pressure distinct from absent counters and refuses duplicate native rows" $ do
        verifiedRelayProblems [measuredRelay] [vrmKey measuredRelay] `shouldBe` []
        verifiedRelayProblems [measuredRelay, measuredRelay] [vrmKey measuredRelay] `shouldSatisfy` (not . null)
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

measuredRelay :: VerifiedRelayMeasurement
measuredRelay =
    VerifiedRelayMeasurement
        { vrmKey = ("rubygems/gem-verified-known-length", Loaded)
        , vrmExpectedBodyBytes = 1024
        , vrmElapsedSeconds = 1
        , vrmMeasuredResponses = VerifiedResponseCounts 10 10 0 0 0 0
        , vrmCompanionResponses = Nothing
        , vrmWarmupSettled = Just True
        , vrmCounters = Just (ArtifactWindow (Just 10240) (Just 10240) (Just 10) (Just 0) (Just 0))
        , vrmWireReservation = Just (summariseGauge [Just 1024, Just 0])
        , vrmAllServeMemoryCharges = Just (summariseGauge [Just 8192, Just 0])
        , vrmFirstBodyByteAllResponses = Just (percentiles [0.001, 0.002])
        , vrmSuccessfulDuration = percentiles [0.003, 0.004]
        , vrmAllocationWindow = Just (rtsWindow rtsSnapshot rtsSnapshot{rsAllocatedBytes = 2000})
        , vrmProcessRtsAtEnd = Just rtsSnapshot{rsMaxLiveBytes = 1024}
        , vrmRetainedHeapAfterGc = Just 512
        , vrmCgroupAtEnd = Nothing
        }
