-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native evidence cannot fill missing rows with declarations or stale results.
module Ecluse.Test.Contract.PerformanceSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Contract.Fixtures (executionIdentity)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (..))

spec :: Spec
spec = describe "auditPerformanceReceipts" $ do
    it "accepts actual rows bound to the required run and conditions" $
        audit [receipt] `shouldBe` []
    it "preserves receipt identities and native outcomes through JSON" $
        eitherDecode (encode receipt) `shouldBe` Right receipt
    it "rejects declarations with no observations" $
        audit [receipt{prNative = native{npObservations = []}}] `shouldContain` ["missing measurement: npm/react/full"]
    it "rejects an absent receipt" $
        audit [] `shouldContain` ["missing receipt: allocation/ecosystem-npm"]
    it "rejects duplicate native rows" $
        audit [receipt{prNative = native{npObservations = npObservations native <> npObservations native}}] `shouldContain` ["duplicate measurement: npm/react/full"]
    it "keeps extra native extensions and checks their outcomes" $ do
        audit [receipt{prNative = native{npObservations = npObservations native <> [PerformanceObservation "npm/native-extension" NativePassed]}}] `shouldBe` []
        audit [receipt{prNative = native{npObservations = npObservations native <> [PerformanceObservation "npm/native-extension" NativeFailed]}}] `shouldContain` ["measurement did not pass: npm/native-extension"]
    for_ [NativeFailed, NativeUnavailable] $ \outcome -> do
        it ("rejects a " <> show outcome <> " row") $
            audit [receipt{prNative = native{npObservations = [PerformanceObservation "npm/react/full" outcome]}}] `shouldSatisfy` (not . null)
        it ("rejects a " <> show outcome <> " process with successful rows") $
            audit [receipt{prNative = native{npOutcome = outcome}}] `shouldSatisfy` (not . null)
    it "rejects duplicate receipts" $
        audit [receipt, receipt] `shouldContain` ["duplicate receipt: allocation/ecosystem-npm"]
    it "rejects a previous attempt" $
        audit [receipt{prAttempt = "previous"}] `shouldContain` ["performance attempt mismatch: allocation/ecosystem-npm"]
    for_ changedIdentities $ \changed ->
        it ("rejects mismatched identity " <> show changed) $
            audit [receipt{prIdentity = changed}] `shouldContain` ["performance identity mismatch: allocation/ecosystem-npm"]
    it "rejects absent artifacts and malformed digests" $ do
        audit [receipt{prArtifacts = Map.empty}] `shouldContain` ["performance artifacts missing: allocation/ecosystem-npm"]
        audit [receipt{prArtifacts = Map.singleton "native.json" "not-a-digest"}] `shouldContain` ["invalid performance artifact digest: native.json"]
    it "rejects a changed measurement condition" $
        audit [receipt{prNative = native{npConditions = Map.singleton "passes" "1"}}] `shouldContain` ["measurement condition mismatch: passes"]
    it "rejects an empty or duplicated requirement" $ do
        auditPerformanceReceipts executionIdentity "1" [] [] `shouldContain` ["empty performance obligations"]
        auditPerformanceReceipts executionIdentity "1" [obligation{poRows = ["npm/react/full", "npm/react/full"]}] [receipt] `shouldContain` ["duplicate required measurement: npm/react/full"]
  where
    audit = auditPerformanceReceipts executionIdentity "1" [obligation]

obligation :: PerformanceObligation
obligation = PerformanceObligation "allocation" "ecosystem-npm" ["npm/react/full"] (Map.singleton "passes" "5")

native :: NativePerformance
native = NativePerformance NativePassed [PerformanceObservation "npm/react/full" NativePassed] (poConditions obligation)

receipt :: PerformanceReceipt
receipt = PerformanceReceipt 1 executionIdentity "1" "allocation" "ecosystem-npm" native (Map.singleton "native.json" (T.replicate 64 "a"))

changedIdentities :: [ExecutionIdentity]
changedIdentities =
    [ executionIdentity{identitySource = "other"}
    , executionIdentity{identityRunId = "other"}
    , executionIdentity{identityArchitecture = "other"}
    , executionIdentity{identityImage = "other"}
    , executionIdentity{identityPlanDigest = "other"}
    ]
