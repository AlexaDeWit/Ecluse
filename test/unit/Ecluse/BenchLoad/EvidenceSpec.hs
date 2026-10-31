-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Diagnostic native success cannot certify calibrated load completion, and each attempt's count reaches the observations.
module Ecluse.BenchLoad.EvidenceSpec (spec) where

import Data.Aeson (Value (Array, Object), eitherDecode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Evidence (loadEvidence)
import Ecluse.BenchLoad.Floors (Cell (Floored, ReportOnlyCell, Uncalibrated), Enforcement (Enforced, NotHeld), FloorKey, Pass (Loaded), Unheld (SettingsDiffer))
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown), RunEvidence (..))
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
        native <- emitted (Enforced Unlimited 1 (Map.singleton key (Floored 5 10 (1 :| [2, 3]))))
        audit native `shouldBe` []
    it "rejects old native output even when the caller asserts held floors" $ do
        native <- emitted (Enforced Unlimited 1 (Map.singleton key (Floored 5 10 (1 :| [2, 3]))))
        audit native{npConditions = Map.delete "floorEnforcement" (npConditions native)} `shouldContain` ["load floors were not held: load-foundation/ecosystem-pypi"]
    it "rejects contradictory held output with reasons" $ do
        native <- emitted (Enforced Unlimited 1 (Map.singleton key (Floored 5 10 (1 :| [2, 3]))))
        audit native{npConditions = Map.insert "floorEnforcementReasons" "SettingsDiffer" (npConditions native)} `shouldContain` ["load floors were not held: load-foundation/ecosystem-pypi"]
    it "records each attempt's count and whether it ran clean, beside the cell's floor" $ do
        let attempts = Right healthy{reSuccesses = [("", 4)]} :| [Right healthy{reSuccesses = [("measured", 7), ("concurrent load", 6)]}, Right healthy{reOomKills = 1}, Left "the scenario process exited 1."]
        observation (Enforced Unlimited 1 (Map.singleton key (Floored 5 10 (1 :| [2, 3])))) attempts
            `shouldBe` Just
                ( object
                    [ "id" .= ("pypi/index-cold/loaded" :: Text)
                    , "outcome" .= ("failed" :: Text)
                    , "attempts" .= [attempt (Just 4) True, attempt (Just 6) True, attempt (Just 10) False, attempt Nothing False]
                    , "floor" .= (5 :: Int)
                    ]
                )
    it "records an uncalibrated or report-only cell without a floor, with its reason" $ do
        floorFields (Enforced Unlimited 1 (Map.singleton key Uncalibrated)) `shouldBe` [("floor", "null"), ("reportOnly", "\"uncalibrated\"")]
        floorFields (Enforced Unlimited 1 (Map.singleton key (ReportOnlyCell "its counts vary"))) `shouldBe` [("floor", "null"), ("reportOnly", "\"its counts vary\"")]
        floorFields (NotHeld (SettingsDiffer ("durationSeconds" :| []) :| [])) `shouldBe` []
  where
    emitted enforcement = expectRight (eitherDecode (encode (loadEvidence conditions enforcement [] [(key, Right healthy :| [])])))
    observation :: Enforcement -> NonEmpty (Either Text RunEvidence) -> Maybe Value
    observation enforcement attempts = case loadEvidence conditions enforcement [] [(key, attempts)] of
        Object top -> case KeyMap.lookup "observations" top of
            Just (Array items) | [only] <- toList items -> Just only
            _ -> Nothing
        _ -> Nothing
    floorFields :: Enforcement -> [(Text, Text)]
    floorFields enforcement = case observation enforcement (Right healthy :| []) of
        Just (Object fields) -> [(Key.toText name, decodeUtf8 (encode value)) | (name, value) <- KeyMap.toList fields, name `elem` ["floor", "reportOnly"]]
        _ -> []
    attempt :: Maybe Int -> Bool -> Value
    attempt successes clean = object ["successes" .= successes, "clean" .= clean]
    audit native = auditPerformanceReceipts runIdentity "1" [obligation] [makePerformanceReceipt request artifacts native]
    request = PerformanceRequest runIdentity "1" obligation [] NativePassed (Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")])
    obligation = PerformanceObligation "load-foundation" "ecosystem-pypi" ["pypi/index-cold/loaded"] conditions
    conditions = Map.singleton "durationSeconds" "30"
    runIdentity = ExecutionIdentity "source" "not-applicable" "arm64" "plan" "run"
    artifacts = Map.singleton "load-observations.json" (T.replicate 64 "a")
    healthy = RunEvidence "pypi/index-cold" [("", 10)] 0 (Just CleanShutdown) False

key :: FloorKey
key = ("pypi/index-cold", Loaded)
