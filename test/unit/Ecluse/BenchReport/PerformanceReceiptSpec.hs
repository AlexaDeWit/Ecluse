-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Receipt creation preserves diagnostic exclusions, native failures and stale invalidation.
module Ecluse.BenchReport.PerformanceReceiptSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.BenchReport.Performance (PerformanceRequest (..))
import Ecluse.BenchReport.PerformanceCommand (compileObligation)
import Ecluse.BenchReport.PerformanceReceipt
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (..))
import Ecluse.Test.Env (withEnvOverrides)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "measurement boundary conditions" $ do
        it "keeps empty selections and literal values without inventing settings" $
            conditionArguments ["selectedScenarios=", "rts=-T", "label=x=y"]
                `shouldBe` Right (Map.fromList [("selectedScenarios", ""), ("rts", "-T"), ("label", "x=y")])
        it "rejects duplicate, malformed and caller-supplied compiler identities" $
            forM_ [["rts=-T", "rts=-N1"], ["rts"], ["=value"], ["compiler=unknown"]] $ \arguments ->
                conditionArguments arguments `shouldSatisfy` isLeft
    describe "receipt qualification" $ do
        it "keeps selected scenarios and thrash probes diagnostic even without native rows" $ do
            receiptDiagnostic "load" (Map.singleton "selectedScenarios" "npm/merge-cold") Nothing `shouldBe` Just "selected scenarios"
            receiptDiagnostic "load" (Map.singleton "thrashLimitsMiB" "2048,1536") Nothing `shouldBe` Just "GC-thrash probe"
            receiptDiagnostic "load" (Map.singleton "selectedScenarios" " \t\n") (Just held) `shouldBe` Nothing
        it "keeps named partial profiles diagnostic despite a claimed held native result" $ do
            for_ ["read-paths", "memory-admission", "artifacts"] $ \profile ->
                for_ ["load", "load-foundation"] $ \kind ->
                    receiptDiagnostic kind (Map.singleton "loadProfile" profile) (Just held) `shouldBe` Just "selected load profile"
            receiptDiagnostic "load" (Map.singleton "loadProfile" "full") (Just held) `shouldBe` Nothing
        it "uses native floor reasons for every diagnostic exclusion"
            $ forM_
                [ (Map.empty, "load floor enforcement missing")
                , (Map.singleton "floorEnforcement" "held", "load floor enforcement reasons missing")
                , (Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "WrongRunner")], "contradictory load floor enforcement")
                , (Map.fromList [("floorEnforcement", "not-held"), ("floorEnforcementReasons", "ChangedDuration")], "load floors not held")
                ]
            $ \(conditions, reason) ->
                receiptDiagnostic "load" Map.empty (Just held{npConditions = conditions}) `shouldBe` Just reason
        it "keeps unavailable live upstreams diagnostic despite native process success" $
            receiptDiagnostic "live-allocation" Map.empty (Just held{npOutcome = NativeUnavailable}) `shouldBe` Just "live upstream unavailable"
    describe "receipt publication" $ do
        it "binds all subjects to their own obligations and actual artifact bytes" $ withFixture $ do
            writePerformanceReceipts "allocation" "receipts" "conditions.json"
            forM_ ["npm", "pypi", "cargo", "rubygems"] $ \ecosystem -> do
                let directory = "receipts/ecosystem-" <> ecosystem <> "/"
                request <- readJsonFile (directory <> "request.json")
                receipt <- readJsonFile (directory <> "receipt.json")
                identityPlanDigest (requestIdentity request) `shouldBe` digest (jsonLine (requestObligation request))
                Map.lookup "perf-allocation-observations.json" (prArtifacts receipt) `shouldBe` Just (digest (jsonLine (nativeFor request)))
                poSubject (requestObligation request) `shouldBe` "ecosystem-" <> toText ecosystem
        it "publishes all failed receipts before returning failure" $
            withFixture $
                withEnvOverrides [("PERF_NATIVE_OUTCOME", "failure")] $ do
                    writePerformanceReceipts "allocation" "receipts" "conditions.json" `shouldThrow` (== ExitFailure 1)
                    forM_ ["npm", "pypi", "cargo", "rubygems"] $ \ecosystem -> do
                        receipt <- readJsonFile ("receipts/ecosystem-" <> ecosystem <> "/receipt.json")
                        npOutcome (prNative receipt) `shouldBe` NativeFailed
        it "clears stale receipts before missing native evidence fails" $ withFixture $ do
            writePerformanceReceipts "allocation" "receipts" "conditions.json"
            removeFile "perf-allocation-observations.json"
            writePerformanceReceipts "allocation" "receipts" "conditions.json" `shouldThrow` (== ExitFailure 1)
            forM_ ["npm", "pypi", "cargo", "rubygems"] $ \ecosystem ->
                doesFileExist ("receipts/ecosystem-" <> ecosystem <> "/receipt.json") `shouldReturn` False
        it "clears stale publication before an invalid conditions file fails" $ withFixture $ do
            writePerformanceReceipts "allocation" "receipts" "conditions.json"
            BS.writeFile "conditions.json" "invalid"
            writePerformanceReceipts "allocation" "receipts" "conditions.json" `shouldThrow` (== ExitFailure 1)
            doesFileExist "receipts/ecosystem-npm/receipt.json" `shouldReturn` False
            doesFileExist "receipts/ecosystem-pypi/receipt.json" `shouldReturn` False
            doesFileExist "receipts/ecosystem-cargo/receipt.json" `shouldReturn` False
            doesFileExist "receipts/ecosystem-rubygems/receipt.json" `shouldReturn` False

held :: NativePerformance
held = NativePerformance NativePassed [] (Map.fromList [("floorEnforcement", "held"), ("floorEnforcementReasons", "")])

withFixture :: IO () -> IO ()
withFixture action = withSystemTempDirectory "performance-receipt" $ \root -> withCurrentDirectory root $
    withEnvOverrides [("ECLTEST_SOURCE", "source"), ("ECLTEST_ARCHITECTURE", "arm64"), ("ECLTEST_RUN_ID", "run"), ("ECLTEST_ATTEMPT", "2"), ("PERF_NATIVE_OUTCOME", "success")] $ do
        writeConditions "conditions.json" ["optimisation=-O1", "profiling=off"]
        conditions <- readJsonFile "conditions.json"
        obligations <- traverse (expectRight . (\subject -> compileObligation "allocation" subject (Map.union allocationConditions conditions))) ["ecosystem-npm", "ecosystem-pypi", "ecosystem-cargo", "ecosystem-rubygems"]
        writeJsonFile "perf-allocation-observations.json" (NativePerformance NativePassed [PerformanceObservation row NativePassed | obligation <- obligations, row <- poRows obligation] allocationConditions)
        BS.writeFile "perf-allocation-report.md" "native report\n"
        writeJsonFile "acceptance/criteria.json" (Map.empty :: Map Text Text)
        writeJsonFile "bench/corpus/pins.json" (Map.singleton "captures" (Map.fromList [(ecosystem, Map.singleton "sample" (Map.singleton "path" (ecosystem <> "/sample.json"))) | ecosystem <- ["npm", "pypi", "cargo", "rubygems"]]) :: Map Text (Map Text (Map Text (Map Text Text))))
        forM_ ["npm", "pypi", "cargo", "rubygems"] $ \ecosystem -> writeJsonFile ("bench/corpus/" <> ecosystem <> "/sample.json") (Map.empty :: Map Text Text)
        action

allocationConditions :: Map Text Text
allocationConditions = Map.fromList [("mode", "captures"), ("passes", "5")]

nativeFor :: PerformanceRequest -> NativePerformance
nativeFor request = NativePerformance NativePassed observations allocationConditions
  where
    obligations = mapMaybe (rightToMaybe . (\subject -> compileObligation "allocation" subject (requestRecordedConditions request))) ["ecosystem-npm", "ecosystem-pypi", "ecosystem-cargo", "ecosystem-rubygems"]
    observations = [PerformanceObservation row NativePassed | obligation <- obligations, row <- poRows obligation]
