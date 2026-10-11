-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Primary shards must prove the exact expected groups, cases, and execution identity.
module Ecluse.Test.Contract.ResultSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Phase (PhaseTimings (..))
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

spec :: Spec
spec = describe "auditReports" $ do
    it "accepts complete primary evidence" $
        auditReports executionIdentity plan [report] `shouldBe` []
    forM_ invalidReports $ \(label, reports) ->
        it label $ auditReports executionIdentity plan reports `shouldSatisfy` (not . null)
    forM_ identityChanges $ \changed ->
        it ("rejects identity mismatch " <> show changed) $
            auditReports executionIdentity plan [report{reportIdentity = changed}] `shouldSatisfy` (not . null)
    forM_ [Failed, Pending, SetupFailed, Cancelled, TimedOut] $ \status ->
        it ("rejects " <> show status <> " cases") $
            auditReports executionIdentity plan [report{reportCases = [CaseResult "example/read/identity" "example/read" status 0 []]}] `shouldSatisfy` (not . null)
    it "requires group evidence even when every case passed" $
        auditReports executionIdentity plan [report{reportGroups = []}] `shouldSatisfy` (not . null)
    it "keeps the required case set when groups move to another partition" $ do
        let moved = plan{planPartitions = ["moved"], planCatalogue = catalogue{catalogueGroups = [scenarioGroup{groupPartition = "moved"}]}}
        auditReports executionIdentity moved [report{reportShard = "moved", reportSelectedGroups = [scenarioGroup{groupPartition = "moved"}]}] `shouldBe` []
    it "does not accept a modified partition manifest" $
        auditReports executionIdentity plan{planPartitions = []} [report] `shouldSatisfy` (not . null)
  where
    plan = Plan 1 ["primary"] catalogue

invalidReports :: [(String, [Report])]
invalidReports =
    [ ("rejects missing artifacts", [])
    , ("rejects duplicate primary shards", [report, report])
    , ("rejects unknown shards", [report{reportShard = "unknown"}])
    , ("rejects empty results", [report{reportCases = []}])
    , ("rejects duplicate results", [report{reportCases = reportCases report <> reportCases report}])
    , ("rejects unknown results", [report{reportCases = [CaseResult "unknown" "example/read" Passed 0 []]}])
    , ("rejects wrong group ownership", [report{reportCases = [CaseResult "example/read/identity" "wrong" Passed 0 []]}])
    , ("rejects failed lifecycle cleanup", [report{reportGroups = [GroupResult "example/read" Failed 0 ["cleanup"] (PhaseTimings 0 0 0)]}])
    , ("does not replace a missing primary report with a repeat", [report{reportRepeat = True}])
    ]

identityChanges :: [ExecutionIdentity]
identityChanges =
    [ executionIdentity{identitySource = "other"}
    , executionIdentity{identityImage = "other"}
    , executionIdentity{identityArchitecture = "other"}
    , executionIdentity{identityPlanDigest = "other"}
    , executionIdentity{identityRunId = "other"}
    ]
