-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Reports must prove the expected groups, cases, and execution identity.
module Ecluse.Test.Contract.ResultSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Phase (PhaseTimings (..))
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

spec :: Spec
spec = do
    describe "auditReports" $ do
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
    describe "auditPartitionReport" $ do
        it "audits one partition against the original complete plan" $
            auditPartitionReport executionIdentity fullPlan report `shouldBe` []
        it "rejects a repeated partition as primary completion evidence" $
            auditPartitionReport executionIdentity fullPlan report{reportRepeat = True} `shouldSatisfy` (not . null)
        it "rejects omitted cases within the submitted partition" $
            auditPartitionReport executionIdentity fullPlan report{reportCases = []} `shouldSatisfy` (not . null)
        it "rejects an unknown partition" $
            auditPartitionReport executionIdentity fullPlan report{reportShard = "unknown"} `shouldSatisfy` (not . null)
        forM_ identityChanges $ \changed ->
            it ("rejects partition identity mismatch " <> show changed) $
                auditPartitionReport executionIdentity fullPlan report{reportIdentity = changed} `shouldSatisfy` (not . null)
    describe "auditFullReport" $ do
        it "accepts the whole plan without per-partition reports" $
            auditFullReport executionIdentity fullPlan completeReport `shouldBe` []
        forM_ invalidFullReports $ \(label, altered) ->
            it label $ auditFullReport executionIdentity fullPlan altered `shouldSatisfy` (not . null)
        forM_ identityChanges $ \changed ->
            it ("rejects full-suite identity mismatch " <> show changed) $
                auditFullReport executionIdentity fullPlan completeReport{reportIdentity = changed} `shouldSatisfy` (not . null)
        forM_ [Failed, Pending, SetupFailed, Cancelled, TimedOut] $ \status ->
            it ("rejects full-suite " <> show status <> " evidence") $
                auditFullReport executionIdentity fullPlan completeReport{reportCases = map (\result -> result{crStatus = status}) (reportCases completeReport)} `shouldSatisfy` (not . null)
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

fullPlan :: Plan
fullPlan =
    Plan
        1
        ["primary", "secondary"]
        catalogue
            { catalogueGroups = [scenarioGroup, secondGroup]
            , catalogueObligations = [obligation{obligationCases = [caseId item, "example/read/second"]}]
            }

secondGroup :: Group
secondGroup =
    scenarioGroup
        { groupId = "example/second"
        , groupPartition = "secondary"
        , groupCases = [Case "example/read/second" [obligationId obligation]]
        }

completeReport :: Report
completeReport =
    report
        { reportShard = "all"
        , reportSelectedGroups = [scenarioGroup, secondGroup]
        , reportGroups = reportGroups report <> [GroupResult "example/second" Passed 0 [] (PhaseTimings 0 0 0)]
        , reportCases = reportCases report <> [CaseResult "example/read/second" "example/second" Passed 0 []]
        }

invalidFullReports :: [(String, Report)]
invalidFullReports =
    [ ("rejects a filtered full-suite selection", report{reportShard = "all"})
    , ("rejects missing full-suite cases", completeReport{reportCases = reportCases report})
    , ("rejects duplicate full-suite cases", completeReport{reportCases = reportCases completeReport <> reportCases report})
    , ("rejects unknown full-suite cases", completeReport{reportCases = reportCases completeReport <> [CaseResult "unknown" "example/read" Passed 0 []]})
    , ("rejects missing full-suite groups", completeReport{reportGroups = reportGroups report})
    , ("rejects unknown full-suite groups", completeReport{reportGroups = reportGroups completeReport <> [GroupResult "unknown" Passed 0 [] (PhaseTimings 0 0 0)]})
    , ("rejects duplicate full-suite groups", completeReport{reportGroups = reportGroups completeReport <> reportGroups report})
    , ("rejects full-suite manifest omissions", completeReport{reportSelectedGroups = [scenarioGroup]})
    , ("rejects repeated full-suite reports", completeReport{reportRepeat = True})
    , ("rejects a partition presented as the full suite", completeReport{reportShard = "primary"})
    ]
