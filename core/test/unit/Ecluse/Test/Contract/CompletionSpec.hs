-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Completion controls remove requirements and execution evidence independently.
module Ecluse.Test.Contract.CompletionSpec (spec) where

import Data.List (lookup)
import Test.Hspec

import Ecluse.Test.Contract.Completion
import Ecluse.Test.Contract.Fixtures qualified as Fixture
import Ecluse.Test.Contract.Phase (PhaseTimings (..))
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

spec :: Spec
spec = describe "full completion" $ do
    it "requires all eight ecosystem families and all six package-store families" $ do
        map fst (requiredFamilyLaws FullEcosystem) `shouldMatchList` ["proxying", "installation", "policy", "advisories", "mirroring", "publication", "dredger", "telemetry"]
        map fst (requiredFamilyLaws FullPackageStore) `shouldMatchList` ["reads", "writes", "inventory", "deletion", "integrity", "recovery"]
    it "distinguishes the explicit read-only foundation from full completion" $ do
        map fst (requiredFamilyLaws ReadOnlyEcosystemFoundation) `shouldMatchList` ["proxying", "installation", "policy", "advisories", "telemetry"]
        lookup "telemetry" (requiredFamilyLaws FullEcosystem) `shouldNotBe` lookup "telemetry" (requiredFamilyLaws ReadOnlyEcosystemFoundation)
    it "requires the npm outage law even when its registration and evidence are omitted" $ do
        let evidence = [LawEvidence family law E2E "npm" ("npm/" <> family <> "/" <> law) | (family, required) <- requiredFamilyLaws FullEcosystem, law <- required]
            omitted = Completion "npm" FullEcosystem evidence
        fromLeft [] (validateCompletion omitted []) `shouldContain` ["missing required law: dredger/skipped-admission-recovery"]
    it "keeps the outage law outside PyPI and the read-only foundation" $ do
        requiredFamilyLawsFor "pypi" FullEcosystem `shouldBe` requiredFamilyLaws FullEcosystem
        requiredFamilyLawsFor "npm" ReadOnlyEcosystemFoundation `shouldBe` requiredFamilyLaws ReadOnlyEcosystemFoundation
    it "requires native Ruby name and origin bindings without inventing self-reported fields" $ do
        let proxying = fromMaybe [] (lookup "proxying" (requiredFamilyLawsFor "rubygems" FullEcosystem))
        for_ ["metadata-fetch/held-origin-mismatch", "metadata-projection/name-binding", "metadata-projection/origin-binding"] $ \law ->
            proxying `shouldContain` [law]
        for_ ["metadata-fetch/held-name-mismatch", "metadata-projection/missing-name", "metadata-projection/name-mismatch"] $ \law ->
            proxying `shouldNotContain` [law]
        length proxying `shouldBe` length (fromMaybe [] (lookup "proxying" (requiredFamilyLaws FullEcosystem)))
    it "rejects an omitted Ruby binding even when its declaration and evidence disappear" $ do
        for_ ["metadata-fetch/held-origin-mismatch", "metadata-projection/name-binding", "metadata-projection/origin-binding"] $ \missing -> do
            let evidence = [LawEvidence family law E2E "rubygems" ("rubygems/" <> family <> "/" <> law) | (family, required) <- requiredFamilyLawsFor "rubygems" FullEcosystem, law <- required, law /= missing]
            fromLeft [] (validateCompletion (Completion "rubygems" FullEcosystem evidence) [])
                `shouldContain` ["missing required law: proxying/" <> missing]
    it "requires MAL refusal in both scopes and MAL revocation in full completion" $ do
        for_ [FullEcosystem, ReadOnlyEcosystemFoundation] $ \scope ->
            lookup "advisories" (requiredFamilyLaws scope) `shouldSatisfy` maybe False (elem "mal-denial")
        lookup "dredger" (requiredFamilyLaws FullEcosystem) `shouldSatisfy` maybe False (elem "mal-revocation")
    it "accepts independently declared laws with matching cases" $
        validateCompletion completion [catalogue] `shouldBe` Right completion
    it "rejects each omitted law even when its mapping and registration disappear together" $
        forM_ (completionEvidence completion) $ \evidence -> do
            let altered = completion{completionEvidence = filter (/= evidence) (completionEvidence completion)}
                remaining = filter ((/= evidenceCase evidence) . caseId) (groupCases scenarioGroup)
                changed = catalogue{catalogueGroups = [scenarioGroup{groupCases = remaining}], catalogueObligations = [obligation{obligationCases = map caseId remaining}]}
            validateCompletion altered [changed] `shouldSatisfy` isLeft
    it "rejects a whole omitted family without relying on a provider capability flag" $ do
        let altered = completion{completionEvidence = filter ((/= "recovery") . evidenceFamily) (completionEvidence completion)}
        validateCompletion altered [catalogue] `shouldSatisfy` isLeft
    it "rejects another tier or an unknown case as evidence" $
        forM_ [(\e -> e{evidenceTier = Load}, "completion group missing: backend: Load"), (\e -> e{evidenceCase = "missing"}, "completion case missing: missing")] $ \(change, fault) ->
            validateCompletion completion{completionEvidence = zipWith ($) (change : repeat id) (completionEvidence completion)} [catalogue] `shouldBe` Left [fault]
    it "rejects one passing case relabelled as several business laws" $ do
        let altered = completion{completionEvidence = map (\e -> e{evidenceCase = "backend/reads/present"}) (completionEvidence completion)}
        validateCompletion altered [catalogue] `shouldSatisfy` isLeft
    it "audits executed cases rather than trusting their declarations" $
        case makePlan catalogue of
            Left faults -> expectationFailure (show faults :: String)
            Right plan -> do
                auditCompletion completion [(Fixture.executionIdentity, plan, [report])] `shouldBe` []
                forM_ (completionEvidence completion) $ \evidence -> do
                    let omitted = report{reportCases = filter ((/= evidenceCase evidence) . crCaseId) (reportCases report)}
                        failed = report{reportCases = map (\actual -> if crCaseId actual == evidenceCase evidence then actual{crStatus = Failed} else actual) (reportCases report)}
                    auditCompletion completion [(Fixture.executionIdentity, plan, [omitted])] `shouldContain` ["completion case did not execute: " <> evidenceCase evidence]
                    auditCompletion completion [(Fixture.executionIdentity, plan, [failed])] `shouldContain` ["completion law did not pass: " <> evidenceFamily evidence <> "/" <> evidenceLaw evidence]
    it "refuses another partition's passing report as evidence for the subject's cases" $
        case makePlan sharedCatalogue of
            Left faults -> expectationFailure (show faults :: String)
            Right plan -> do
                auditCompletion completion [(Fixture.executionIdentity, plan, [report, otherReport])] `shouldBe` []
                auditCompletion completion [(Fixture.executionIdentity, plan, [otherReport])]
                    `shouldBe` ["completion case did not execute: " <> evidenceCase evidence | evidence <- completionEvidence completion]
    it "rejects absent execution reports" $
        auditCompletion completion [] `shouldContain` ["completion has no execution evidence"]

completion :: Completion
completion = Completion "backend" FullPackageStore [LawEvidence family law Unit "backend" ("backend/" <> family <> "/" <> law) | (family, law) <- laws]

laws :: [(Text, Text)]
laws =
    [ ("reads", "present")
    , ("reads", "absent")
    , ("writes", "complete")
    , ("writes", "idempotent")
    , ("inventory", "exact")
    , ("deletion", "selected")
    , ("integrity", "bytes-and-digests")
    , ("recovery", "publish-retry")
    , ("recovery", "reopen")
    , ("recovery", "delete-retry")
    ]

catalogue :: Catalogue
catalogue = Catalogue [] [Profile "backend" [] ["backend"] []] [obligation] [scenarioGroup]

obligation :: Obligation
obligation = Obligation "backend" "backend" "package-store" Supported [] ["backend/" <> family <> "/" <> law | (family, law) <- laws]

scenarioGroup :: Group
scenarioGroup = Group "backend" "package-store" "backend" Unit "backend" [Case identifier ["backend"] | identifier <- obligationCases obligation] [] [] [] []

report :: Report
report = Report 1 Fixture.executionIdentity "backend" False [GroupResult "backend" Passed 0 [] (PhaseTimings 0 0 0)] [CaseResult identifier "backend" Passed 0 [] | identifier <- obligationCases obligation] 1 [scenarioGroup]

-- | One plan shared by the subject's partition and an unrelated partition, as an E2E plan is.
sharedCatalogue :: Catalogue
sharedCatalogue = catalogue{catalogueObligations = [obligation, otherObligation], catalogueGroups = [scenarioGroup, otherGroup]}

otherObligation :: Obligation
otherObligation = obligation{obligationId = "other", obligationCases = ["other/case"]}

otherGroup :: Group
otherGroup = scenarioGroup{groupId = "other", groupPartition = "other", groupCases = [Case "other/case" ["other"]]}

-- | A fully passing report for the unrelated partition alone.
otherReport :: Report
otherReport = Report 1 Fixture.executionIdentity "other" False [GroupResult "other" Passed 0 [] (PhaseTimings 0 0 0)] [CaseResult "other/case" "other" Passed 0 []] 1 [otherGroup]
