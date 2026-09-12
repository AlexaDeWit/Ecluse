-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Acceptance budgets, ecosystem isolation, rendering, and the driver's exit decision.
Live registry availability never substitutes for these deterministic checks.
-}
module Ecluse.AcceptanceSpec (spec) where

import Ecluse.Acceptance qualified as Acceptance

import Data.Aeson (eitherDecode)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Acceptance (
    Assessment (Assessment),
    Criteria (
        Criteria,
        critDefaultBudgetMs,
        critDefaultSingleVersionBudgetMs,
        critPerPackageBudgetMs,
        critPerPackageSingleVersionBudgetMs
    ),
    CriteriaCatalogue (catalogueCriteria),
    OperatingPoint (OperatingPoint),
    PackageOutcome (Measured, Unavailable),
    Report (reportOutcomes),
    Sample (Sample),
    Verdict (Breached, Within),
    budgetFor,
    decodeCriteria,
    headroom,
    loadCriteria,
    renderReport,
    reportBreached,
    reportExitCode,
    singleVersionBudgetFor,
    watchFraction,
 )

-- | Pin budget isolation and the process exit decision without live registry dependencies.
spec :: Spec
spec = do
    describe "Criteria JSON" $ do
        it "decodes the full and single-version defaults and per-package overrides" $
            eitherDecode
                "{\"defaultBudgetMs\":100,\"perPackageBudgetMs\":{\"a\":5},\"defaultSingleVersionBudgetMs\":30,\"perPackageSingleVersionBudgetMs\":{\"a\":2}}"
                `shouldBe` Right (Criteria 100 (Map.fromList [("a", 5)]) 30 (Map.fromList [("a", 2)]))
        it "defaults the per-package maps to empty when absent" $
            eitherDecode "{\"defaultBudgetMs\":100,\"defaultSingleVersionBudgetMs\":30}"
                `shouldBe` Right (Criteria 100 mempty 30 mempty)
        it "rejects criteria missing the required full default budget" $
            (eitherDecode "{\"defaultSingleVersionBudgetMs\":30}" :: Either String Criteria) `shouldSatisfy` isLeft
        it "rejects criteria missing the required single-version default budget" $
            (eitherDecode "{\"defaultBudgetMs\":100}" :: Either String Criteria) `shouldSatisfy` isLeft

    describe "budgetFor" $ do
        it "uses a per-package override when present" $
            budgetFor crit "@types/node" `shouldBe` 500
        it "falls back to the default budget otherwise" $
            budgetFor crit "lodash" `shouldBe` 100

    describe "singleVersionBudgetFor" $ do
        it "uses a per-package override when present" $
            singleVersionBudgetFor crit "@types/node" `shouldBe` 60
        it "falls back to the single-version default otherwise" $
            singleVersionBudgetFor crit "lodash" `shouldBe` 30

    describe "evaluate" $ do
        it "assesses both legs, marking each over-budget leg Breached by its margin" $
            reportOutcomes (Acceptance.evaluate Npm (Just crit) [Right within, Right overFull])
                `shouldBe` [ Measured within (Assessment 100 Within) (Assessment 30 Within)
                           , Measured overFull (Assessment 100 (Breached 75)) (Assessment 30 Within)
                           ]
        it "breaches the single-version leg when only it is over budget" $
            reportOutcomes (Acceptance.evaluate Npm (Just crit) [Right overSingle])
                `shouldBe` [Measured overSingle (Assessment 100 Within) (Assessment 30 (Breached 50))]
        it "holds a per-package sample to its override budgets" $
            reportOutcomes (Acceptance.evaluate Npm (Just crit) [Right heavy])
                `shouldBe` [Measured heavy (Assessment 500 Within) (Assessment 60 Within)]
        it "carries an unavailable package through, never as a breach" $
            reportOutcomes (Acceptance.evaluate Npm (Just crit) [Left ("webpack", "registry unreachable")])
                `shouldBe` [Unavailable "webpack" "registry unreachable"]

    describe "reportBreached" $ do
        it "is True when the full leg is over budget" $
            reportBreached (Acceptance.evaluate Npm (Just crit) [Right overFull]) `shouldBe` True
        it "is True when only the single-version leg is over budget" $
            reportBreached (Acceptance.evaluate Npm (Just crit) [Right overSingle]) `shouldBe` True
        it "is False when both legs are within budget" $
            reportBreached (Acceptance.evaluate Npm (Just crit) [Right within]) `shouldBe` False
        it "is False for an unavailable package (flaky registry is not a regression)" $
            reportBreached (Acceptance.evaluate Npm (Just crit) [Left ("x", "unreachable")]) `shouldBe` False

    describe "headroom" $ do
        it "is the budget-to-observed multiple" $
            headroom 100 20 `shouldBe` Just 5
        it "is undefined for a non-positive observed figure" $ do
            headroom 100 0 `shouldBe` Nothing
            headroom 100 (-1) `shouldBe` Nothing

    describe "watchFraction" $
        it "sits strictly between the healthy range and the bar" $
            watchFraction `shouldSatisfy` (\f -> f > 0 && f < 1)

    describe "renderReport" $ do
        let op = OperatingPoint 5 8
            rendered = renderOne op (Acceptance.evaluate Npm (Just crit) [Right overFull, Right overSingle, Left ("webpack", "unreachable")])
        it "names the overall breach result" $
            ("Result: BREACH" `T.isInfixOf` rendered) `shouldBe` True
        it "names the breached full leg and its margin" $
            ("BREACH full +75.0 ms" `T.isInfixOf` rendered) `shouldBe` True
        it "names the breached single-version leg and its margin" $
            ("BREACH 1-ver +50.0 ms" `T.isInfixOf` rendered) `shouldBe` True
        it "keeps the upstream, full, and single-version legs in separate columns" $ do
            ("Upstream (ms)" `T.isInfixOf` rendered) `shouldBe` True
            ("Full overhead (ms)" `T.isInfixOf` rendered) `shouldBe` True
            ("Single-version (ms)" `T.isInfixOf` rendered) `shouldBe` True
        it "lists an unavailable package as unavailable, not breached" $
            ("unavailable: unreachable" `T.isInfixOf` rendered) `shouldBe` True
        it "names the operating point: catalogue size, timed passes, and budgets source" $ do
            ("8 packages (bench/corpus/pins.json)" `T.isInfixOf` rendered) `shouldBe` True
            ("median of 5 timed passes per leg" `T.isInfixOf` rendered) `shouldBe` True
            ("acceptance/criteria.json" `T.isInfixOf` rendered) `shouldBe` True
        it "renders each measured leg's headroom multiple" $ do
            let clean = renderOne op (Acceptance.evaluate Npm (Just crit) [Right within])
            ("Headroom full/1-ver" `T.isInfixOf` clean) `shouldBe` True
            ("5.0x / 6.0x" `T.isInfixOf` clean) `shouldBe` True
        it "renders an undefined headroom as n/a" $
            ("n/a / n/a" `T.isInfixOf` renderOne op (Acceptance.evaluate Npm (Just crit) [Right zeroObserved]))
                `shouldBe` True
        it "marks a within-budget leg at or above the watch fraction, with the note" $ do
            let watchRendered = renderOne op (Acceptance.evaluate Npm (Just crit) [Right nearFull])
            ("watch: full at 85% of budget" `T.isInfixOf` watchRendered) `shouldBe` True
            ("watch marks a leg at or above 70% of its budget" `T.isInfixOf` watchRendered)
                `shouldBe` True
        it "keeps a row clear of the watch fraction a plain within, with no note" $ do
            let clean = renderOne op (Acceptance.evaluate Npm (Just crit) [Right within])
            ("| within |" `T.isInfixOf` clean) `shouldBe` True
            ("watch" `T.isInfixOf` clean) `shouldBe` False
        it "never downgrades a breached leg to a watch mark" $
            ("watch: full" `T.isInfixOf` renderOne op (Acceptance.evaluate Npm (Just crit) [Right overFull]))
                `shouldBe` False

    describe "ecosystem criteria" $ do
        it "decodes separately named ecosystem sections" $
            (Map.keys . catalogueCriteria <$> decodeCriteria "{\"ecosystems\":{\"npm\":null,\"pypi\":null}}") `shouldBe` Right [Npm, PyPI]
        it "requires a section for each supported ecosystem" $
            decodeCriteria "{\"ecosystems\":{\"npm\":null}}" `shouldSatisfy` isLeft
        it "rejects an unknown ecosystem" $
            decodeCriteria "{\"ecosystems\":{\"npm\":null,\"pypi\":null,\"cargo\":null}}" `shouldSatisfy` isLeft
        it "rejects malformed budgets" $
            decodeCriteria "{\"ecosystems\":{\"npm\":{\"defaultBudgetMs\":0,\"defaultSingleVersionBudgetMs\":2},\"pypi\":null}}" `shouldSatisfy` isLeft
        it "loads npm budgets and an explicit uncalibrated PyPI section" $ do
            sections <- catalogueCriteria <$> loadCriteria
            Map.lookup PyPI sections `shouldBe` Just Nothing
            case join (Map.lookup Npm sections) of
                Nothing -> expectationFailure "missing npm criteria"
                Just npm -> do
                    critDefaultBudgetMs npm `shouldBe` 100
                    critDefaultSingleVersionBudgetMs npm `shouldBe` 25
                    Map.lookup "@types/node" (critPerPackageBudgetMs npm) `shouldBe` Just 300
                    Map.lookup "@types/node" (critPerPackageSingleVersionBudgetMs npm) `shouldBe` Just 100

    describe "ecosystem exit decisions" $ do
        forM_ [Npm, PyPI] $ \eco -> do
            it ("fails for " <> show eco <> " full overhead") $ do
                let report = Acceptance.evaluate eco (Just crit) [Right overFull]
                reportBreached report `shouldBe` True
                reportExitCode [report] `shouldBe` ExitFailure 1
            it ("fails for " <> show eco <> " selective overhead") $ do
                let report = Acceptance.evaluate eco (Just crit) [Right overSingle]
                reportBreached report `shouldBe` True
                reportExitCode [report] `shouldBe` ExitFailure 1
            it ("passes " <> show eco <> " observations exactly at both budgets") $ do
                let report = Acceptance.evaluate eco (Just crit) [Right (Sample "exact" 1 10 100 30)]
                reportBreached report `shouldBe` False
                reportExitCode [report] `shouldBe` ExitSuccess
            it ("passes " <> show eco <> " unavailable registries without a breach") $ do
                let report = Acceptance.evaluate eco (Just crit) [Left ("same", "registry HTTP 503")]
                reportBreached report `shouldBe` False
                reportExitCode [report] `shouldBe` ExitSuccess
        it "keeps the same package name's budgets separate across ecosystems" $ do
            let npm = Acceptance.evaluate Npm (Just crit) [Right within]
                pypi = Acceptance.evaluate PyPI (Just (Criteria 10 mempty 2 mempty)) [Right within]
            reportBreached npm `shouldBe` False
            reportBreached pypi `shouldBe` True
            reportExitCode [npm, pypi] `shouldBe` ExitFailure 1
            let rendered = renderReport (OperatingPoint 5 2) [npm, pypi]
            rendered `shouldSatisfy` T.isInfixOf "### npm"
            rendered `shouldSatisfy` T.isInfixOf "### pypi"
            rendered `shouldSatisfy` T.isInfixOf "100.0 / 30.0"
            rendered `shouldSatisfy` T.isInfixOf "10.0 / 2.0"
        it "prints uncalibrated observations and fails without inventing a budget" $ do
            let report = Acceptance.evaluate PyPI Nothing [Right within]
            reportBreached report `shouldBe` False
            reportExitCode [report] `shouldBe` ExitFailure 1
            renderOne (OperatingPoint 5 1) report `shouldSatisfy` T.isInfixOf "UNCALIBRATED"
        it "fails an uncalibrated section even when its registry is unavailable" $
            reportExitCode [Acceptance.evaluate PyPI Nothing [Left ("requests", "unreachable")]] `shouldBe` ExitFailure 1

renderOne :: OperatingPoint -> Report -> Text
renderOne op report = renderReport op [report]

crit :: Criteria
crit =
    Criteria
        { critDefaultBudgetMs = 100
        , critPerPackageBudgetMs = Map.fromList [("@types/node", 500)]
        , critDefaultSingleVersionBudgetMs = 30
        , critPerPackageSingleVersionBudgetMs = Map.fromList [("@types/node", 60)]
        }

within :: Sample
within = Sample "lodash" 113 50 20 5

overFull :: Sample
overFull = Sample "react" 135 30 175 8

overSingle :: Sample
overSingle = Sample "express" 480 40 60 80

heavy :: Sample
heavy = Sample "@types/node" 2339 400 480 40

nearFull :: Sample
nearFull = Sample "vue" 300 25 85 10

zeroObserved :: Sample
zeroObserved = Sample "empty" 1 10 0 0
