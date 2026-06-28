module Ecluse.AcceptanceSpec (spec) where

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
    PackageOutcome (Measured, Unavailable),
    Report (reportOutcomes),
    Sample (Sample),
    Verdict (Breached, Within),
    budgetFor,
    decodeCriteria,
    evaluate,
    loadCriteria,
    renderReport,
    reportBreached,
    singleVersionBudgetFor,
 )

spec :: Spec
spec = do
    describe "decodeCriteria" $ do
        it "decodes the full and single-version defaults and per-package overrides" $
            decodeCriteria
                "{\"defaultBudgetMs\":100,\"perPackageBudgetMs\":{\"a\":5},\"defaultSingleVersionBudgetMs\":30,\"perPackageSingleVersionBudgetMs\":{\"a\":2}}"
                `shouldBe` Right (Criteria 100 (Map.fromList [("a", 5)]) 30 (Map.fromList [("a", 2)]))
        it "defaults the per-package maps to empty when absent" $
            decodeCriteria "{\"defaultBudgetMs\":100,\"defaultSingleVersionBudgetMs\":30}"
                `shouldBe` Right (Criteria 100 mempty 30 mempty)
        it "rejects criteria missing the required full default budget" $
            decodeCriteria "{\"defaultSingleVersionBudgetMs\":30}" `shouldSatisfy` isLeft
        it "rejects criteria missing the required single-version default budget" $
            decodeCriteria "{\"defaultBudgetMs\":100}" `shouldSatisfy` isLeft

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
            reportOutcomes (evaluate crit [Right within, Right overFull])
                `shouldBe` [ Measured within (Assessment 100 Within) (Assessment 30 Within)
                           , Measured overFull (Assessment 100 (Breached 75)) (Assessment 30 Within)
                           ]
        it "breaches the single-version leg when only it is over budget" $
            reportOutcomes (evaluate crit [Right overSingle])
                `shouldBe` [Measured overSingle (Assessment 100 Within) (Assessment 30 (Breached 50))]
        it "holds a per-package sample to its override budgets" $
            reportOutcomes (evaluate crit [Right heavy])
                `shouldBe` [Measured heavy (Assessment 500 Within) (Assessment 60 Within)]
        it "carries an unavailable package through, never as a breach" $
            reportOutcomes (evaluate crit [Left ("webpack", "registry unreachable")])
                `shouldBe` [Unavailable "webpack" "registry unreachable"]

    describe "reportBreached" $ do
        it "is True when the full leg is over budget" $
            reportBreached (evaluate crit [Right overFull]) `shouldBe` True
        it "is True when only the single-version leg is over budget" $
            reportBreached (evaluate crit [Right overSingle]) `shouldBe` True
        it "is False when both legs are within budget" $
            reportBreached (evaluate crit [Right within]) `shouldBe` False
        it "is False for an unavailable package (flaky registry is not a regression)" $
            reportBreached (evaluate crit [Left ("x", "unreachable")]) `shouldBe` False

    describe "renderReport" $ do
        let rendered = renderReport (evaluate crit [Right overFull, Right overSingle, Left ("webpack", "unreachable")])
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

    -- Guards the committed criteria file: a malformed or empty-budget edit fails here,
    -- in the gating tier, before the live harness ever runs.
    describe "the committed criteria" $
        it "loads with positive default budgets and per-package overrides for the heaviest package" $ do
            crit0 <- loadCriteria
            critDefaultBudgetMs crit0 `shouldSatisfy` (> 0)
            critDefaultSingleVersionBudgetMs crit0 `shouldSatisfy` (> 0)
            Map.member "@types/node" (critPerPackageBudgetMs crit0) `shouldBe` True
            Map.member "@types/node" (critPerPackageSingleVersionBudgetMs crit0) `shouldBe` True

-- ── fixtures ─────────────────────────────────────────────────────────────────────

crit :: Criteria
crit =
    Criteria
        { critDefaultBudgetMs = 100
        , critPerPackageBudgetMs = Map.fromList [("@types/node", 500)]
        , critDefaultSingleVersionBudgetMs = 30
        , critPerPackageSingleVersionBudgetMs = Map.fromList [("@types/node", 60)]
        }

-- name, versions, upstream ms, full overhead ms, single-version overhead ms
within :: Sample
within = Sample "lodash" 113 50 20 5

overFull :: Sample
overFull = Sample "react" 135 30 175 8

overSingle :: Sample
overSingle = Sample "express" 480 40 60 80

heavy :: Sample
heavy = Sample "@types/node" 2339 400 480 40
