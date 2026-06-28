module Ecluse.AcceptanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Acceptance (
    Criteria (Criteria, critDefaultBudgetMs, critPerPackageBudgetMs),
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
 )

spec :: Spec
spec = do
    describe "decodeCriteria" $ do
        it "decodes the default budget and the per-package overrides" $
            decodeCriteria "{\"defaultBudgetMs\":100,\"perPackageBudgetMs\":{\"a\":5}}"
                `shouldBe` Right (Criteria 100 (Map.fromList [("a", 5)]))
        it "defaults perPackageBudgetMs to empty when absent" $
            decodeCriteria "{\"defaultBudgetMs\":100}"
                `shouldBe` Right (Criteria 100 mempty)
        it "rejects criteria missing the required default budget" $
            decodeCriteria "{}" `shouldSatisfy` isLeft

    describe "budgetFor" $ do
        it "uses a per-package override when present" $
            budgetFor crit "@types/node" `shouldBe` 500
        it "falls back to the default budget otherwise" $
            budgetFor crit "lodash" `shouldBe` 100

    describe "evaluate" $ do
        it "marks an under-budget sample Within and an over-budget one Breached by its margin" $
            reportOutcomes (evaluate crit [Right within, Right over])
                `shouldBe` [Measured within 100 Within, Measured over 100 (Breached 75)]
        it "holds a per-package sample to its override budget" $
            reportOutcomes (evaluate crit [Right heavy])
                `shouldBe` [Measured heavy 500 Within]
        it "carries an unavailable package through, never as a breach" $
            reportOutcomes (evaluate crit [Left ("webpack", "registry unreachable")])
                `shouldBe` [Unavailable "webpack" "registry unreachable"]

    describe "reportBreached" $ do
        it "is True when a measured sample is over budget" $
            reportBreached (evaluate crit [Right over]) `shouldBe` True
        it "is False for an under-budget sample" $
            reportBreached (evaluate crit [Right within]) `shouldBe` False
        it "is False for an unavailable package (flaky registry is not a regression)" $
            reportBreached (evaluate crit [Left ("x", "unreachable")]) `shouldBe` False

    describe "renderReport" $ do
        let rendered = renderReport (evaluate crit [Right over, Left ("webpack", "unreachable")])
        it "names the overall breach result" $
            ("Result: BREACH" `T.isInfixOf` rendered) `shouldBe` True
        it "names the breached package and its margin" $ do
            ("react" `T.isInfixOf` rendered) `shouldBe` True
            ("BREACH +75.0 ms" `T.isInfixOf` rendered) `shouldBe` True
        it "separates the upstream and Ecluse-overhead legs (room for a normalization column)" $ do
            ("Upstream (ms)" `T.isInfixOf` rendered) `shouldBe` True
            ("Écluse overhead (ms)" `T.isInfixOf` rendered) `shouldBe` True
        it "lists an unavailable package as unavailable, not breached" $
            ("unavailable: unreachable" `T.isInfixOf` rendered) `shouldBe` True

    -- Guards the committed criteria file: a malformed or empty-budget edit fails here,
    -- in the gating tier, before the live harness ever runs.
    describe "the committed criteria" $
        it "loads with a positive default budget and a per-package override for the heaviest package" $ do
            crit0 <- loadCriteria
            critDefaultBudgetMs crit0 `shouldSatisfy` (> 0)
            Map.member "@types/node" (critPerPackageBudgetMs crit0) `shouldBe` True

-- ── fixtures ─────────────────────────────────────────────────────────────────────

crit :: Criteria
crit = Criteria{critDefaultBudgetMs = 100, critPerPackageBudgetMs = Map.fromList [("@types/node", 500)]}

-- name, versions, upstream ms, overhead ms
within :: Sample
within = Sample "lodash" 113 50 20

over :: Sample
over = Sample "react" 135 30 175

heavy :: Sample
heavy = Sample "@types/node" 2339 400 480
