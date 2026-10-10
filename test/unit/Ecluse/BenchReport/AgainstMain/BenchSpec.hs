-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bench comparison sets each time beside the one on @main@ in a section for each
ecosystem, leaves allocation out, and never fails on a missing side.
-}
module Ecluse.BenchReport.AgainstMain.BenchSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Missing (NoSuccessfulRun), Origin (Origin), sharedBaselineNote)
import Ecluse.BenchReport.AgainstMain.Bench (againstMain)

spec :: Spec
spec = do
    describe "againstMain" $ do
        let rendered = T.lines (againstMain (Baseline origin (csv onMain)) (Right (csv here)))
        it "leads with how the whole run moved, which one runner measured" $
            rendered `shouldSatisfy` elem "**Whole run.** Benches compared: 5. Time rose in 2, fell in 1, and held in 2. The median change is 0.0%, and the changes run from -20.0% to +10.0%."
        it "gives each ecosystem its own section, in this run's order, then the benches outside one" $
            filter (T.isPrefixOf "### ") rendered `shouldBe` ["### npm", "### pypi", "### no ecosystem", "### Reading the comparison"]
        it "says how each ecosystem's benches moved" $ do
            rendered `shouldSatisfy` elem "Benches compared: 3. Time rose in 1, fell in 1, and held in 1. The median change is 0.0%, and the changes run from -20.0% to +10.0%."
            rendered `shouldSatisfy` elem "Benches compared: 1. Time rose in 1, fell in 0, and held in 0. The median change is +10.0%, and the changes run from +10.0% to +10.0%."
        it "summarises each top group under its ecosystem" $ do
            rendered `shouldSatisfy` elem "| wire | 2 | -10.0% | -20.0% | 0.0% |"
            rendered `shouldSatisfy` elem "| rules | 1 | +10.0% | +10.0% | +10.0% |"
            rendered `shouldSatisfy` elem "| cache maintenance | 1 | 0.0% | 0.0% | 0.0% |"
        it "lists every bench with both times, the change, and the precision each run reached" $ do
            rendered `shouldSatisfy` elem "| wire.lodash | decode | 2.00 ms | 1.60 ms | -20.0% | 10.0% | 5.0% |"
            rendered `shouldSatisfy` elem "| rules.evalRules | lodash | 1.00 us | 1.10 us | +10.0% | 10.0% | 10.0% |"
            rendered `shouldSatisfy` elem "| cache maintenance | cold fill | 500 ns | 500 ns | 0.0% | 10.0% | 10.0% |"
        it "shows no precision for a single-iteration bench" $
            rendered `shouldSatisfy` elem "| wire.react | decode | 100 ms | 100 ms | 0.0% | n/a | n/a |"
        it "names the benches only one side holds, and compares none of them" $ do
            rendered `shouldSatisfy` elem "Only in this run: `ecosystem: npm.new.fresh`."
            rendered `shouldSatisfy` elem "Only on `main`: `ecosystem: npm.gone.old`."
            filter (T.isInfixOf "| fresh |") rendered `shouldBe` []
        it "says that every run shares this one baseline" $
            rendered `shouldSatisfy` elem sharedBaselineNote
        it "compares time alone: a change in allocation leaves the section as it was" $
            againstMain (Baseline origin (csv onMain)) (Right (csv [(name, mean, stdev, allocated * 3) | (name, mean, stdev, allocated) <- here]))
                `shouldBe` T.unlines rendered
        it "shows no change for a bench whose time on main is zero" $
            T.lines (againstMain (Baseline origin (csv [("g.b", 0, 0, 1)])) (Right (csv [("g.b", 5000, 0, 1)])))
                `shouldSatisfy` elem "| g | b | 0 ps | 5.00 ns | n/a | n/a | n/a |"

    describe "a missing side" $ do
        it "prints no baseline, with the reason, and compares nothing" $ do
            let rendered = againstMain (NoBaseline NoSuccessfulRun) (Right (csv here))
            rendered `shouldSatisfy` T.isInfixOf "**No baseline.** The fetch found no successful run on `main`."
            rendered `shouldNotSatisfy` T.isInfixOf "###"
        it "prints no baseline for a CSV on main that does not parse" $
            againstMain (Baseline origin "not a csv") (Right (csv here))
                `shouldSatisfy` T.isInfixOf "could not be read. The CSV did not parse: unrecognised CSV header: not a csv."
        it "prints no baseline for a CSV on main without a row" $
            againstMain (Baseline origin (csv [])) (Right (csv here)) `shouldSatisfy` T.isInfixOf "could not be read. The CSV holds no benchmark row."
        it "says so when this run left no CSV" $
            againstMain (Baseline origin (csv onMain)) (Left "A file is missing: bench-results.csv.")
                `shouldSatisfy` T.isInfixOf "**Nothing to compare.** This run's report could not be read. A file is missing: bench-results.csv."

origin :: Origin
origin = Origin "abc123" "https://example.test/runs/7" "2026-01-02T03:04:05Z"

-- A bench under the tier prefix: its path, mean, twice its deviation, and its allocation.
type Bench = (Text, Integer, Integer, Integer)

onMain :: [Bench]
onMain =
    [ ("ecosystem: npm.wire.lodash.decode", 2_000_000_000, 200_000_000, 100)
    , ("ecosystem: npm.wire.react.decode", 100_000_000_000, 0, 100)
    , ("ecosystem: npm.rules.evalRules.lodash", 1_000_000, 100_000, 100)
    , ("ecosystem: npm.gone.old", 1_000, 100, 100)
    , ("ecosystem: pypi.wire.numpy.decode", 4_000_000_000, 400_000_000, 100)
    , ("cache maintenance.cold fill", 500_000, 50_000, 100)
    ]

here :: [Bench]
here =
    [ ("ecosystem: npm.wire.lodash.decode", 1_600_000_000, 80_000_000, 100)
    , ("ecosystem: npm.wire.react.decode", 100_000_000_000, 0, 100)
    , ("ecosystem: npm.rules.evalRules.lodash", 1_100_000, 110_000, 100)
    , ("ecosystem: npm.new.fresh", 1_000, 100, 100)
    , ("ecosystem: pypi.wire.numpy.decode", 4_400_000_000, 440_000_000, 100)
    , ("cache maintenance.cold fill", 500_000, 50_000, 100)
    ]

csv :: [Bench] -> Text
csv benches =
    T.unlines $
        "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory"
            : [ T.intercalate "," ["All.ecluse-core (work-per-request)." <> name, show mean, show stdev, show allocated, "0", "0"]
              | (name, mean, stdev, allocated) <- benches
              ]
