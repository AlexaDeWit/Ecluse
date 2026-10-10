-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The acceptance comparison reads the report the live run renders, sets each leg's time
beside the one on @main@ in a section for each ecosystem, and never fails on a missing side.
-}
module Ecluse.BenchReport.AgainstMain.AcceptanceSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Acceptance (
    Leg (FullDocument, SingleVersion),
    Measurement (Measurement),
    OperatingPoint (OperatingPoint),
    PackageOutcome (Failed, Measured, Unavailable),
    Sample (Sample),
    renderLiveReport,
 )
import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Origin (Origin))
import Ecluse.BenchReport.AgainstMain.Acceptance (againstMain)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

spec :: Spec
spec = do
    describe "againstMain, over the reports the live run renders" $ do
        let rendered = T.lines (againstMain (Baseline origin (report onMain)) (Right (report here)))
        it "leads with how the whole run moved, which one runner measured" $
            rendered `shouldSatisfy` elem "**Whole run.** Legs compared: 3. Time rose in 1, fell in 1, and held in 1. The median change is 0.0%, and the changes run from -10.0% to +25.0%."
        it "gives each ecosystem its own section" $
            filter (T.isPrefixOf "### ") rendered `shouldBe` ["### npm", "### pypi", "### Reading the comparison"]
        it "sets each leg's time beside the one on main" $ do
            rendered `shouldSatisfy` elem "| lodash | 117 on main, 118 here | full | 4.000 | 5.000 | +25.0% |"
            rendered `shouldSatisfy` elem "| lodash | 117 on main, 118 here | singleVersion | 1.000 | 1.000 | 0.0% |"
            rendered `shouldSatisfy` elem "| numpy | 137 | full | 70.000 | 63.000 | -10.0% |"
        it "says how each ecosystem's legs moved" $
            rendered `shouldSatisfy` elem "Legs compared: 1. Time rose in 0, fell in 1, and held in 0. The median change is -10.0%, and the changes run from -10.0% to -10.0%."
        it "names a leg that only one side measured, and compares no package that failed on both" $ do
            rendered `shouldSatisfy` elem "Only on `main`: `npm react full`."
            filter (T.isInfixOf "typescript") rendered `shouldBe` []
        it "compares time alone: a change in allocation leaves the section as it was" $
            againstMain (Baseline origin (report onMain)) (Right (report (heavier here))) `shouldBe` T.unlines rendered

    describe "a missing side" $ do
        it "prints no baseline, with the reason, and compares nothing" $ do
            let rendered = againstMain (NoBaseline "The runs of perf-acceptance.yml on main could not be listed.") (Right (report here))
            rendered `shouldSatisfy` T.isInfixOf "**No baseline.** The runs of perf-acceptance.yml on main could not be listed."
            rendered `shouldNotSatisfy` T.isInfixOf "###"
        it "prints no baseline for a report on main without a measured leg" $
            againstMain (Baseline origin (report [(Npm, [Unavailable "lodash" "registry HTTP 503"])])) (Right (report here))
                `shouldSatisfy` T.isInfixOf "could not be read. It holds no measured leg."
        it "says so when this run's report holds no table" $
            againstMain (Baseline origin (report onMain)) (Right "the harness crashed")
                `shouldSatisfy` T.isInfixOf "**Nothing to compare.** This run's report could not be read. It holds no measured leg."

origin :: Origin
origin = Origin "abc123" "https://example.test/runs/7" "2026-01-02T03:04:05Z"

report :: [(Ecosystem, [PackageOutcome])] -> Text
report = renderLiveReport (OperatingPoint 5 4 (64 * 1024 * 1024))

-- One package's full and single-version legs: its version count, and each leg's time in milliseconds.
package :: Text -> Int -> Double -> Double -> PackageOutcome
package name versions fullMs singleMs =
    Measured (Sample name versions (Just 12) [(FullDocument, Measurement 1050 1049 1051 fullMs), (SingleVersion, Measurement 200 200 200 singleMs)])

fullOnly :: Text -> Int -> Double -> PackageOutcome
fullOnly name versions fullMs = Measured (Sample name versions Nothing [(FullDocument, Measurement 9 9 9 fullMs)])

onMain :: [(Ecosystem, [PackageOutcome])]
onMain =
    [ (Npm, [package "lodash" 117 4 1, fullOnly "react" 2967 140, Failed "typescript" "FetchBoundExceeded"])
    , (PyPI, [fullOnly "numpy" 137 70])
    ]

here :: [(Ecosystem, [PackageOutcome])]
here =
    [ (Npm, [package "lodash" 118 5 1, Unavailable "react" "registry HTTP 503", Failed "typescript" "FetchBoundExceeded"])
    , (PyPI, [fullOnly "numpy" 137 63])
    ]

-- The same times over ten times the allocation.
heavier :: [(Ecosystem, [PackageOutcome])] -> [(Ecosystem, [PackageOutcome])]
heavier = map (second (map outcome))
  where
    outcome = \case
        Measured (Sample name versions upstream legs) ->
            Measured (Sample name versions upstream [(leg, Measurement (bytes * 10) (low * 10) (high * 10) ms) | (leg, Measurement bytes low high ms) <- legs])
        other -> other
