-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Acceptance budgets, ecosystem isolation, rendering, and the driver's exit decision.
Live registry availability never substitutes for these deterministic checks.
-}
module Ecluse.AcceptanceSpec (spec) where

import Ecluse.Acceptance qualified as Acceptance

import Data.Aeson (Value (Null), eitherDecode, encode, object, (.=))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Acceptance (
    Assessment (Assessment),
    Criteria (
        Criteria,
        critCalibrationArch,
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
    Verdict (Breached, Uncalibrated, Within),
    budgetFor,
    decodeCriteria,
    headroom,
    hostArch,
    loadCriteria,
    renderReport,
    reportBreached,
    reportExitCode,
    singleVersionBudgetFor,
    watchFraction,
 )

spec :: Spec
spec = do
    describe "Criteria JSON" $ do
        it "decodes the full and single-version defaults, per-package overrides, and the calibration architecture" $
            eitherDecode
                "{\"arch\":\"x86_64\",\"defaultBudgetMs\":100,\"perPackageBudgetMs\":{\"a\":5},\"defaultSingleVersionBudgetMs\":30,\"perPackageSingleVersionBudgetMs\":{\"a\":2}}"
                `shouldBe` Right (Criteria 100 (Map.fromList [("a", 5)]) 30 (Map.fromList [("a", 2)]) "x86_64")
        it "defaults the per-package maps to empty when absent" $
            eitherDecode "{\"arch\":\"aarch64\",\"defaultBudgetMs\":100,\"defaultSingleVersionBudgetMs\":30}"
                `shouldBe` Right (Criteria 100 mempty 30 mempty "aarch64")
        it "rejects criteria missing the required full default budget" $
            (eitherDecode "{\"arch\":\"x86_64\",\"defaultSingleVersionBudgetMs\":30}" :: Either String Criteria) `shouldSatisfy` isLeft
        it "rejects criteria missing the required single-version default budget" $
            (eitherDecode "{\"arch\":\"x86_64\",\"defaultBudgetMs\":100}" :: Either String Criteria) `shouldSatisfy` isLeft
        it "rejects criteria missing the calibration architecture" $
            (eitherDecode "{\"defaultBudgetMs\":100,\"defaultSingleVersionBudgetMs\":30}" :: Either String Criteria) `shouldSatisfy` isLeft
        it "rejects an empty calibration architecture" $
            (eitherDecode "{\"arch\":\"\",\"defaultBudgetMs\":100,\"defaultSingleVersionBudgetMs\":30}" :: Either String Criteria) `shouldSatisfy` isLeft

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
            reportOutcomes (Acceptance.evaluate Npm crit [Right within, Right overFull])
                `shouldBe` [ Measured within (Assessment 100 Within) (Assessment 30 Within)
                           , Measured overFull (Assessment 100 (Breached 75)) (Assessment 30 Within)
                           ]
        it "breaches the single-version leg when only it is over budget" $
            reportOutcomes (Acceptance.evaluate Npm crit [Right overSingle])
                `shouldBe` [Measured overSingle (Assessment 100 Within) (Assessment 30 (Breached 50))]
        it "holds a per-package sample to its override budgets" $
            reportOutcomes (Acceptance.evaluate Npm crit [Right heavy])
                `shouldBe` [Measured heavy (Assessment 500 Within) (Assessment 60 Within)]
        it "carries an unavailable package through, never as a breach" $
            reportOutcomes (Acceptance.evaluate Npm crit [Left ("webpack", "registry unreachable")])
                `shouldBe` [Unavailable "webpack" "registry unreachable"]

    describe "reportBreached and reportExitCode" $
        for_ verdictRows $ \(name, samples, breached, code) ->
            it name $ do
                let report = Acceptance.evaluate Npm crit samples
                reportBreached report `shouldBe` breached
                reportExitCode [report] `shouldBe` code

    describe "calibration architecture" $ do
        let elsewhere = crit{critCalibrationArch = "not-" <> hostArch}
            report = Acceptance.evaluate Npm elsewhere [Right within, Right overFull, Left ("webpack", "unreachable")]
        it "marks every measured leg uncalibrated on another architecture, keeping its budget" $
            reportOutcomes report
                `shouldBe` [ Measured within (Assessment 100 Uncalibrated) (Assessment 30 Uncalibrated)
                           , Measured overFull (Assessment 100 Uncalibrated) (Assessment 30 Uncalibrated)
                           , Unavailable "webpack" "unreachable"
                           ]
        it "never breaches, so an over-budget sample exits 0" $ do
            reportBreached report `shouldBe` False
            reportExitCode [report] `shouldBe` ExitSuccess
        it "names both architectures in the result and marks each row uncalibrated" $ do
            let rendered = renderOne (OperatingPoint 5 3) report
            ("Result: uncalibrated: the budgets were calibrated on not-" <> hostArch) `shouldSatisfy` (`T.isInfixOf` rendered)
            (", and this run is on " <> hostArch) `shouldSatisfy` (`T.isInfixOf` rendered)
            ("| uncalibrated |" `T.isInfixOf` rendered) `shouldBe` True
            ("BREACH" `T.isInfixOf` rendered) `shouldBe` False
            ("watch" `T.isInfixOf` rendered) `shouldBe` False
        it "assesses as usual on the calibration architecture" $
            reportBreached (Acceptance.evaluate Npm crit [Right overFull]) `shouldBe` True

    describe "headroom" $ do
        it "is the budget-to-observed multiple" $
            headroom 100 20 `shouldBe` Just 5
        it "is undefined for a non-positive observed figure" $ do
            headroom 100 0 `shouldBe` Nothing
            headroom 100 (-1) `shouldBe` Nothing

    describe "watchFraction" $
        it "marks a leg at seven tenths of its budget, the figure the report prints" $
            watchFraction `shouldBe` 0.7

    describe "renderReport" $ do
        let op = OperatingPoint 5 8
            rendered = renderOne op (Acceptance.evaluate Npm crit [Right overFull, Right overSingle, Left ("webpack", "unreachable")])
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
            let clean = renderOne op (Acceptance.evaluate Npm crit [Right within])
            ("Headroom full/1-ver" `T.isInfixOf` clean) `shouldBe` True
            ("5.0x / 6.0x" `T.isInfixOf` clean) `shouldBe` True
        it "renders an undefined headroom as n/a" $
            ("n/a / n/a" `T.isInfixOf` renderOne op (Acceptance.evaluate Npm crit [Right zeroObserved]))
                `shouldBe` True
        it "marks a within-budget leg at or above the watch fraction, with the note" $ do
            let watchRendered = renderOne op (Acceptance.evaluate Npm crit [Right nearFull])
            ("watch: full at 85% of budget" `T.isInfixOf` watchRendered) `shouldBe` True
            ("watch marks a leg at or above 70% of its budget" `T.isInfixOf` watchRendered)
                `shouldBe` True
        it "keeps a row clear of the watch fraction a plain within, with no note" $ do
            let clean = renderOne op (Acceptance.evaluate Npm crit [Right within])
            ("| within |" `T.isInfixOf` clean) `shouldBe` True
            ("watch" `T.isInfixOf` clean) `shouldBe` False
        it "never downgrades a breached leg to a watch mark" $
            ("watch: full" `T.isInfixOf` renderOne op (Acceptance.evaluate Npm crit [Right overFull]))
                `shouldBe` False

    describe "ecosystem criteria" $ do
        let budgets = object ["arch" .= ("x86_64" :: Text), "defaultBudgetMs" .= (100 :: Int), "defaultSingleVersionBudgetMs" .= (25 :: Int)]
            document :: [(Text, Value)] -> LByteString
            document entries = encode (object ["ecosystems" .= Map.fromList entries])
        it "decodes separately named ecosystem sections" $
            (Map.keys . catalogueCriteria <$> decodeCriteria (document [("npm", budgets), ("pypi", budgets)]))
                `shouldBe` Right [Npm, PyPI]
        it "requires a section for each supported ecosystem" $
            decodeCriteria (document [("npm", budgets)]) `shouldSatisfy` isLeft
        it "rejects an unknown ecosystem" $
            decodeCriteria (document [("npm", budgets), ("pypi", budgets), ("cargo", budgets)]) `shouldSatisfy` isLeft
        it "rejects a null budget section" $
            decodeCriteria (document [("npm", budgets), ("pypi", Null)]) `shouldSatisfy` isLeft
        it "rejects a missing selective budget in the PyPI section" $
            decodeCriteria (document [("npm", budgets), ("pypi", object ["arch" .= ("x86_64" :: Text), "defaultBudgetMs" .= (1 :: Int)])])
                `shouldSatisfy` isLeft
        it "rejects a non-positive PyPI budget" $
            decodeCriteria (document [("npm", budgets), ("pypi", object ["arch" .= ("x86_64" :: Text), "defaultBudgetMs" .= (0 :: Int), "defaultSingleVersionBudgetMs" .= (1 :: Int)])])
                `shouldSatisfy` isLeft
        it "loads positive budgets and the three calibrated PyPI package overrides" $ do
            sections <- catalogueCriteria <$> loadCriteria
            forM_ [Npm, PyPI] $ \eco ->
                case Map.lookup eco sections of
                    Nothing -> expectationFailure ("missing criteria for " <> show eco)
                    Just criteria -> do
                        critDefaultBudgetMs criteria `shouldSatisfy` (> 0)
                        critDefaultSingleVersionBudgetMs criteria `shouldSatisfy` (> 0)
            (Map.keys . critPerPackageBudgetMs <$> Map.lookup PyPI sections)
                `shouldBe` Just ["boto3", "numpy", "requests"]
            (Map.keys . critPerPackageSingleVersionBudgetMs <$> Map.lookup PyPI sections)
                `shouldBe` Just ["boto3", "numpy", "requests"]

    describe "ecosystem exit decisions" $
        it "keeps the same package name's budgets separate across ecosystems" $ do
            let npm = Acceptance.evaluate Npm crit [Right within]
                pypi = Acceptance.evaluate PyPI (Criteria 10 mempty 2 mempty hostArch) [Right within]
            reportBreached npm `shouldBe` False
            reportBreached pypi `shouldBe` True
            reportExitCode [npm, pypi] `shouldBe` ExitFailure 1
            let rendered = renderReport (OperatingPoint 5 2) [npm, pypi]
            rendered `shouldSatisfy` T.isInfixOf "### npm"
            rendered `shouldSatisfy` T.isInfixOf "### pypi"
            rendered `shouldSatisfy` T.isInfixOf "100.0 / 30.0"
            rendered `shouldSatisfy` T.isInfixOf "10.0 / 2.0"

{- | Each measured leg with the verdict it earns and the status the driver exits with. The
ecosystem is a label neither decision reads, so one ecosystem settles these.
-}
verdictRows :: [(String, [Either (Text, Text) Sample], Bool, ExitCode)]
verdictRows =
    [ ("is True, and exits 1, when the full leg is over budget", [Right overFull], True, ExitFailure 1)
    , ("is True, and exits 1, when only the single-version leg is over budget", [Right overSingle], True, ExitFailure 1)
    , ("is False, and exits 0, when both legs are within budget", [Right within], False, ExitSuccess)
    , ("is False, and exits 0, for observations exactly at both budgets", [Right (Sample "exact" 1 10 100 30)], False, ExitSuccess)
    , ("is False, and exits 0, for an unavailable package (a flaky registry is not a regression)", [Left ("x", "unreachable")], False, ExitSuccess)
    ]

renderOne :: OperatingPoint -> Report -> Text
renderOne op report = renderReport op [report]

crit :: Criteria
crit =
    Criteria
        { critDefaultBudgetMs = 100
        , critPerPackageBudgetMs = Map.fromList [("@types/node", 500)]
        , critDefaultSingleVersionBudgetMs = 30
        , critPerPackageSingleVersionBudgetMs = Map.fromList [("@types/node", 60)]
        , critCalibrationArch = hostArch
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
