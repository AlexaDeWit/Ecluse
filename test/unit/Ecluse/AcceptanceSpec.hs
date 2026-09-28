-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Allocation criteria, the captures run's verdicts and exit decision, and the live run's.
module Ecluse.AcceptanceSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec

import Ecluse.Acceptance (
    AssessedLeg (..),
    Calibration (..),
    CapturesReport (..),
    CapturesSection (..),
    Criteria (..),
    Leg (FullDocument, SingleVersion),
    Measurement (Measurement),
    OperatingPoint (OperatingPoint),
    PackageOutcome (Failed, Measured, Unavailable),
    Row (Assessed, FailedPackage),
    Sample (Sample),
    Verdict (Breached, NoBudget, Uncalibrated, Within),
    assessCaptures,
    budgetBytes,
    capturesExitCode,
    capturesProblems,
    decodeCriteria,
    hostArch,
    liveExitCode,
    loadCriteria,
    renderCapturesReport,
    renderLiveReport,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

spec :: Spec
spec = do
    describe "decodeCriteria" $ do
        it "decodes the calibration and each package's figure per leg" $
            decodeCriteria (document (calibrationJson "abc123" ["https://example.test/runs/1"] 10) [("npm", lodashFigures)])
                `shouldBe` Right (Criteria calibration (Map.fromList [(Npm, Map.fromList [("lodash", Map.fromList [(FullDocument, 1000), (SingleVersion, 100)])])]))
        it "rejects an unknown leg" $
            decodeCriteria (document validCalibration [("npm", object ["lodash" .= object ["partial" .= (1 :: Int)]])]) `shouldSatisfy` isLeft
        it "rejects an unknown ecosystem" $
            decodeCriteria (document validCalibration [("cargo", object [])]) `shouldSatisfy` isLeft
        it "rejects a figure that is not positive" $
            decodeCriteria (document validCalibration [("npm", object ["lodash" .= object ["full" .= (0 :: Int)]])]) `shouldSatisfy` isLeft
        it "rejects a calibration without a commit" $
            decodeCriteria (document (calibrationJson "" ["https://example.test/runs/1"] 10) []) `shouldSatisfy` isLeft
        it "rejects a calibration without a run" $
            decodeCriteria (document (calibrationJson "abc123" [] 10) []) `shouldSatisfy` isLeft
        it "rejects a negative margin" $
            decodeCriteria (document (calibrationJson "abc123" ["https://example.test/runs/1"] (-1)) []) `shouldSatisfy` isLeft
        it "loads the committed criteria, with a section per ecosystem" $ do
            criteria <- loadCriteria
            Map.keys (critAllocatedBytes criteria) `shouldBe` [Npm, PyPI]

    describe "budgetBytes" $ do
        it "adds the margin and rounds up to a whole byte" $
            budgetBytes calibration 1001 `shouldBe` 1102
        it "is the calibrated figure itself with no margin" $
            budgetBytes calibration{calMarginPercent = 0} 1001 `shouldBe` 1001

    describe "assessCaptures" $ do
        it "holds each leg to its calibrated figure plus the margin" $
            verdicts (assessCaptures criteriaFixture [(Npm, [measured "lodash" 1100 200, measured "react" 900 50])])
                `shouldBe` [ ("lodash", FullDocument, Within)
                           , ("lodash", SingleVersion, Breached 90)
                           , ("react", FullDocument, Within)
                           , ("react", SingleVersion, NoBudget)
                           ]
        it "keeps each ecosystem's figures apart" $
            verdicts (assessCaptures criteriaFixture [(PyPI, [measured "lodash" 10 10])])
                `shouldBe` [("lodash", FullDocument, NoBudget), ("lodash", SingleVersion, NoBudget)]
        it "reports a failed and an unavailable package as failed rows" $
            concatMap sectionRows (reportSections (assessCaptures criteriaFixture [(Npm, [Failed "lodash" "refused", Unavailable "react" "HTTP 503"])]))
                `shouldBe` [FailedPackage "lodash" "refused", FailedPackage "react" "unavailable: HTTP 503"]
        it "names the calibrated packages the run did not measure" $
            reportUnmeasured (assessCaptures criteriaFixture [(Npm, [measured "lodash" 1 1])]) `shouldBe` [(Npm, "react")]
        it "counts a failed package as measured, not unmeasured" $
            reportUnmeasured (assessCaptures criteriaFixture [(Npm, [measured "lodash" 1 1, Failed "react" "refused"])]) `shouldBe` []
        it "marks every leg uncalibrated on another architecture" $
            verdicts (assessCaptures elsewhere [(Npm, [measured "lodash" 1 1])])
                `shouldBe` [("lodash", FullDocument, Uncalibrated), ("lodash", SingleVersion, Uncalibrated)]

    describe "capturesExitCode" $ do
        it "passes when every leg is within its budget and every calibrated package ran" $ do
            let report = assessCaptures criteriaFixture{critAllocatedBytes = lodashOnly} [(Npm, [measured "lodash" 1100 110])]
            capturesProblems report `shouldBe` []
            capturesExitCode report `shouldBe` ExitSuccess
        for_ failingRuns $ \(name, criteria, runs, problem) ->
            it name $ do
                let report = assessCaptures criteria runs
                capturesExitCode report `shouldBe` ExitFailure 1
                capturesProblems report `shouldSatisfy` any (problem `T.isInfixOf`)

    describe "renderCapturesReport" $ do
        let rendered = renderCapturesReport operatingPoint (assessCaptures criteriaFixture [(Npm, [measured "lodash" 1050 200, Failed "react" "refused"])])
        it "leads with the failure and its causes" $
            rendered `shouldSatisfy` T.isInfixOf "Result: FAIL: 1 leg(s) over budget, 1 package(s) failed"
        it "shows each leg's allocation, calibrated figure, change, and budget" $
            rendered `shouldSatisfy` T.isInfixOf "| lodash | 10 | full | 1050 | 1000 | +5.0% | 1100 | 5.000 | within |"
        it "names the bytes a breached leg is over by" $
            rendered `shouldSatisfy` T.isInfixOf "OVER by 90 bytes"
        it "lists a failed package with its reason" $
            rendered `shouldSatisfy` T.isInfixOf "FAILED: refused"
        it "names the calibration run and the margin" $ do
            rendered `shouldSatisfy` T.isInfixOf ("ubuntu-24.04-arm (" <> hostArch <> ") at abc123, plus 10%")
            rendered `shouldSatisfy` T.isInfixOf "https://example.test/runs/1"
        it "names the RTS options the run measured under" $
            rendered `shouldSatisfy` T.isInfixOf "RTS: -N4 -A64m"
        it "names a leg without a budget and the calibrated packages the run did not measure" $ do
            let unbudgeted = renderCapturesReport operatingPoint (assessCaptures criteriaFixture{critAllocatedBytes = lodashOnly} [(PyPI, [measured "numpy" 1 1])])
            unbudgeted `shouldSatisfy` T.isInfixOf "NO BUDGET"
            unbudgeted `shouldSatisfy` T.isInfixOf "Calibrated packages this run did not measure: npm lodash."
        it "passes a clean run" $
            renderCapturesReport operatingPoint (assessCaptures criteriaFixture{critAllocatedBytes = lodashOnly} [(Npm, [measured "lodash" 1 1])])
                `shouldSatisfy` T.isInfixOf "Result: PASS"

    describe "the live run" $ do
        let upstream = Measured (Sample "lodash" 10 (Just 12) [(FullDocument, Measurement 1050 5), (SingleVersion, Measurement 200 1)])
        it "fails when the proxy refused a document" $
            liveExitCode [(Npm, [upstream, Failed "typescript" "FetchBoundExceeded"])] `shouldBe` ExitFailure 1
        it "passes, incomplete, when a registry is unavailable" $ do
            let runs = [(Npm, [upstream, Unavailable "react" "registry HTTP 503"])]
            liveExitCode runs `shouldBe` ExitSuccess
            renderLiveReport operatingPoint runs `shouldSatisfy` T.isInfixOf "Result: incomplete: 1 package(s) unavailable"
            renderLiveReport operatingPoint runs `shouldSatisfy` T.isInfixOf "unavailable: registry HTTP 503"
        it "reports a refusal as a failure, never as unavailable" $ do
            let rendered = renderLiveReport operatingPoint [(Npm, [Unavailable "react" "HTTP 503", Failed "typescript" "FetchBoundExceeded"])]
            rendered `shouldSatisfy` T.isInfixOf "Result: FAILED: the proxy refused or could not process 1 package(s)"
            rendered `shouldSatisfy` T.isInfixOf "FAILED: FetchBoundExceeded"
        it "reports every leg with its upstream time and no budget" $ do
            let rendered = renderLiveReport operatingPoint [(Npm, [upstream])]
            rendered `shouldSatisfy` T.isInfixOf "Result: complete"
            rendered `shouldSatisfy` T.isInfixOf "| lodash | 10 | 12.000 | full | 1050 | 5.000 | measured |"
            rendered `shouldSatisfy` T.isInfixOf "| lodash | 10 | 12.000 | singleVersion | 200 | 1.000 | measured |"

-- Each run fails the captures check, with a problem naming why.
failingRuns :: [(String, Criteria, [(Ecosystem, [PackageOutcome])], Text)]
failingRuns =
    [ ("fails a leg over its budget", lodashCriteria, [(Npm, [measured "lodash" 1101 1])], "1 leg(s) over budget")
    , ("fails a leg without a budget", lodashCriteria, [(Npm, [measured "lodash" 1 1]), (PyPI, [measured "numpy" 1 1])], "2 leg(s) without a budget")
    , ("fails a package whose legs did not run", lodashCriteria, [(Npm, [Failed "lodash" "refused"])], "1 package(s) failed")
    , ("fails an unavailable package, which a captures run never expects", lodashCriteria, [(Npm, [Unavailable "lodash" "HTTP 503"])], "1 package(s) failed")
    , ("fails a calibrated package the run did not measure", criteriaFixture, [(Npm, [measured "lodash" 1 1])], "1 calibrated package(s) not measured")
    , ("fails a run that measured nothing", lodashCriteria{critAllocatedBytes = mempty}, [], "no package was measured")
    , ("fails on another architecture", elsewhere{critAllocatedBytes = lodashOnly}, [(Npm, [measured "lodash" 1 1])], "so no leg is assessed")
    ]
  where
    lodashCriteria = criteriaFixture{critAllocatedBytes = lodashOnly}

verdicts :: CapturesReport -> [(Text, Leg, Verdict)]
verdicts report = [(alPackage leg, alLeg leg, alVerdict leg) | section <- reportSections report, Assessed leg <- sectionRows section]

measured :: Text -> Int64 -> Int64 -> PackageOutcome
measured name full single = Measured (Sample name 10 Nothing [(FullDocument, Measurement full 5), (SingleVersion, Measurement single 1)])

calibration :: Calibration
calibration = Calibration hostArch "ubuntu-24.04-arm" "abc123" ("https://example.test/runs/1" :| []) 10

criteriaFixture :: Criteria
criteriaFixture =
    Criteria
        calibration
        (Map.fromList [(Npm, Map.fromList [("lodash", Map.fromList [(FullDocument, 1000), (SingleVersion, 100)]), ("react", Map.fromList [(FullDocument, 1000)])])])

lodashOnly :: Map Ecosystem (Map Text (Map Leg Int64))
lodashOnly = Map.fromList [(Npm, Map.fromList [("lodash", Map.fromList [(FullDocument, 1000), (SingleVersion, 100)])])]

elsewhere :: Criteria
elsewhere = criteriaFixture{critCalibration = calibration{calArch = "not-" <> hostArch}}

operatingPoint :: OperatingPoint
operatingPoint = OperatingPoint 5 4 (64 * 1024 * 1024)

lodashFigures :: Value
lodashFigures = object ["lodash" .= object ["full" .= (1000 :: Int), "singleVersion" .= (100 :: Int)]]

validCalibration :: Value
validCalibration = calibrationJson "abc123" ["https://example.test/runs/1"] 10

calibrationJson :: Text -> [Text] -> Int -> Value
calibrationJson commit runs margin =
    object ["arch" .= hostArch, "runner" .= ("ubuntu-24.04-arm" :: Text), "commit" .= commit, "runs" .= runs, "marginPercent" .= margin]

document :: Value -> [(Text, Value)] -> LByteString
document calibrationValue sections = encode (object ["calibration" .= calibrationValue, "allocatedBytes" .= Map.fromList sections])
