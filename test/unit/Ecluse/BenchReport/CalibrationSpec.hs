-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The floors three calibration runs write: 60% of the median of each cell's clean first attempts.
module Ecluse.BenchReport.CalibrationSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Calibration (..),
    CalibrationRun (CalibrationRun),
    Cell (Floored, ReportOnlyCell, Uncalibrated),
    FloorKey,
    Floors (..),
    OperatingPoint (OperatingPoint),
    Pass (ConcurrencyOne, Loaded),
    PassSelection (EveryPass),
    RunnerKind (RunnerKind),
 )
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), noTuning)
import Ecluse.BenchLoad.Support (twoCores)
import Ecluse.BenchReport.Calibration (FirstAttempt (FirstAttempt), RunObservations (..), calibrate, readRunsDirectory, renderCalibration)

spec :: Spec
spec = do
    describe "calibrate" $ do
        it "sets each cell to 60% of the median of its three first-attempt counts, and names the runs" $ do
            calibrated <- either (fail . toString . unlines) pure (calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            cellsOf calibrated
                `shouldBe` Map.fromList
                    [ (mergeCold, Floored 1200 2000 (11 :| [12, 13]))
                    , (mergeColdOne, Floored 600 1000 (11 :| [12, 13]))
                    , (herd, ReportOnlyCell "its burst successes split between admission levels")
                    ]
            map runId' (calRuns (floorsCalibration calibrated)) `shouldBe` [11, 12, 13]
        it "keeps a report-only cell flagged, whatever its counts" $ do
            calibrated <- either (fail . toString . unlines) pure (calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            Map.lookup herd (cellsOf calibrated) `shouldBe` Just (ReportOnlyCell "its burst successes split between admission levels")
        it "recalibrates only the named scenarios, keeping the other cells and the runs they name" $ do
            first' <- either (fail . toString . unlines) pure (calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            withPypi <- either (fail . toString . unlines) pure (calibrate (withCell pypiCold Uncalibrated first') [pypiRun 21 3000, pypiRun 22 3100, pypiRun 23 2900] (Just ["pypi/index-cold"]))
            Map.lookup mergeCold (cellsOf withPypi) `shouldBe` Just (Floored 1200 2000 (11 :| [12, 13]))
            Map.lookup pypiCold (cellsOf withPypi) `shouldBe` Just (Floored 1800 3000 (21 :| [22, 23]))
            map runId' (calRuns (floorsCalibration withPypi)) `shouldBe` [11, 12, 13, 21, 22, 23]
        it "drops a run no cell names any more" $ do
            first' <- either (fail . toString . unlines) pure (calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            again <- either (fail . toString . unlines) pure (calibrate first' [observed 31 [2000, 1000], observed 32 [2000, 1000], observed 33 [2000, 1000]] Nothing)
            map runId' (calRuns (floorsCalibration again)) `shouldBe` [31, 32, 33]
        it "refuses other than three distinct runs" $
            for_ [[observed 11 [1, 1], observed 12 [1, 1]], [observed 11 [1, 1], observed 11 [1, 1], observed 12 [1, 1]], [observed n [1, 1] | n <- [11 .. 14]]] $ \runs ->
                calibrate uncalibrated runs Nothing `shouldSatisfy` isLeft
        it "refuses runs that measured different commits, naming each run's commit" $
            calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observedAt "def" "workflow_dispatch" 13 [1900, 900]] Nothing
                `shouldBe` Left ["the runs measured different commits: run 11 at abc, run 12 at abc, run 13 at def"]
        it "refuses a run that was not a workflow dispatch, naming its event" $
            calibrate uncalibrated [observed 11 [2000, 1000], observedAt "abc" "schedule" 12 [2200, 1100], observed 13 [1900, 900]] Nothing
                `shouldBe` Left ["run 12 was triggered by schedule, not workflow_dispatch"]
        it "refuses a scenario the floors do not name" $
            calibrate uncalibrated [observed n [1, 1] | n <- [11 .. 13]] (Just ["npm/merge-colder"]) `shouldBe` Left ["no scenario in the floors is named npm/merge-colder"]
        it "names every run that was not held, missed a pod shape or a pass, or whose first attempt did not run clean" $ do
            let notHeld = (observed 11 [2000, 1000]){roShapes = Map.map (first (const False)) (roShapes (observed 11 [2000, 1000]))}
                noShape = (observed 12 [2000, 1000]){roShapes = mempty}
                unclean = (observed 13 [2000, 1000]){roShapes = Map.singleton twoCores (True, Map.fromList [(mergeCold, FirstAttempt (Just 2000) False), (mergeColdOne, FirstAttempt (Just 0) True)])}
            calibrate uncalibrated [notHeld, noShape, unclean] Nothing
                `shouldBe` Left
                    [ "run 11 under 2cpu-1gib was not held to the floors"
                    , "run 12 has no load observations under 2cpu-1gib"
                    , "run 13 under 2cpu-1gib: the first attempt at npm/merge-cold loaded did not run clean"
                    , "run 13 under 2cpu-1gib: the first attempt at npm/merge-cold concurrencyOne did not run clean"
                    ]
            calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2000, 1000], (observed 13 [2000, 1000]){roShapes = Map.singleton twoCores (True, mempty)}] Nothing
                `shouldBe` Left ["run 13 under 2cpu-1gib did not measure npm/merge-cold loaded", "run 13 under 2cpu-1gib did not measure npm/merge-cold concurrencyOne"]

    describe "calibrate at another floor fraction" $
        it "sets each floor to the file's floor fraction of the median" $ do
            let atHalf = uncalibrated{floorsCalibration = (floorsCalibration uncalibrated){calFloorFraction = 1 / 2}}
            calibrated <- either (fail . toString . unlines) pure (calibrate atHalf [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            Map.lookup mergeCold (cellsOf calibrated) `shouldBe` Just (Floored 1000 2000 (11 :| [12, 13]))

    describe "renderCalibration" $
        it "lists each changed cell's floor before and after, and its median" $ do
            calibrated <- either (fail . toString . unlines) pure (calibrate uncalibrated [observed 11 [2000, 1000], observed 12 [2200, 1100], observed 13 [1900, 900]] Nothing)
            let rendered = T.lines (renderCalibration uncalibrated calibrated)
            rendered `shouldSatisfy` elem "## Success floors from calibration runs 11, 12, 13"
            rendered `shouldSatisfy` elem "| 2cpu-1gib | npm/merge-cold | loaded | none | 1200 | 2000 |"
            rendered `shouldSatisfy` elem "| 2cpu-1gib | npm/merge-cold | concurrencyOne | none | 600 | 1000 |"
            rendered `shouldNotSatisfy` any (T.isInfixOf "npm/herd")

    describe "readRunsDirectory" $
        it "reads each run's record and the first attempts of every pod shape's observations" $
            withSystemTempDirectory "calibration" $ \root -> do
                createDirectoryIfMissing True (root </> "11" </> "bench-load-results-ARM64-2cpu-1gib-sha" </> "nested")
                writeFileLBS (root </> "11" </> "run.json") (encode (object ["databaseId" .= (11 :: Int), "url" .= ("https://example.test/runs/11" :: Text), "headSha" .= ("abc" :: Text), "event" .= ("workflow_dispatch" :: Text)]))
                writeFileLBS (root </> "11" </> "bench-load-results-ARM64-2cpu-1gib-sha" </> "nested" </> "load-observations.json") (encode (observationsFile "2cpu-1gib" "held"))
                writeFileLBS (root </> "11" </> "bench-load-results-ARM64-2cpu-1gib-sha" </> "nested" </> "other.json") "{}"
                readRunsDirectory root
                    `shouldReturn` Right
                        [ RunObservations
                            (CalibrationRun 11 "https://example.test/runs/11" "abc" "workflow_dispatch")
                            (Map.singleton twoCores (True, Map.fromList [(mergeCold, FirstAttempt (Just 2000) True), (mergeColdOne, FirstAttempt Nothing False)]))
                        ]
  where
    runId' (CalibrationRun runNumber _ _ _) = runNumber

mergeCold, mergeColdOne, herd, pypiCold :: FloorKey
mergeCold = ("npm/merge-cold", Loaded)
mergeColdOne = ("npm/merge-cold", ConcurrencyOne)
herd = ("npm/herd", Loaded)
pypiCold = ("pypi/index-cold", Loaded)

-- Two npm cells to calibrate and a flagged herd, under one pod shape, before any calibration.
uncalibrated :: Floors
uncalibrated =
    Floors
        "Example floors."
        (Calibration "unified-v3" "ubuntu-26.04-arm" (RunnerKind "Linux" "ARM64") (OperatingPoint 30 100 363520 20 3 64 Nothing Nothing Nothing Nothing [] EveryPass noTuning) 0.6 1 [])
        (Map.singleton twoCores (Map.fromList [(mergeCold, Uncalibrated), (mergeColdOne, Uncalibrated), (herd, ReportOnlyCell "its burst successes split between admission levels")]))

cellsOf :: Floors -> Map FloorKey Cell
cellsOf = Map.findWithDefault mempty twoCores . floorsByShape

withCell :: FloorKey -> Cell -> Floors -> Floors
withCell key cell floors = floors{floorsByShape = Map.adjust (Map.insert key cell) twoCores (floorsByShape floors)}

-- A run dispatched at commit abc, held under 2cpu-1gib with clean first attempts at these merge-cold counts, loaded then concurrency one, and a herd.
observed :: Integer -> [Int] -> RunObservations
observed = observedAt "abc" "workflow_dispatch"

-- The same run at this commit, triggered by this event.
observedAt :: Text -> Text -> Integer -> [Int] -> RunObservations
observedAt commit event runNumber counts =
    RunObservations
        (CalibrationRun runNumber ("https://example.test/runs/" <> show runNumber) commit event)
        (Map.fromList [(twoCores, (True, Map.fromList (zip [mergeCold, mergeColdOne] [FirstAttempt (Just count) True | count <- counts] <> [(herd, FirstAttempt (Just 3) True)]))), (Unlimited, (True, mempty))])

-- A held run under 2cpu-1gib with a clean first attempt at PyPI's index-cold.
pypiRun :: Integer -> Int -> RunObservations
pypiRun runNumber count = RunObservations (CalibrationRun runNumber ("https://example.test/runs/" <> show runNumber) "def" "workflow_dispatch") (Map.singleton twoCores (True, Map.singleton pypiCold (FirstAttempt (Just count) True)))

-- One job's load-observations.json, as the harness writes it.
observationsFile :: Text -> Text -> Value
observationsFile shape enforcement =
    object
        [ "outcome" .= ("passed" :: Text)
        , "conditions" .= object ["podShape" .= shape, "floorEnforcement" .= enforcement, "measurementConditions" .= ("unified-v3" :: Text)]
        , "observations"
            .= [ object ["id" .= ("npm/merge-cold/loaded" :: Text), "outcome" .= ("passed" :: Text), "attempts" .= [object ["successes" .= (2000 :: Int), "clean" .= True], object ["successes" .= (2100 :: Int), "clean" .= True]]]
               , object ["id" .= ("npm/merge-cold/concurrencyOne" :: Text), "outcome" .= ("failed" :: Text), "attempts" .= [object ["successes" .= (Nothing :: Maybe Int), "clean" .= False]]]
               ]
        ]
