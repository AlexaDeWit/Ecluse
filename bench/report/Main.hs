-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Render native reports and bind their observations to independently compiled obligations.
module Main (main) where

import Control.Exception qualified as Exception
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import System.FilePath (takeFileName, (</>))

import Ecluse.BenchLoad.Floors (decodeFloors, loadFloors, renderFloorsFile)
import Ecluse.BenchLoad.Sweep (loadSweep, sweepJob)
import Ecluse.BenchReport (ReportInput (ReportInput, riConsoleLog, riCsv), parseCsv, renderReport)
import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Missing (FileMissing), parseOrigin)
import Ecluse.BenchReport.AgainstMain.Acceptance qualified as Acceptance
import Ecluse.BenchReport.AgainstMain.Bench qualified as Bench
import Ecluse.BenchReport.AgainstMain.Load qualified as Load
import Ecluse.BenchReport.Calibration (calibrate, readRunsDirectory, renderCalibration)
import Ecluse.BenchReport.PerformanceCommand qualified as Performance
import Ecluse.BenchReport.PerformanceReceipt qualified as Receipt
import Ecluse.BenchReport.Sweep qualified as Sweep

main :: IO ()
main =
    getArgs >>= \case
        "conditions" : path : arguments -> Receipt.writeConditions path (map toText arguments)
        ["receipt-allocation", root, conditions] -> Receipt.writePerformanceReceipts "allocation" root conditions
        ["receipt-live-allocation", root, conditions] -> Receipt.writePerformanceReceipts "live-allocation" root conditions
        ["receipt-benchmark", root, conditions] -> Receipt.writePerformanceReceipts "benchmark" root conditions
        ["receipt-load", root, conditions] -> Receipt.writePerformanceReceipts "load" root conditions
        ["receipt-load-foundation", root, conditions] -> Receipt.writePerformanceReceipts "load-foundation" root conditions
        ["performance-obligation", kind, subject, conditionsPath] -> Performance.writeObligation (toText kind) (toText subject) conditionsPath
        ["performance-receipt", requestPath, nativePath, receiptPath] -> Performance.writeReceipt requestPath nativePath receiptPath
        ["performance-audit", requestPath, receiptPath] -> Performance.auditReceipt requestPath receiptPath
        [csvPath] -> run csvPath Nothing
        [csvPath, logPath] -> run csvPath (Just logPath)
        ["against-main", kind, baselineDir, reportPath]
            | Just compareWith <- comparison kind -> againstMain compareWith baselineDir reportPath
        "sweep" : name : job : directory : reportPaths -> againstAnchors (toText name) (toText job) directory reportPaths
        ["floors", floorsFile, runsDir] -> writeFloors floorsFile runsDir Nothing
        ["floors", floorsFile, runsDir, scenarios] -> writeFloors floorsFile runsDir (Just (filter (not . T.null) (map T.strip (T.splitOn "," (toText scenarios)))))
        _ ->
            die . toString . unlines $
                [ "usage: bench-report <results.csv> [<console-log>]"
                , "       bench-report conditions <path> <key=value> ..."
                , "       bench-report receipt-{allocation,live-allocation,benchmark,load,load-foundation} <root> <conditions.json>"
                , "       bench-report performance-obligation <kind> <subject> <conditions.json>"
                , "       bench-report performance-receipt <request.json> <native-results> <receipt.json>"
                , "       bench-report performance-audit <request.json> <receipt.json>"
                , "       bench-report against-main (bench | acceptance | load) <baseline-dir> <report>"
                , "       bench-report sweep <sweep> <job> <job-dir> <report> ..."
                , "       bench-report floors <floors.json> <runs-dir> [<scenario>,...]"
                ]

run :: FilePath -> Maybe FilePath -> IO ()
run csvPath logPath = do
    csv <- readTextFile csvPath
    consoleLog <- traverse readTextFile logPath
    publish $
        renderReport
            ReportInput
                { riCsv = parseCsv =<< csv
                , riConsoleLog = rightToMaybe =<< consoleLog
                }

comparison :: String -> Maybe (Baseline Text -> Either Text Text -> Text)
comparison = \case
    "bench" -> Just Bench.againstMain
    "acceptance" -> Just Acceptance.againstMain
    "load" -> Just Load.againstMain
    _ -> Nothing

-- The baseline directory holds the fetch's record and, when it found a run, that run's report under this report's file name.
againstMain :: (Baseline Text -> Either Text Text -> Text) -> FilePath -> FilePath -> IO ()
againstMain compareWith baselineDir reportPath = do
    record <- fetched "baseline.txt"
    onMain <- fetched (takeFileName reportPath)
    current <- first (\problem -> "A file is missing: " <> problem <> ".") <$> readTextFile reportPath
    publish (compareWith (either NoBaseline id (Baseline <$> (record >>= parseOrigin) <*> onMain)) current)
  where
    fetched name = first FileMissing <$> readTextFile (baselineDir </> name)

publish :: Text -> IO ()
publish output = do
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)

-- One sweep job's reports, in slot order, beside the job's anchors, written into the job's directory.
againstAnchors :: Text -> Text -> FilePath -> [FilePath] -> IO ()
againstAnchors name job directory reportPaths = do
    sweep <- either (die . toString) pure =<< loadSweep name
    floors <- either (die . toString) pure =<< loadFloors
    chosen <- maybe (die ("bench-report: the sweep " <> toString name <> " has no job " <> toString job)) pure (sweepJob sweep job)
    reports <- traverse readTextFile reportPaths
    -- Task names the commit it measures, and a workflow step outside Task has the runner's.
    source <- lookupEnv "ECLTEST_SOURCE"
    sha <- lookupEnv "GITHUB_SHA"
    let compared = Sweep.compareJob (toText <$> (source <|> sha)) (Sweep.referenceFloors floors) sweep chosen reports
    Sweep.writeComparison directory compared
    publish (Sweep.renderComparison compared)

-- Set each selected cell of the floors file from the three runs under the directory, and print what changed.
writeFloors :: FilePath -> FilePath -> Maybe [Text] -> IO ()
writeFloors floorsFile runsDir selection = do
    floors <- either (die . toString) pure . decodeFloors =<< readFileLBS floorsFile
    runs <- either (die . toString . unlines) pure =<< readRunsDirectory runsDir
    updated <- either (die . toString . unlines) pure (calibrate floors runs selection)
    writeFileText floorsFile (renderFloorsFile updated)
    publish (renderCalibration floors updated)

-- Lenient UTF-8, and a described failure instead of a throw, so an unreadable file becomes a note in the output.
readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = do
    result <- Exception.try (readFileBS path)
    pure $ case result of
        Left (e :: Exception.IOException) ->
            Left ("could not read " <> toText path <> ": " <> show e)
        Right bytes -> Right (TE.decodeUtf8With TEE.lenientDecode bytes)
