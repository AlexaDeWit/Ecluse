-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Render native reports and bind their observations to independently compiled obligations.
module Main (main) where

import Control.Exception qualified as Exception
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import System.FilePath (takeFileName, (</>))

import Ecluse.BenchReport (ReportInput (ReportInput, riConsoleLog, riCsv), parseCsv, renderReport)
import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Missing (FileMissing), parseOrigin)
import Ecluse.BenchReport.AgainstMain.Acceptance qualified as Acceptance
import Ecluse.BenchReport.AgainstMain.Bench qualified as Bench
import Ecluse.BenchReport.AgainstMain.Load qualified as Load
import Ecluse.BenchReport.AllocationDiagnostic qualified as AllocationDiagnostic
import Ecluse.BenchReport.AllocationDiagnostic.Controls qualified as AllocationDiagnosticControls
import Ecluse.BenchReport.PerformanceCommand qualified as Performance
import Ecluse.BenchReport.PerformanceReceipt qualified as Receipt
import Ecluse.BenchReport.RefactorComparison qualified as Comparison
import Ecluse.BenchReport.RefactorEvidence qualified as ComparisonEvidence
import Ecluse.BenchReport.RefactorInputs qualified as ComparisonInputs

-- | Route report and diagnostic commands without changing measured product code.
main :: IO ()
main =
    getArgs >>= \case
        ["allocation-diagnostic-controls", root] -> AllocationDiagnosticControls.controls root
        ["allocation-diagnostic-build", root, build, source] -> AllocationDiagnostic.recordBuild root (toText build) (toText source)
        ["allocation-diagnostic-conditions", root, passName] -> AllocationDiagnostic.writeConditions root (toText passName)
        ["allocation-diagnostic-bind", root, passName] -> AllocationDiagnostic.bindPass root (toText passName)
        ["allocation-diagnostic-run-bind", root] -> AllocationDiagnostic.bindRun root
        ["allocation-diagnostic-audit", root, runId, attempt] -> AllocationDiagnostic.audit root (toText runId) (toText attempt)
        ["comparison-inputs", root] -> ComparisonInputs.prepareInputs root
        ["comparison-pass-conditions", root, kind, passName] -> ComparisonEvidence.writePassConditions root (toText kind) (toText passName)
        ["comparison-bind", root, kind, passName] -> ComparisonEvidence.recordBinding root (toText kind) (toText passName)
        ["comparison-controls", manifest] -> Comparison.controls manifest
        ["comparison-audit", root, runId, attempt, baseline, baselineTree, candidate, candidateTree, manifest] -> Comparison.audit root (toText runId) (toText attempt) (toText baseline) (toText baselineTree) (toText candidate) (toText candidateTree) manifest
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
        _ ->
            die . toString . unlines $
                [ "usage: bench-report <results.csv> [<console-log>]"
                , "       bench-report conditions <path> <key=value> ..."
                , "       bench-report receipt-{allocation,live-allocation,benchmark,load,load-foundation} <root> <conditions.json>"
                , "       bench-report performance-obligation <kind> <subject> <conditions.json>"
                , "       bench-report performance-receipt <request.json> <native-results> <receipt.json>"
                , "       bench-report performance-audit <request.json> <receipt.json>"
                , "       bench-report against-main (bench | acceptance | load) <baseline-dir> <report>"
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

-- The baseline directory pairs the fetch record with the report under its original name.
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

-- Lenient UTF-8, and a described failure instead of a throw, so an unreadable file becomes a note in the output.
readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = do
    result <- Exception.try (readFileBS path)
    pure $ case result of
        Left (e :: Exception.IOException) ->
            Left ("could not read " <> toText path <> ": " <> show e)
        Right bytes -> Right (TE.decodeUtf8With TEE.lenientDecode bytes)
