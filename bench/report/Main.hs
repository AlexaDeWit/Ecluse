-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @bench-report@ entry point. Its first form renders the work-per-request CSV, with the
console log when captured ("Ecluse.BenchReport"). Its @against-main@ form sets one report
beside the results @scripts/perf-baseline.sh@ fetched from @main@
("Ecluse.BenchReport.AgainstMain"). Both print Markdown and append it to
@GITHUB_STEP_SUMMARY@ when set. A missing or malformed file becomes a note in the output,
so the only non-zero exit is a usage error.
-}
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

main :: IO ()
main =
    getArgs >>= \case
        [csvPath] -> run csvPath Nothing
        [csvPath, logPath] -> run csvPath (Just logPath)
        ["against-main", kind, baselineDir, reportPath]
            | Just compareWith <- comparison kind -> againstMain compareWith baselineDir reportPath
        _ ->
            die . toString . unlines $
                [ "usage: bench-report <results.csv> [<console-log>]"
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

-- Lenient UTF-8, and a described failure instead of a throw, so an unreadable file becomes a note in the output.
readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = do
    result <- Exception.try (readFileBS path)
    pure $ case result of
        Left (e :: Exception.IOException) ->
            Left ("could not read " <> toText path <> ": " <> show e)
        Right bytes -> Right (TE.decodeUtf8With TEE.lenientDecode bytes)
