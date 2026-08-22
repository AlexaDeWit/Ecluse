-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @bench-report@ entry point. It reads the work-per-request benchmark CSV and,
when captured, the raw console log. It renders the structured Markdown report
("Ecluse.BenchReport"), prints it, and mirrors it to the GitHub step summary when
@GITHUB_STEP_SUMMARY@ is set. The load and acceptance harnesses self-append the same
way, so the workflow step stays a single unwrapped command.

A missing or malformed file becomes a loud note inside the report rather than a
failure. The bench run itself already reds the job on a genuine benchmark failure, and
a partial CSV from a red run is still worth rendering. The only non-zero exit is a
usage error.
-}
module Main (main) where

import Control.Exception qualified as Exception
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE

import Ecluse.BenchReport (ReportInput (ReportInput, riConsoleLog, riCsv), parseCsv, renderReport)

main :: IO ()
main =
    getArgs >>= \case
        [csvPath] -> run csvPath Nothing
        [csvPath, logPath] -> run csvPath (Just logPath)
        _ -> die "usage: bench-report <results.csv> [<console-log>]"

run :: FilePath -> Maybe FilePath -> IO ()
run csvPath logPath = do
    csv <- readTextFile csvPath
    consoleLog <- traverse readTextFile logPath
    let output =
            renderReport
                ReportInput
                    { riCsv = parseCsv =<< csv
                    , riConsoleLog = rightToMaybe =<< consoleLog
                    }
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)

-- Lenient UTF-8, and a described failure instead of a throw: an unreadable CSV becomes the
-- report's loud note, and an unreadable console log its log-not-captured note.
readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = do
    result <- Exception.try (readFileBS path)
    pure $ case result of
        Left (e :: Exception.IOException) ->
            Left ("could not read " <> toText path <> ": " <> show e)
        Right bytes -> Right (TE.decodeUtf8With TEE.lenientDecode bytes)
