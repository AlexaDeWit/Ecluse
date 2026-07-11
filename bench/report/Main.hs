-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @bench-report@ entry point: read the work-per-request benchmark CSV (and,
when captured, the raw console log), render the structured Markdown report
("Ecluse.BenchReport"), print it, and mirror it to the GitHub step summary when
@GITHUB_STEP_SUMMARY@ is set -- the same self-append the load and acceptance
harnesses do, so the workflow step stays a single unwrapped command.

A missing or malformed file becomes a loud note __inside__ the report rather than a
failure: the bench run itself already reds the job on a genuine benchmark failure,
and a partial CSV from a red run is still worth rendering. The only non-zero exit is
a usage error.
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

-- Read a file as leniently decoded UTF-8, describing a failure instead of throwing:
-- an unreadable CSV becomes the report's loud note, and an unreadable console log
-- degrades to the report's log-not-captured note.
readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path = do
    result <- Exception.try (readFileBS path)
    pure $ case result of
        Left (e :: Exception.IOException) ->
            Left ("could not read " <> toText path <> ": " <> show e)
        Right bytes -> Right (TE.decodeUtf8With TEE.lenientDecode bytes)
