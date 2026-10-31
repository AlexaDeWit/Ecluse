-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Attempt publication retains current failed evidence and the native exit status.
module Ecluse.Test.Contract.Artifact.Attempt (
    beginAttempt,
    finishAttempt,
    continueAttempt,
) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))

import Ecluse.Test.Contract.Artifact.Identity (verifyPreparation)
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Result

-- | Invalidate old reports before setup. The caller holds the artifact lock throughout the attempt.
beginAttempt :: FilePath -> Text -> FilePath -> IO ()
beginAttempt root shard attempt = do
    validateShard shard
    createDirectoryIfMissing True attempt
    forM_ [root </> "reports" </> toString shard <> ".json", root </> "reports/all.json"] $ \path -> do
        exists <- doesFileExist path
        when exists $ renameFile path (attempt </> "previous-" <> takeFileName path)
    writeArtifactFile (attempt </> "status") "running\n"

-- | Publish a nonempty current report before returning its native failure, including exit 7.
finishAttempt :: FilePath -> Text -> FilePath -> Int -> IO ExitCode
finishAttempt root shard attempt status = do
    validateShard shard
    unless (status >= 0 && status <= 255) $ failRunner "invalid native exit status"
    writeArtifactFile (attempt </> "status") (encodeUtf8 (show status <> "\n" :: Text))
    exists <- doesFileExist (attempt </> "report.json")
    bytes <- if exists then BS.readFile (attempt </> "report.json") else pure BS.empty
    if BS.null bytes
        then do
            when (status == 0) $ writeArtifactFile (attempt </> "status") "missing-report\n"
            pure (ExitFailure (if status == 0 then 1 else status))
        else do
            writeArtifactFile (root </> "reports" </> toString shard <> ".json") bytes
            pure (if status == 0 then ExitSuccess else ExitFailure status)

-- | Continue a full run only after complete ordinary case results from this attempt.
continueAttempt :: FilePath -> Text -> FilePath -> IO ()
continueAttempt root shard attempt = do
    validateShard shard
    expected <- verifyPreparation root Nothing
    plan <- readJsonFile (root </> "plan.json")
    report <- readJsonFile (attempt </> "report.json")
    current <- BS.readFile (root </> "reports" </> toString shard <> ".json")
    actual <- BS.readFile (attempt </> "report.json")
    status <- TE.decodeUtf8 <$> BS.readFile (attempt </> "status")
    cleanup <- TE.decodeUtf8 <$> BS.readFile (attempt </> "cleanup-status")
    let problems =
            ["container cleanup did not pass" | T.strip cleanup /= "0"]
                <> ["current report differs from attempt" | actual /= current]
                <> ["current report belongs to another subject" | reportShard report /= shard]
                <> continuationProblems expected plan (readMaybe (toString (T.strip status))) report
    unless (null problems) $ failRunner (T.unlines problems)

-- Setup, cancellation, timeout, pending results and incomplete declarations stop aggregation.
continuationProblems :: ExecutionIdentity -> Plan -> Maybe Int -> Report -> [Text]
continuationProblems expected plan status report =
    ["attempt did not finish ordinarily" | maybe True (\code -> code `notElem` [0, 1]) status]
        <> ["failed process has no failed case evidence" | maybe False (/= 0) status, all ((/= Failed) . crStatus) (reportCases report)]
        <> ["attempt contains incomplete execution" | any (`notElem` [Passed, Failed]) statuses]
        <> auditPartitionReport expected plan ordinary
  where
    statuses = map grStatus (reportGroups report) <> map crStatus (reportCases report)
    ordinary =
        report
            { reportGroups = [result{grStatus = Passed} | result <- reportGroups report]
            , reportCases = [result{crStatus = Passed} | result <- reportCases report]
            }

validateShard :: Text -> IO ()
validateShard shard = unless valid $ failRunner "invalid shard name"
  where
    letter char = char `elem` ("abcdefghijklmnopqrstuvwxyz0123456789" :: String)
    valid = case T.uncons shard of
        Nothing -> False
        Just (initial, rest) -> letter initial && T.all (\char -> letter char || char == '-') rest
