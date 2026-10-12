-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Current failed reports survive publication, while incomplete runs stop aggregation.
module Ecluse.Test.Contract.Artifact.AttemptSpec (spec) where

import Data.ByteString qualified as BS
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Test.Contract.Artifact.Attempt
import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Result
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "attempt publication" $ do
        it "invalidates every prior publication before setup starts" $
            withSystemTempDirectory "attempt" $ \root -> do
                writeJsonFile (root </> "reports/primary.json") report
                writeJsonFile (root </> "reports/all.json") report
                beginAttempt root "primary" (root </> "attempt")
                doesFileExist (root </> "reports/primary.json") `shouldReturn` False
                doesFileExist (root </> "reports/all.json") `shouldReturn` False
                BS.readFile (root </> "attempt/previous-primary.json") `shouldReturn` jsonLine report
                finishAttempt root "primary" (root </> "attempt") 7 `shouldReturn` ExitFailure 7
                doesFileExist (root </> "reports/primary.json") `shouldReturn` False
        it "publishes the current failed report and returns native exit 7" $
            withSystemTempDirectory "attempt" $ \root -> do
                beginAttempt root "primary" (root </> "attempt")
                writeJsonFile (root </> "attempt/report.json") failedReport
                finishAttempt root "primary" (root </> "attempt") 7 `shouldReturn` ExitFailure 7
                BS.readFile (root </> "reports/primary.json") `shouldReturn` jsonLine failedReport
                BS.readFile (root </> "attempt/status") `shouldReturn` "7\n"
                doesFileExist (root </> "reports/primary.json.tmp") `shouldReturn` False
        it "rejects a successful process with no current report" $
            withSystemTempDirectory "attempt" $ \root -> do
                beginAttempt root "primary" (root </> "attempt")
                finishAttempt root "primary" (root </> "attempt") 0 `shouldReturn` ExitFailure 1
                BS.readFile (root </> "attempt/status") `shouldReturn` "missing-report\n"
        it "keeps independent roots separate" $
            withSystemTempDirectory "attempt" $ \root -> do
                beginAttempt (root </> "one") "primary" (root </> "one/attempt")
                beginAttempt (root </> "two") "primary" (root </> "two/attempt")
                writeJsonFile (root </> "one/attempt/report.json") failedReport
                finishAttempt (root </> "one") "primary" (root </> "one/attempt") 7 `shouldReturn` ExitFailure 7
                doesFileExist (root </> "two/reports/primary.json") `shouldReturn` False
    describe "aggregate continuation" $ do
        it "accepts complete ordinary failures without changing their stored verdict" $ do
            plan <- expectRight (makePlan catalogue)
            continuationProblems executionIdentity plan (Just 1) failedReport `shouldBe` []
        it "stops on setup failures, cancellation, pending results and timeouts" $ do
            plan <- expectRight (makePlan catalogue)
            forM_ [SetupFailed, Cancelled, Pending, TimedOut] $ \status ->
                continuationProblems executionIdentity plan (Just 1) (withStatus status) `shouldSatisfy` (not . null)
            forM_ [Nothing, Just 7, Just 124, Just 129, Just 130, Just 143] $ \status ->
                continuationProblems executionIdentity plan status failedReport `shouldSatisfy` (not . null)
        it "rejects missing cases, stale identities and unreported native failures" $ do
            plan <- expectRight (makePlan catalogue)
            forM_ [failedReport{reportCases = []}, failedReport{reportIdentity = executionIdentity{identityRunId = "old"}}, report] $ \actual ->
                continuationProblems executionIdentity plan (Just 1) actual `shouldSatisfy` (not . null)

failedReport :: Report
failedReport = withStatus Failed

withStatus :: Status -> Report
withStatus status =
    report
        { reportGroups = [result{grStatus = status} | result <- reportGroups report]
        , reportCases = [result{crStatus = status} | result <- reportCases report]
        }
