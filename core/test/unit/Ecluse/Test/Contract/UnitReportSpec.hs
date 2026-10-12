-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Unit receipts must preserve formatter delivery and record actual failed or missing outcomes.
module Ecluse.Test.Contract.UnitReportSpec (spec) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as BS
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Core.Format qualified as Format

import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.UnitReport

spec :: Spec
spec = describe "unit execution receipts" $ do
    it "never replaces missing execution with invented successful cases" $ do
        let actual = unitReport executionIdentity "primary" 1 [scenarioGroup] []
        reportCases actual `shouldBe` []
        map grStatus (reportGroups actual) `shouldBe` [Failed]
    it "requires every declared example to pass" $
        forM_ [Failed, Pending, SetupFailed, Cancelled, TimedOut] $ \status -> do
            let result = CaseResult (caseId item) (groupId scenarioGroup) status 1 []
                actual = unitReport executionIdentity "primary" 1 [scenarioGroup] [result]
            reportCases actual `shouldBe` [result]
            map grStatus (reportGroups actual) `shouldBe` [Failed]
    it "preserves the selected formatter and records its actual successful case" $
        withSystemTempDirectory "unit-receipt" $ \directory -> do
            events <- newIORef (0 :: Int)
            let path = directory </> "report.json"
                delegate _ = pure $ \_ -> modifyIORef' events (+ 1)
            format <- wrapUnitFormatter path executionIdentity "primary" [scenarioGroup] delegate Format.defaultFormatConfig
            format (Format.ItemDone ([], "[example/read/identity] observes identity") passedItem)
            format (Format.Done [])
            readIORef events `shouldReturn` 2
            actual <- eitherDecodeStrict' <$> BS.readFile path
            case actual of
                Left reason -> expectationFailure reason
                Right receipt -> auditSelection [scenarioGroup] (reportGroups receipt) (reportCases receipt) `shouldBe` []
    it "persists a pending case before rejecting the incomplete receipt" $
        withSystemTempDirectory "unit-receipt" $ \directory -> do
            let path = directory </> "report.json"
                pendingItem = passedItem{Format.itemResult = Format.Pending Nothing (Just "missing operation")}
            format <- wrapUnitFormatter path executionIdentity "primary" [scenarioGroup] (const $ pure $ const $ pure ()) Format.defaultFormatConfig
            format (Format.ItemDone ([], "[example/read/identity] observes identity") pendingItem)
            format (Format.Done []) `shouldThrow` (== ExitFailure 1)
            actual <- eitherDecodeStrict' <$> BS.readFile path
            case actual of
                Left reason -> expectationFailure reason
                Right receipt -> map crStatus (reportCases receipt) `shouldBe` [Pending]
    it "rejects a failed cleanup even after every canonical case passed" $
        withSystemTempDirectory "unit-receipt" $ \directory -> do
            let path = directory </> "report.json"
                failedHook = passedItem{Format.itemResult = Format.Failure Nothing (Format.Reason "cleanup failed")}
            format <- wrapUnitFormatter path executionIdentity "primary" [scenarioGroup] (const $ pure $ const $ pure ()) Format.defaultFormatConfig
            format (Format.ItemDone ([], "[example/read/identity] observes identity") passedItem)
            format (Format.ItemDone ([], "afterAll-hook") failedHook)
            format (Format.Done []) `shouldThrow` (== ExitFailure 1)
    it "rejects a filtered execution even when the formatter reports no failures" $
        withSystemTempDirectory "unit-receipt" $ \directory -> do
            format <- wrapUnitFormatter (directory </> "report.json") executionIdentity "primary" [scenarioGroup] (const $ pure $ const $ pure ()) Format.defaultFormatConfig
            format (Format.Done []) `shouldThrow` (== ExitFailure 1)

passedItem :: Format.Item
passedItem = Format.Item Nothing (Format.Seconds 1) "" Format.Success
