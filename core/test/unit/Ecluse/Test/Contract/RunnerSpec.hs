-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deliberate broken providers prove that the runner cannot report incomplete work as passed.
module Ecluse.Test.Contract.RunnerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as Exception
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Test.Contract.Catalogue
import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Options
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Runner
import Ecluse.Test.Contract.Types

spec :: Spec
spec = describe "executeGroups" $ do
    it "executes assertions and releases a lifecycle exactly once" $ do
        lifecycle <- newIORef (0 :: Int)
        let body =
                aroundAll_ (Exception.bracket_ (modifyIORef' lifecycle (+ 1)) (modifyIORef' lifecycle (+ 1))) $
                    it label $
                        readIORef lifecycle >>= (`shouldBe` 1)
        actual <- execute body (const $ pure Nothing)
        auditSelection [group] (reportGroups actual) (reportCases actual) `shouldBe` []
        readIORef lifecycle `shouldReturn` 2
    it "retains pending as a failing required result" $ do
        actual <- execute (it label $ pendingWith "unavailable fixture") (const $ pure Nothing)
        map crStatus (reportCases actual) `shouldBe` [Pending]
        map grStatus (reportGroups actual) `shouldBe` [Failed]
    it "rejects a success without the expected example" $ do
        actual <- execute (pure ()) (const $ pure Nothing)
        map crStatus (reportCases actual) `shouldBe` [SetupFailed]
    it "records unreadable prerequisites as setup failure without running the case" $ do
        ran <- newIORef False
        actual <- execute (it label $ writeIORef ran True) (const $ pure $ Just "image missing")
        readIORef ran `shouldReturn` False
        map crStatus (reportCases actual) `shouldBe` [SetupFailed]
        concatMap grDiagnostics (reportGroups actual) `shouldSatisfy` elem "image missing"
    it "records failed assertions" $ do
        actual <- execute (it label $ expectationFailure "broken observation") (const $ pure Nothing)
        map crStatus (reportCases actual) `shouldBe` [Failed]
    it "rejects a provider that labels a different case" $ do
        actual <- execute (it "[unknown] example" $ pure ()) (const $ pure Nothing)
        auditSelection [group] (reportGroups actual) (reportCases actual) `shouldSatisfy` (not . null)
    it "rejects cleanup failure even after the example passes" $ do
        actual <- execute (afterAll_ (Exception.throwIO CleanupFailure) $ it label $ pure ()) (const $ pure Nothing)
        map grStatus (reportGroups actual) `shouldBe` [Failed]
    it "bounds setup and records timeout rather than passing omitted cases" $ do
        actual <- executeGroups identity options{runTimeoutSeconds = Just 1} [group] (Map.singleton (groupId group) $ const $ it label $ pure ()) (const $ threadDelay 2000000 >> pure Nothing)
        map grStatus (reportGroups actual) `shouldBe` [TimedOut]
        map crStatus (reportCases actual) `shouldBe` [TimedOut]
    it "persists cancellation evidence before rethrowing the asynchronous exception" $
        withSystemTempDirectory "contract-runner" $ \directory -> do
            let path = directory </> "cancelled.json"
            outcome <-
                Exception.try @Exception.AsyncException $
                    executeGroups identity options{runReportPath = Just path} [group] Map.empty (const $ Exception.throwIO Exception.ThreadKilled)
            outcome `shouldSatisfy` isLeft
            decoded <- eitherDecodeStrict' <$> BS.readFile path
            case decoded of
                Left reason -> expectationFailure reason
                Right actual -> map grStatus (reportGroups actual) `shouldBe` [Cancelled]
    it "runs independent groups alone and in reverse order without changing their case set" $ do
        let otherItem = Case "example/read/other" ["read/identity"]
            otherGroup = group{groupId = "example/other", groupCases = [otherItem]}
            actions = Map.fromList [(groupId group, const $ it label $ pure ()), (groupId otherGroup, const $ it (caseLabel (caseId otherItem) "other") $ pure ())]
        forM_ [[otherGroup], [otherGroup, group], [group, otherGroup]] $ \selected -> do
            actual <- executeGroups identity options selected actions (const $ pure Nothing)
            auditSelection selected (reportGroups actual) (reportCases actual) `shouldBe` []
  where
    label = caseLabel (caseId item) "identity"
    options = RunOptions emptySelection Nothing 1 Nothing False
    execute body = executeGroups identity options [group] (Map.singleton (groupId group) $ const body)

data CleanupFailure = CleanupFailure deriving stock (Show)
instance Exception CleanupFailure
