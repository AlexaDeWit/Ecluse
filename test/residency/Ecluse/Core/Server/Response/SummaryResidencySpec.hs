-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A completed refusal summary releases the decision list and its deferred diagnostics.
module Ecluse.Core.Server.Response.SummaryResidencySpec (spec) where

import Data.ByteString qualified as BS
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Rules.Types (Reason (RunsOnInstall))
import Ecluse.Core.Server.Response (RejectReason (ByPolicy), Rejection (Rejection), RuleName (RuleName), ServeDecision (Reject))
import Ecluse.Core.Server.Response.Summary (summarisedRejections)
import Ecluse.Test.Registry.Source (assertSourceHeld, awaitSourceRelease, trackedSource)

spec :: Spec
spec = describe "refusal summary retention" $ do
    it "keeps the tracked diagnostic allocation while decisions remain reachable" $ do
        released <- newEmptyMVar
        bracket (trackedDecisions released >>= newStablePtr) freeStablePtr $ \root -> do
            assertSourceHeld released
            decisions <- deRefStablePtr root
            evaluate (length decisions) `shouldReturn` 1000
        awaitSourceRelease released

    it "releases deferred per-version diagnostics while the evaluated summary remains reachable" $ do
        released <- newEmptyMVar
        bracket (completedSummary released >>= newStablePtr) freeStablePtr $ \root -> do
            awaitSourceRelease released
            deRefStablePtr root
                `shouldReturn` "BlockedByDefault: no allow rule matched (rejection entries: 1000)"

completedSummary :: MVar () -> IO Text
completedSummary released = trackedDecisions released >>= evaluate . summarisedRejections

trackedDecisions :: MVar () -> IO [ServeDecision]
trackedDecisions released = do
    source <- trackedSource "diagnostic" released
    pure [decision source n | n <- [1 .. 1000 :: Int]]
  where
    decision source n = Reject (Rejection (ByPolicy (RuleName "BlockedByDefault") [RunsOnInstall (decodeUtf8 (BS.take n source))]) "full per-version diagnostic")
