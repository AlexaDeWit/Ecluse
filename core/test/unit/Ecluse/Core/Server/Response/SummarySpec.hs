-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Grouped refusal text preserves causes and entry counts without per-version diagnostics.
module Ecluse.Core.Server.Response.SummarySpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Rules.Types (Reason (..), mkAdvisoryIds)
import Ecluse.Core.Server.Response (RejectReason (..), Rejection (..), RuleName (..), ServeDecision (..))
import Ecluse.Core.Server.Response.Summary (detailedRejections, summarisedRejections)

spec :: Spec
spec = describe "grouped rejection text" $ do
    it "keeps varying ages, versions and advisory identifiers inside their deciding rule group" $ do
        let decisions = concatMap entry [1 .. 4000 :: Int]
        summarisedRejections decisions
            `shouldBe` "BlockedByDefault: no allow rule matched (rejection entries: 4000). DenyIfKnownMalicious: known malicious advisories affect the versions (rejection entries: 4000)"
        T.length (summarisedRejections decisions) `shouldSatisfy` (< 250)

    it "counts repeated versions from different source contributions as separate rejection entries" $ do
        let contribution = [deny "DenyInstallTimeExecution" [RunsOnInstall "postinstall"] "thing@1.0.0 install script"]
        summarisedRejections (contribution <> contribution)
            `shouldBe` "DenyInstallTimeExecution: installation runs code (rejection entries: 2)"

    it "keeps each integrity category separate from policy denials in a deterministic order" $
        summarisedRejections [Reject (Rejection BelowIntegrityFloor "sha1"), deny "custom" [] "custom detail", Reject (Rejection MissingIntegrity "1.0.0 lacks digest"), Reject (Rejection MissingIntegrity "2.0.0 lacks digest")]
            `shouldBe` "custom: the policy rule denied admission (rejection entries: 1). MissingIntegrity: public artifacts lack an integrity digest (rejection entries: 2). BelowIntegrityFloor: public artifact digests fall below the configured integrity floor (rejection entries: 1)"

    it "does not evaluate per-version policy reasons" $
        summarisedRejections [deny "BlockedByDefault" [impureThrow DiagnosticRead] "full diagnostic"]
            `shouldBe` "BlockedByDefault: no allow rule matched (rejection entries: 1)"

    it "finishes the decision list before the returned text reaches weak head normal form" $
        evaluate (summarisedRejections (deny "BlockedByDefault" [] "detail" : impureThrow DiagnosticRead))
            `shouldThrow` (== DiagnosticRead)

    it "retains the empty-listing fallback and ignores admissions" $
        summarisedRejections [Admit] `shouldBe` detailedRejections []

    it "keeps the detailed renderer's existing messages" $
        detailedRejections [Admit, deny "first" [] "first detail", Reject (Rejection MissingIntegrity "second detail")]
            `shouldBe` "first detail; second detail"
  where
    entry n =
        [ deny "BlockedByDefault" [PublishedTooRecently (fromIntegral n) 604800] ("thing@" <> show n <> " is too new")
        , deny "DenyIfKnownMalicious" [KnownMalicious (mkAdvisoryIds (("MAL-" <> show n) :| []))] ("thing@" <> show n <> " MAL-" <> show n)
        ]

deny :: Text -> [Reason] -> Text -> ServeDecision
deny name reasons = Reject . Rejection (ByPolicy (RuleName name) reasons)

data DiagnosticRead = DiagnosticRead
    deriving stock (Eq, Show)

instance Exception DiagnosticRead
