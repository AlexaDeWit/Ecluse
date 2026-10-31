-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Grouped refusal text preserves causes and entry counts without per-version diagnostics.
module Ecluse.Core.Server.Response.SummarySpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Rules.Types (AdvisoryScore (Epss), Reason (..), mkAdvisoryIds)
import Ecluse.Core.Server.Response (RejectReason (..), Rejection (..), RuleName (..), ServeDecision (..))
import Ecluse.Core.Server.Response.Summary (detailedRejections, summarisedRejections)
import Ecluse.Test.Rules (exposure, revocation)

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

    for_
        [ ("DenyIfCve", exposure, "DenyIfCve: advisories deny admission under the configured severity policy (rejection entries: 1)")
        , ("DenyIfEpss", AffectedBy Epss 0.5 (mkAdvisoryIds ("GHSA-test-0001" :| [])), "DenyIfEpss: advisories deny admission under the configured exploit probability policy (rejection entries: 1)")
        , ("DenyExternalDependencies", FetchesOutside "dependency 'left-pad' in dependencies names a git source", "DenyExternalDependencies: dependencies install from outside the registry (rejection entries: 1)")
        , ("DenyIfYanked", VersionYanked, "DenyIfYanked: the versions are yanked (rejection entries: 1)")
        , ("DenyIfDeprecated", VersionDeprecated, "DenyIfDeprecated: the versions are deprecated (rejection entries: 1)")
        , ("DenyByIdentity", revocation, "DenyByIdentity: the operator revoked the package identity (rejection entries: 1)")
        ]
        $ \(name, reason, expected) ->
            it ("gives the short policy explanation for " <> toString name) $
                summarisedRejections [deny name [reason] "version-specific diagnostic"] `shouldBe` expected

    it "keeps each cause and its rejection count when admissions surround and separate the denied entries" $
        summarisedRejections
            [ Admit
            , deny "DenyIfYanked" [VersionYanked] "first yanked version"
            , Admit
            , deny "DenyIfDeprecated" [VersionDeprecated] "deprecated version"
            , Admit
            , deny "DenyIfYanked" [VersionYanked] "second yanked version"
            , Admit
            ]
            `shouldBe` "DenyIfDeprecated: the versions are deprecated (rejection entries: 1). DenyIfYanked: the versions are yanked (rejection entries: 2)"

    it "keeps each integrity category separate from policy denials in a deterministic order" $
        summarisedRejections [Reject (Rejection BelowIntegrityFloor "sha1"), deny "custom" [] "custom detail", Reject (Rejection MissingIntegrity "1.0.0 lacks digest"), Reject (Rejection MissingIntegrity "2.0.0 lacks digest")]
            `shouldBe` "custom: the policy rule denied admission (rejection entries: 1). MissingIntegrity: artifacts lack an integrity digest (rejection entries: 2). BelowIntegrityFloor: artifact digests fall below the configured integrity floor (rejection entries: 1)"

    it "does not evaluate per-version policy reasons" $
        summarisedRejections [deny "BlockedByDefault" [impureThrow DiagnosticRead] "full diagnostic"]
            `shouldBe` "BlockedByDefault: no allow rule matched (rejection entries: 1)"

    it "finishes the decision list before the returned text reaches weak head normal form" $
        evaluate (summarisedRejections (deny "BlockedByDefault" [] "detail" : impureThrow DiagnosticRead))
            `shouldThrow` (== DiagnosticRead)

    for_ [("empty", []), ("admitted-only", [Admit]), ("repeated admissions", [Admit, Admit])] $ \(label, decisions) ->
        it ("keeps the exact fallback text for " <> label <> " inputs") $ do
            summarisedRejections decisions `shouldBe` "no versions are available for this package"
            detailedRejections decisions `shouldBe` "no versions are available for this package"

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
