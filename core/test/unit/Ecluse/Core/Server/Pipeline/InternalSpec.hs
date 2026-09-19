-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Pipeline.InternalSpec (spec) where

import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Katip (SimpleLogPayload, closeScribes, toObject)
import Katip.Monadic (runKatipContextT)
import Test.Hspec

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve.Types (DbEtag (..))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Hash,
    HashAlg (SHA1, SHA256),
    PackageDetails,
    PackageInfo (..),
    PackageName,
 )
import Ecluse.Core.Rules (
    PreparedRule (..),
    Resilience (Resilience),
    noSourceReporter,
    prepare,
 )
import Ecluse.Core.Rules.Effectful (defaultEffectfulConfig, newBreaker)
import Ecluse.Core.Rules.Types (
    Decision (BlockedByDefault, Undecidable),
    FailureAlignment (FailDeny),
    Rule (AllowIfOlderThan),
    RuleVerdict (NoDecision),
    SkippedCheck (SkippedUnavailable, Unreached),
 )
import Ecluse.Core.Server.Pipeline.Internal (
    DenialAudit (..),
    Metadata (..),
    VersionVerdict (VersionVerdict),
    admitByIntegrity,
    denialAuditPayload,
    denialLabels,
    evalTier,
    logDenials,
    logSkippedChecks,
    packumentServeDecision,
    recordDenials,
    recordEffectfulFailures,
    serveDecisionClass,
    transienceCause,
 )
import Ecluse.Core.Server.Response (
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Log (captureStdout, jsonLogEnv)
import Ecluse.Test.Package (defaultMinIntegrity, detailsWith, unsafeHash, unscopedNpm, validSha1, validSha256)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)

spec :: Spec
spec = do
    -- Asserting every branch directly pins the bounded label set the metric catalogue records.
    -- PipelineSpec exercises the call sites.
    describe "packumentServeDecision (no-survivors -> decision)" $ do
        it "an admit in the set is an admit" $
            packumentServeDecision [Admit] `shouldBe` Metric.Admit
        it "an all-policy-denial set is a deny" $
            packumentServeDecision [Reject (Rejection (ByPolicy (RuleName "min-age")) "denied")]
                `shouldBe` Metric.Deny
        it "a transient-outage set is an unavailability" $
            packumentServeDecision [Reject (Rejection (Unavailable (WillResolve Nothing)) "down")]
                `shouldBe` Metric.Unavailable

    describe "serveDecisionClass (artifact-path decision)" $ do
        it "maps an admit to admit" $
            serveDecisionClass Admit `shouldBe` Metric.Admit
        it "maps a policy or integrity refusal to deny" $ do
            serveDecisionClass (Reject (Rejection (ByPolicy (RuleName "r")) "m")) `shouldBe` Metric.Deny
            serveDecisionClass (Reject (Rejection MissingIntegrity "m")) `shouldBe` Metric.Deny
            serveDecisionClass (Reject (Rejection BelowIntegrityFloor "m")) `shouldBe` Metric.Deny
        it "maps an upstream outage or invalid response to unavailability" $ do
            serveDecisionClass (Reject (Rejection (Unavailable (WillResolve Nothing)) "m")) `shouldBe` Metric.Unavailable
            serveDecisionClass (Reject (Rejection UpstreamInvalid "m")) `shouldBe` Metric.Unavailable

    describe "denialLabels (rule-denial labels)" $ do
        it "carries the deciding rule name only for a policy denial" $ do
            denialLabels (ByPolicy (RuleName "min-age")) `shouldBe` (Just "min-age", Metric.ReasonPolicy)
            denialLabels MissingIntegrity `shouldBe` (Nothing, Metric.ReasonMissingIntegrity)
            denialLabels BelowIntegrityFloor `shouldBe` (Nothing, Metric.ReasonMissingIntegrity)
            denialLabels (Unavailable (WillResolve Nothing)) `shouldBe` (Nothing, Metric.ReasonUnavailable)
            denialLabels UpstreamInvalid `shouldBe` (Nothing, Metric.ReasonUnavailable)

    describe "evalTier (rule-evaluation tier)" $ do
        it "is the structural tier for an empty rule set" $
            evalTier ([] :: [PreparedRule]) `shouldBe` Metric.Structural
        it "is the structural tier for a purely-pure rule set" $ do
            rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan 0)]
            evalTier rules `shouldBe` Metric.Structural
        it "is the effectful tier when any rule carries a resilience policy" $ do
            breaker <- newBreaker
            let effectful :: PreparedRule
                effectful =
                    PreparedRule
                        { prepName = "EffRule"
                        , prepPrecedence = 300
                        , prepResilience = Just (Resilience defaultEffectfulConfig FailDeny breaker noBreakerReporter getCurrentTime noSourceReporter)
                        , prepAdvisoryGate = Nothing
                        , prepEval = \_ _ -> pure (NoDecision "noop")
                        }
            evalTier [effectful] `shouldBe` Metric.Effectful

    describe "transienceCause (effectful-failure cause)" $ do
        it "maps a retryable cause to a connection fault" $
            transienceCause (WillResolve Nothing) `shouldBe` Metric.Connection
        it "maps a permanent cause to the catch-all other" $
            transienceCause WontResolve `shouldBe` Metric.OtherCause

    describe "recordDenials" $
        it "records a denial per reject and nothing for an admit" $
            recordDenials
                noopMetricsPort
                [ Admit
                , Reject (Rejection (ByPolicy (RuleName "min-age")) "denied")
                , Reject (Rejection (Unavailable (WillResolve Nothing)) "down")
                ]

    describe "recordEffectfulFailures" $
        it "records a failure per undecidable verdict, skipping decided ones" $
            recordEffectfulFailures
                noopMetricsPort
                [ Undecidable (WillResolve Nothing) "unreachable"
                , BlockedByDefault []
                ]

    describe "logDenials" $
        -- An open rule-source breaker fast-fails to this verdict, so it is the routine
        -- shape of a rule source being down rather than a fault nobody answered.
        it "keeps an absorbed rule-source outage below the level an operator pages on" $ do
            let denied =
                    VersionVerdict
                        "1.0.0"
                        (Reject (Rejection (Unavailable (WillResolve Nothing)) "DenyIfCve: the rule could not be evaluated"))
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logDenials (unscopedNpm "is-odd") Nothing [denied])
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""

    describe "logSkippedChecks" $ do
        it "records a check skipped for unavailability once, at WARNING, with the package, version, rule, and cause" $ do
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty $
                    logSkippedChecks (unscopedNpm "is-odd") "1.0.0" (Just (DbEtag "etag-xyz")) [SkippedUnavailable "DenyIfCve" "no advisory database loaded", Unreached "DenyIfEpss"]
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""
            logged `shouldSatisfy` T.isInfixOf "\"version\":\"1.0.0\""
            logged `shouldSatisfy` T.isInfixOf "\"rule\":\"DenyIfCve\""
            logged `shouldSatisfy` T.isInfixOf "\"cause\":\"no advisory database loaded\""
            logged `shouldSatisfy` T.isInfixOf "\"active_advisory_db_etag\":\"etag-xyz\""
            -- An unreached check is the operator's precedence choice, not degradation: no line.
            logged `shouldSatisfy` (not . T.isInfixOf "DenyIfEpss")
            length (filter (T.isInfixOf "skipped for unavailability") (lines logged)) `shouldBe` 1

        it "records nothing for an admission that skipped no check" $ do
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logSkippedChecks (unscopedNpm "is-odd") "1.0.0" Nothing [])
                void (closeScribes logEnv)
            logged `shouldBe` ""

    describe "denialAuditPayload" $ do
        let audit etag extra =
                DenialAudit
                    { daPackage = unscopedNpm "left-pad"
                    , daVersion = "1.2.3"
                    , daRule = Just "DenyIfCve"
                    , daReasonClass = Metric.ReasonPolicy
                    , daAdvisoryEtag = etag
                    , daExtra = extra
                    }

        it "names the version, deciding rule, and active advisory ETag" $ do
            let obj = toObject (denialAuditPayload (audit (Just (DbEtag "etag-xyz")) mempty))
            KeyMap.lookup "version" obj `shouldBe` Just (String "1.2.3")
            KeyMap.lookup "rule" obj `shouldBe` Just (String "DenyIfCve")
            KeyMap.lookup "active_advisory_db_etag" obj `shouldBe` Just (String "etag-xyz")

        it "omits the ETag field when no advisory database is active" $
            KeyMap.lookup "active_advisory_db_etag" (toObject (denialAuditPayload (audit Nothing mempty)))
                `shouldBe` Nothing

        it "folds the extension metadata bag into the payload" $
            KeyMap.lookup "cve" (toObject (denialAuditPayload (audit Nothing (Metadata (Map.fromList [("cve", "CVE-2024-1")])))))
                `shouldBe` Just (String "CVE-2024-1")

    -- Pin the refusal order the single pass must preserve: every below-floor refusal precedes
    -- every missing-integrity refusal, however the two classes interleave by version key.
    describe "admitByIntegrity (integrity-floor admission)" $
        it "buckets refusals below-floor before missing-integrity, keeping the floor-clearing versions" $ do
            let (admissible, refusals) =
                    admitByIntegrity defaultMinIntegrity belowFloorMarker missingMarker mixedIntegrityInfo
            -- Only the SHA-256 version clears the default floor. The SHA-1 and digestless
            -- versions drop out of the served listing.
            Map.keys (infoVersions admissible) `shouldBe` ["1.5.0"]
            -- Two below-floor (SHA-1) versions, then two missing-integrity (no digest)
            -- versions: the bucket order the fold must hold, not the key order.
            refusals `shouldBe` [belowFloorMarker, belowFloorMarker, missingMarker, missingMarker]

{- | A packument interleaving the three integrity classes by key: two below floor, two missing a
digest, one admissible. The refused classes alternate in ascending key order, so the assertion
pins the bucket order and not the key order.
-}
mixedIntegrityInfo :: PackageInfo
mixedIntegrityInfo =
    PackageInfo
        { infoName = mixedPkg
        , infoVersions =
            Map.fromList
                [ ("0.9.0", versionWith "0.9.0" []) -- missing integrity
                , ("1.0.0", versionWith "1.0.0" [unsafeHash SHA1 validSha1]) -- below floor
                , ("1.5.0", versionWith "1.5.0" [unsafeHash SHA256 validSha256]) -- admissible
                , ("2.0.0", versionWith "2.0.0" [unsafeHash SHA1 validSha1]) -- below floor
                , ("2.5.0", versionWith "2.5.0" []) -- missing integrity
                ]
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

-- | The package the admission fixture is built around. Its identity is inert to the gate.
mixedPkg :: PackageName
mixedPkg = unscopedNpm "leftpad"

{- | The two context-worded refusals admitByIntegrity projects the dropped versions to.
They stay distinct, so the bucket order is observable in the refusal list.
-}
belowFloorMarker, missingMarker :: ServeDecision
belowFloorMarker = Reject (Rejection BelowIntegrityFloor "below the integrity floor")
missingMarker = Reject (Rejection MissingIntegrity "no integrity digest")

{- | A snapshot of 'mixedPkg' at the given version carrying exactly the given digests.
Everything else is an inert default, since admitByIntegrity reads only the artifacts.
-}
versionWith :: Text -> [Hash] -> PackageDetails
versionWith raw = detailsWith mixedPkg (mkVersion Npm raw)
