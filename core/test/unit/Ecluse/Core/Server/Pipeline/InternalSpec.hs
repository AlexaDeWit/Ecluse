-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Pipeline.InternalSpec (spec) where

import Data.Aeson (Object, Value (String), decodeStrict, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime, nominalDay)
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Katip (toObject)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve.Types (DbEtag (..))
import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg (SHA1, SHA256),
    PackageDetails (..),
    PackageInfo (..),
    mkScope,
 )
import Ecluse.Core.Package.Integrity (VersionIntegrity (MeetsFloor), classifyDigests)
import Ecluse.Core.Rules (
    PreparedRule,
    Resilience (Resilience),
    prepare,
 )
import Ecluse.Core.Rules.Effectful (defaultEffectfulConfig, newBreaker)
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss, Epss),
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    FailureAlignment (FailDeny),
    Inability (EvaluationFailed, NoDatabaseLoaded, RuleThrew),
    LimitShortfall (RemainingUnscored),
    Reason (AffectedBy, FixStillApplies, FixesButStillAffected, FixesNoAdvisory, KnownMalicious, PublishedTooRecently, Remediates, RemediatesWithinLimits, RemediationBeyondLimits, RuleUnable, RunsOnInstall, ScopeNotAllowListed),
    Rule (AllowIfOlderThan),
    RuleEvidence (evName, evVersion),
    RuleVerdict (NoDecision),
    ScoreComparison (ScoreComparison),
    ScoreReading (Scored, Unscored),
    ScoreSides (ScoreSides),
    SkippedCheck (SkippedUnavailable, Unreached),
    completeEvidence,
    defaultDifferentialLimits,
    mkAdvisoryIds,
    mkLimitShortfalls,
 )
import Ecluse.Core.Server.Pipeline.Internal (
    DenialAudit (..),
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
    Rejection (Rejection, rejectionMessage),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    serveDecisionOf,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRuleEffectfulFailure))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Log (runJsonLog)
import Ecluse.Test.Package (
    artifactsWith,
    defaultMinIntegrity,
    detailsWith,
    leftpadName,
    npmVersion,
    pypiVersion,
    requestsName,
    unsafeHash,
    unscopedNpm,
    validSha1,
    validSha256,
 )
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps, packageRule)
import Ecluse.Test.Server.Response (reasonOf, rejectionOf)

spec :: Spec
spec = do
    -- Asserting every branch directly pins the bounded label set the metric catalogue records.
    -- PipelineSpec exercises the call sites.
    describe "packumentServeDecision (no-survivors -> decision)" $ do
        it "an admit in the set is an admit" $
            packumentServeDecision [Admit] `shouldBe` Metric.Admit
        it "an all-policy-denial set is a deny" $
            packumentServeDecision [Reject (Rejection (ByPolicy (RuleName "min-age") []) "denied")]
                `shouldBe` Metric.Deny
        it "a transient-outage set is an unavailability" $
            packumentServeDecision [Reject (Rejection (Unavailable (WillResolve Nothing)) "down")]
                `shouldBe` Metric.Unavailable

    describe "serveDecisionClass (artifact-path decision)" $ do
        it "maps an admit to admit" $
            serveDecisionClass Admit `shouldBe` Metric.Admit
        it "maps a policy or integrity refusal to deny" $ do
            serveDecisionClass (Reject (Rejection (ByPolicy (RuleName "r") []) "m")) `shouldBe` Metric.Deny
            serveDecisionClass (Reject (Rejection MissingIntegrity "m")) `shouldBe` Metric.Deny
            serveDecisionClass (Reject (Rejection BelowIntegrityFloor "m")) `shouldBe` Metric.Deny
        it "maps an upstream outage or invalid response to unavailability" $ do
            serveDecisionClass (Reject (Rejection (Unavailable (WillResolve Nothing)) "m")) `shouldBe` Metric.Unavailable
            serveDecisionClass (Reject (Rejection UpstreamInvalid "m")) `shouldBe` Metric.Unavailable

    describe "denialLabels (rule-denial labels)" $ do
        it "carries the deciding rule name only for a policy denial" $ do
            denialLabels (ByPolicy (RuleName "min-age") [ScopeNotAllowListed (mkScope "myorg")]) `shouldBe` (Just "min-age", Metric.ReasonPolicy)
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
            let resilience = Resilience defaultEffectfulConfig breaker noBreakerReporter getCurrentTime
            evalTier [packageRule "EffRule" 300 FailDeny (Just resilience) pass (NoDecision FixesNoAdvisory)] `shouldBe` Metric.Effectful

    describe "remediation outcomes on the denial metrics" $ do
        let ids = mkAdvisoryIds ("GHSA-fixed-0001" :| ["GHSA-open-0002"])
            compared = ScoreComparison ids ids (ScoreSides (Scored 9.8) Unscored) (ScoreSides Unscored (Scored 0.005)) defaultDifferentialLimits
            refusal = Undecidable (WillResolve Nothing) (RuleUnable "AllowIfRemediatesCve" NoDatabaseLoaded)
            evidence = completeEvidence (detailsWith leftpadName (npmVersion "1.0.0") [])
            labelsOf decision = denialLabels <$> reasonOf (serveDecisionOf evidence decision)
        it "records no denial labels for a remediation admission" $
            labelsOf (Admitted "AllowIfRemediatesCve" (RemediatesWithinLimits compared) []) `shouldBe` Nothing
        it "labels every remediation refusal without an advisory identifier"
            $ for_
                [ BlockedByDefault [RemediationBeyondLimits compared (mkLimitShortfalls (RemainingUnscored Cvss ids :| [])), FixStillApplies ids, FixesButStillAffected ids ids]
                , refusal
                ]
            $ \decision -> case labelsOf decision of
                Nothing -> expectationFailure "expected a refusal"
                Just (rule, reasonClass) -> (maybeToList rule <> [show reasonClass]) `shouldSatisfy` all (\label -> not ("GHSA-" `T.isInfixOf` label))
        it "counts a fail-closed remediation refusal as unavailable and as an effectful failure" $ do
            labelsOf refusal `shouldBe` Just (Nothing, Metric.ReasonUnavailable)
            causes <- newIORef []
            recordEffectfulFailures noopMetricsPort{mpRuleEffectfulFailure = \cause -> modifyIORef' causes (cause :)} [refusal]
            readIORef causes `shouldReturn` [Metric.Connection]

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
                , Reject (Rejection (ByPolicy (RuleName "min-age") []) "denied")
                , Reject (Rejection (Unavailable (WillResolve Nothing)) "down")
                ]

    describe "recordEffectfulFailures" $
        it "records a failure per undecidable verdict, skipping decided ones" $
            recordEffectfulFailures
                noopMetricsPort
                [ Undecidable (WillResolve Nothing) (RuleUnable "DenyIfCve" EvaluationFailed)
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
            logged <- runJsonLog (logDenials (unscopedNpm "is-odd") Nothing [denied])
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""

    describe "logDenials (the advisories an audit line names)" $ do
        it "records the advisories a scored or a malicious denial names" $ do
            auditCve npmEvidence (Blocked "DenyIfCve" (Just (DbEtag "etag-1")) (AffectedBy Cvss 7.0 (mkAdvisoryIds ("CVE-2026-0001" :| ["GHSA-aaaa-bbbb-cccc"]))))
                `shouldReturn` [Just "CVE-2026-0001, GHSA-aaaa-bbbb-cccc"]
            auditCve pypiEvidence (Blocked "DenyIfKnownMalicious" (Just (DbEtag "etag-1")) (KnownMalicious (mkAdvisoryIds ("MAL-one" :| ["MAL-two"]))))
                `shouldReturn` [Just "MAL-one, MAL-two"]
        it "records no advisory where no reason names one, whatever the rendered text reads"
            $ for_
                [ Blocked "DenyInstallTimeExecution" Nothing (RunsOnInstall "affected by GHSA-text-0001 (CVSS >= 7.0)")
                , BlockedByDefault [ScopeNotAllowListed (mkScope "myorg"), quarantine]
                , Undecidable (WillResolve Nothing) (RuleUnable "DenyIfCve" (RuleThrew "affected by GHSA-text-0001 (CVSS >= 7.0)"))
                ]
            $ \decision -> auditCve npmEvidence decision `shouldReturn` [Nothing]
        for_ [("npm", npmEvidence, "leftpad@1.0.0"), ("PyPI", pypiEvidence, "requests@1.0.0")] $ \(ecosystem, evidence, subject) ->
            describe ("a default denial joining a remediation reason with the quarantine, for " <> ecosystem) $ do
                let partialFix = FixesButStillAffected (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) (mkAdvisoryIds ("GHSA-open-0002" :| []))
                    beyond = RemediationBeyondLimits partialComparison (mkLimitShortfalls (RemainingUnscored Epss (mkAdvisoryIds ("GHSA-open-0002" :| [])) :| []))
                    stillApplies = FixStillApplies (mkAdvisoryIds ("GHSA-open-0002" :| []))
                    denied reason = BlockedByDefault [reason, quarantine]
                it "records the advisories still affecting the version, never the one it fixes" $
                    for_ [partialFix, beyond, stillApplies] $ \reason ->
                        auditCve evidence (denied reason) `shouldReturn` [Just "GHSA-open-0002"]
                it "renders the message from its reasons and labels the metric by rule, with no advisory identifier" $ do
                    fmap rejectionMessage (rejectionOf (serveDecisionOf evidence (denied partialFix)))
                        `shouldBe` Just (subject <> " was denied (no rule allowed it): fixes GHSA-fixed-0001 with GHSA-open-0002 remaining; published only 1 day ago, minimum age is 7 days")
                    for_ [partialFix, beyond, stillApplies] $ \reason ->
                        denialLabels <$> reasonOf (serveDecisionOf evidence (denied reason))
                            `shouldBe` Just (Just "BlockedByDefault", Metric.ReasonPolicy)

    describe "logSkippedChecks" $ do
        it "records a check skipped for unavailability once, at WARNING, with the package, version, rule, and cause" $ do
            logged <-
                runJsonLog $
                    logSkippedChecks (unscopedNpm "is-odd") "1.0.0" (Just (DbEtag "etag-xyz")) [SkippedUnavailable "DenyIfCve" NoDatabaseLoaded, Unreached "DenyIfEpss"]
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
            logged <- runJsonLog (logSkippedChecks (unscopedNpm "is-odd") "1.0.0" Nothing [])
            logged `shouldBe` ""

    describe "denialAuditPayload" $ do
        let audit etag reasons =
                DenialAudit
                    { daPackage = unscopedNpm "left-pad"
                    , daVersion = "1.2.3"
                    , daRule = Just "DenyIfCve"
                    , daReasonClass = Metric.ReasonPolicy
                    , daAdvisoryEtag = etag
                    , daReasons = reasons
                    }

        it "names the version, deciding rule, and active advisory ETag" $ do
            let obj = toObject (denialAuditPayload (audit (Just (DbEtag "etag-xyz")) mempty))
            KeyMap.lookup "version" obj `shouldBe` Just (String "1.2.3")
            KeyMap.lookup "rule" obj `shouldBe` Just (String "DenyIfCve")
            KeyMap.lookup "active_advisory_db_etag" obj `shouldBe` Just (String "etag-xyz")

        it "omits the ETag field when no advisory database is active" $
            KeyMap.lookup "active_advisory_db_etag" (toObject (denialAuditPayload (audit Nothing mempty)))
                `shouldBe` Nothing

        it "names each advisory still affecting the version once, in reason order, and none it fixes" $
            KeyMap.lookup "cve" (toObject (denialAuditPayload (audit Nothing [Remediates (mkAdvisoryIds ("A-1" :| [])), FixesButStillAffected (mkAdvisoryIds ("A-1" :| [])) (mkAdvisoryIds ("B-2" :| [])), AffectedBy Cvss 7.0 (mkAdvisoryIds ("B-2" :| ["C-3"]))])))
                `shouldBe` Just (String "B-2, C-3")

        it "omits the advisory field when no reason names one" $
            KeyMap.lookup "cve" (toObject (denialAuditPayload (audit Nothing [quarantine]))) `shouldBe` Nothing

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

    describe "admitByIntegrity and the download gate's floor" $
        modifyMaxSuccess (const 500) $
            it "lists a file exactly when the gate's floor admits it, and refuses a version by its files' digests" $
                hedgehog $ do
                    info <- forAll genIntegrityInfo
                    let (admissible, refusals) = admitByIntegrity defaultMinIntegrity belowFloorMarker missingMarker info
                        listed = Map.mapMaybe (nonEmpty . NE.filter gateFloorAdmits . pkgArtifacts) (infoVersions info)
                        (digestless, weak) = Map.partition (all (null . artHashes) . pkgArtifacts) (infoVersions info `Map.difference` listed)
                    cover 10 "a version listed whole" (any (all gateFloorAdmits . pkgArtifacts) (infoVersions info))
                    cover 5 "a version that loses a file" (or (Map.intersectionWith (\details kept -> length kept < length (pkgArtifacts details)) (infoVersions info) listed))
                    cover 10 "a version refused below the floor" (not (null weak))
                    cover 10 "a version refused with no digest" (not (null digestless))
                    cover 20 "one file a version, the npm shape" (infoName info == leftpadName)
                    cover 20 "several files in a version, the PyPI shape" (infoName info == requestsName)
                    infoVersions admissible === Map.intersectionWith (\details kept -> details{pkgArtifacts = kept}) (infoVersions info) listed
                    refusals === (belowFloorMarker <$ Map.elems weak) <> (missingMarker <$ Map.elems digestless)
                    infoDistTags admissible === Map.filter ((`Map.member` listed) . renderVersion) (infoDistTags info)

-- | The @cve@ field of each audit line 'logDenials' writes for one version's decision.
auditCve :: RuleEvidence -> Decision -> IO [Maybe Text]
auditCve evidence decision = do
    logged <- runJsonLog (logDenials (evName evidence) Nothing [VersionVerdict (renderVersion (evVersion evidence)) (serveDecisionOf evidence decision)])
    pure [join (parseMaybe (\entry -> entry .: "data" >>= (.:? "cve")) line) | Just line <- map (decodeStrict @Object . encodeUtf8) (lines logged)]

npmEvidence, pypiEvidence :: RuleEvidence
npmEvidence = completeEvidence (detailsWith leftpadName (npmVersion "1.0.0") [])
pypiEvidence = completeEvidence (detailsWith requestsName (pypiVersion "1.0.0") [])

-- | The quarantine's abstention for a version one day old.
quarantine :: Reason
quarantine = PublishedTooRecently nominalDay (7 * nominalDay)

-- | A partial fix of GHSA-fixed-0001 with GHSA-open-0002 remaining, outside the shipped limits.
partialComparison :: ScoreComparison
partialComparison =
    ScoreComparison (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) (mkAdvisoryIds ("GHSA-open-0002" :| [])) (ScoreSides (Scored 9.8) (Scored 7.0)) (ScoreSides Unscored Unscored) defaultDifferentialLimits

-- | Three integrity classes interleaved by key, so the assertion pins the bucket order and not the key order.
mixedIntegrityInfo :: PackageInfo
mixedIntegrityInfo =
    PackageInfo
        { infoName = leftpadName
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

-- | The two refusals admitByIntegrity projects dropped versions to, distinct so the bucket order shows.
belowFloorMarker, missingMarker :: ServeDecision
belowFloorMarker = Reject (Rejection BelowIntegrityFloor "below the integrity floor")
missingMarker = Reject (Rejection MissingIntegrity "no integrity digest")

-- | 'leftpadName' at the version, with exactly the given digests. admitByIntegrity reads only the artifacts.
versionWith :: Text -> [Hash] -> PackageDetails
versionWith raw = detailsWith leftpadName (npmVersion raw)

-- Whether the download gate's floor admits one file: its test of the artifact a request selects.
gateFloorAdmits :: Artifact -> Bool
gateFloorAdmits artifact = classifyDigests defaultMinIntegrity (artHashes artifact) == MeetsFloor

-- | Up to four versions, npm with one file each and PyPI up to three, each digest strong, weak, both, or none.
genIntegrityInfo :: Gen PackageInfo
genIntegrityInfo = do
    (name, versionOf, fileCount) <- Gen.element [(leftpadName, npmVersion, Range.singleton 1), (requestsName, pypiVersion, Range.constant 1 3)]
    keys <- Gen.subsequence ["0.9.0", "1.0.0", "1.5.0", "2.0.0"]
    versions <- forM keys $ \key -> do
        digests <- Gen.nonEmpty fileCount (Gen.subsequence [unsafeHash SHA1 validSha1, unsafeHash SHA256 validSha256])
        pure (key, (detailsWith name (versionOf key) []){pkgArtifacts = artifactsWith digests})
    tagged <- Gen.subsequence keys
    pure
        PackageInfo
            { infoName = name
            , infoVersions = Map.fromList versions
            , infoDistTags = Map.fromList (zip ["latest", "next", "beta", "legacy"] (map versionOf tagged))
            , infoInvalidEntries = []
            }
