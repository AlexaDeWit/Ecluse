module Ecluse.Server.Pipeline.InternalSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (
    ColorStrategy (ColorLog),
    Environment (Environment),
    LogEnv,
    Namespace (Namespace),
    Severity (DebugS),
    SimpleLogPayload,
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    permitItem,
    registerScribe,
 )
import Katip.Monadic (runKatipContextT)
import Katip.Scribes.Handle (jsonFormat, mkHandleScribeWithFormatter)
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg (SHA1, SHA256),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
 )
import Ecluse.Core.Package.Integrity (defaultMinIntegrity)
import Ecluse.Core.Rules (
    PreparedRule (..),
    Resilience (Resilience),
    defaultEffectfulConfig,
    newBreaker,
    noBreakerReporter,
    prepare,
 )
import Ecluse.Core.Rules.Types (
    Decision (BlockedByDefault, Undecidable),
    FailureAlignment (FailDeny),
    Rule (AllowIfOlderThan),
    RuleResult (NoDecision),
    atDefaultPrecedence,
 )
import Ecluse.Core.Server.Pipeline.Internal (
    admitByIntegrity,
    denialLabels,
    evalTier,
    logDecodeFailure,
    logNameMismatch,
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
import Ecluse.Test.Package (unsafeHash, validSha1, validSha256)
import Ecluse.Test.Port (noopMetricsPort)

spec :: Spec
spec = do
    describe "logDecodeFailure" $
        it "logs a WARNING tagged with this module and the package, naming the decode failure" $ do
            -- Drive the real JSONL stdout scribe and capture the line, so the
            -- structured `module` / `package` fields and the severity are asserted on
            -- the exact bytes an operator would see.
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logDecodeFailure (mkPackageName Npm Nothing "is-odd"))
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""
            logged `shouldSatisfy` T.isInfixOf "did not decode"

    describe "logNameMismatch" $
        it "logs a WARNING carrying both names and the origin when an upstream reports a different package" $ do
            -- The serve path drives this through the request's ambient katip context;
            -- here it is run against a real JSONL scribe so the warning's actual bytes --
            -- the requested name, the upstream's reported name, and the origin -- are
            -- pinned against what an operator reads. No span is active, so no @dd@ object
            -- is added: the dd-correlation that goes live on the serve path is the only
            -- delta to these lines.
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logNameMismatch (mkPackageName Npm Nothing "thing") "http://upstream.test" "other")
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""
            logged `shouldSatisfy` T.isInfixOf "\"upstreamName\":\"other\""
            logged `shouldSatisfy` T.isInfixOf "\"origin\":\"http://upstream.test\""
            logged `shouldSatisfy` T.isInfixOf "different package"

    -- The pure metric-label projections that classify a serve outcome into the bounded
    -- labels the catalogue records. Every branch is asserted directly, so the
    -- bounded-cardinality mapping is pinned independently of the serve path that drives
    -- it (the call sites are exercised in PipelineSpec).
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
            rules <- prepare [atDefaultPrecedence (AllowIfOlderThan 0)]
            evalTier rules `shouldBe` Metric.Structural
        it "is the effectful tier when any rule carries a resilience policy" $ do
            breaker <- newBreaker
            let effectful :: PreparedRule
                effectful =
                    PreparedRule
                        { prepName = "EffRule"
                        , prepPrecedence = 300
                        , prepResilience = Just (Resilience defaultEffectfulConfig FailDeny breaker noBreakerReporter)
                        , prepEval = \_ _ -> pure (NoDecision "noop")
                        }
            evalTier [effectful] `shouldBe` Metric.Effectful

    describe "transienceCause (effectful-failure cause)" $ do
        it "maps a retryable cause to a connection fault" $
            transienceCause (WillResolve Nothing) `shouldBe` Metric.Connection
        it "maps a permanent cause to the catch-all other" $
            transienceCause WontResolve `shouldBe` Metric.OtherCause

    -- The thin emit helpers that fold a serve outcome into the catalogue counters,
    -- driven against an inert metrics port: they exercise the per-decision branches
    -- (record vs skip) and the projection calls without a telemetry backend.
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

    -- The integrity-floor admission policy buckets the dropped versions into two refusal
    -- lists in a single pass over the class map. This pins the contract that pass must
    -- preserve: every below-floor refusal precedes every missing-integrity refusal,
    -- regardless of how the two classes interleave by version key.
    describe "admitByIntegrity (integrity-floor admission)" $
        it "buckets refusals below-floor before missing-integrity, keeping the floor-clearing versions" $ do
            let (admissible, refusals) =
                    admitByIntegrity defaultMinIntegrity belowFloorMarker missingMarker mixedIntegrityInfo
            -- Only the SHA-256 version clears the default floor; the SHA-1 and digestless
            -- versions are dropped from the served listing.
            Map.keys (infoVersions admissible) `shouldBe` ["1.5.0"]
            -- Two below-floor (SHA-1) versions, then two missing-integrity (no digest)
            -- versions -- the bucket order the fold must hold, not the key order.
            refusals `shouldBe` [belowFloorMarker, belowFloorMarker, missingMarker, missingMarker]

{- | A packument whose versions interleave the three integrity classes by key: two clear
the floor only with SHA-1 (below floor), two carry no digest at all (missing), and one
carries a SHA-256 digest (admissible). The keys are arranged so the two refused classes
alternate in ascending order, so the assertion pins the /bucket/ order (below floor before
missing) rather than incidentally tracking the key order.
-}
mixedIntegrityInfo :: PackageInfo
mixedIntegrityInfo =
    PackageInfo
        { infoName = mixedPkg
        , infoVersions =
            Map.fromList
                [ ("0.9.0", detailsWith "0.9.0" []) -- missing integrity
                , ("1.0.0", detailsWith "1.0.0" [unsafeHash SHA1 validSha1]) -- below floor
                , ("1.5.0", detailsWith "1.5.0" [unsafeHash SHA256 validSha256]) -- admissible
                , ("2.0.0", detailsWith "2.0.0" [unsafeHash SHA1 validSha1]) -- below floor
                , ("2.5.0", detailsWith "2.5.0" []) -- missing integrity
                ]
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

-- | The package the admission fixture is built around; its identity is inert to the gate.
mixedPkg :: PackageName
mixedPkg = mkPackageName Npm Nothing "leftpad"

{- | The two context-worded refusals admitByIntegrity projects the dropped versions to;
kept distinct so the bucket order is observable in the refusal list.
-}
belowFloorMarker, missingMarker :: ServeDecision
belowFloorMarker = Reject (Rejection BelowIntegrityFloor "below the integrity floor")
missingMarker = Reject (Rejection MissingIntegrity "no integrity digest")

{- | A per-version snapshot carrying exactly the given integrity digests; every other field
is an inert default, since admitByIntegrity reads only the version's artifacts.
-}
detailsWith :: Text -> [Hash] -> PackageDetails
detailsWith raw hashes =
    PackageDetails
        { pkgName = mixedPkg
        , pkgVersion = mkVersion Npm raw
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = artifactWith hashes :| []
        , pkgLicenses = []
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

-- | A single inert tarball carrying the given integrity digests and nothing else.
artifactWith :: [Hash] -> Artifact
artifactWith hashes =
    Artifact
        { artFilename = "leftpad.tgz"
        , artUrl = "https://example.test/leftpad.tgz"
        , artKind = Tarball
        , artHashes = hashes
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | Run an 'IO' action with 'stdout' redirected to a temporary file, returning
everything written -- so a scribe's output is assertable with no network. The original
'stdout' is restored on every exit path. (Mirrors the local helper in "Ecluse.LogSpec"
and "Ecluse.Server.PipelineSpec"; kept local to avoid exporting a test-only utility.)
-}
captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-pipeline-internal-log.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved

{- | A @katip@ 'LogEnv' with a single stdout scribe in the compact one-line JSON form,
built from @katip@ directly (the application's "Ecluse.Log".@newLogEnv@ is not on the
core side of the boundary). It reproduces that scribe -- colour off, every severity
admitted -- so a warning's serialised bytes are assertable here.
-}
jsonLogEnv :: IO LogEnv
jsonLogEnv = do
    scribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem DebugS) V2
    base <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    registerScribe "stdout" scribe defaultScribeSettings base
