module Ecluse.Server.Pipeline.InternalSpec (spec) where

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

import Network.HTTP.Client (HttpException (InvalidUrlException))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Npm (ResponseBoundExceeded (ResponseBoundExceeded))
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
import Ecluse.Core.Security (LimitError (BodyTooLarge))
import Ecluse.Core.Server.Pipeline.Internal (
    PackumentNameMismatch (PackumentNameMismatch),
    PackumentUndecodable (PackumentUndecodable),
    denialLabels,
    evalTier,
    fetchCause,
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
import Ecluse.Test.Port (noopMetricsPort)

{- | A stand-in exception 'fetchCause' does not specifically classify, so the
catch-all @other@ arm is exercised with a typed throw rather than a restricted
@userError@.
-}
data OtherFetchFault = OtherFetchFault
    deriving stock (Show)

instance Exception OtherFetchFault

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
            -- here it is run against a real JSONL scribe so the warning's actual bytes —
            -- the requested name, the upstream's reported name, and the origin — are
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

    describe "PackumentNameMismatch" $
        it "has usable Eq/Show (the typed-throw contract)" $ do
            -- A distinct typed exception, caught by the origin fetcher and recovered via
            -- 'fromException'; its derived instances back the catch and any audit show.
            show PackumentNameMismatch `shouldBe` ("PackumentNameMismatch" :: Text)
            PackumentNameMismatch `shouldBe` PackumentNameMismatch

    -- The pure metric-label projections that classify a serve outcome into the bounded
    -- labels the catalogue records. Every branch is asserted directly, so the
    -- bounded-cardinality mapping is pinned independently of the serve path that drives
    -- it (the call sites are exercised in PipelineSpec).
    describe "fetchCause (upstream-fetch error class)" $ do
        it "classifies an undecodable or name-mismatched body as a decode fault" $ do
            fetchCause (toException PackumentUndecodable) `shouldBe` Metric.Decode
            fetchCause (toException PackumentNameMismatch) `shouldBe` Metric.Decode
        it "classifies a response-bound breach as the catch-all other" $
            fetchCause (toException (ResponseBoundExceeded (BodyTooLarge 1))) `shouldBe` Metric.OtherCause
        it "classifies a transport error as a connection fault" $
            fetchCause (toException (InvalidUrlException "http://x" "bad")) `shouldBe` Metric.Connection
        it "classifies anything else as the catch-all other" $
            fetchCause (toException OtherFetchFault) `shouldBe` Metric.OtherCause

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

{- | Run an 'IO' action with 'stdout' redirected to a temporary file, returning
everything written — so a scribe's output is assertable with no network. The original
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
core side of the boundary). It reproduces that scribe — colour off, every severity
admitted — so a warning's serialised bytes are assertable here.
-}
jsonLogEnv :: IO LogEnv
jsonLogEnv = do
    scribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem DebugS) V2
    base <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    registerScribe "stdout" scribe defaultScribeSettings base
