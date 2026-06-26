module Ecluse.Telemetry.InstrumentsSpec (spec) where

import Test.Hspec

import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Telemetry.Instruments (
    newMetrics,
    recordBreakerState,
    recordCacheEntries,
    recordCacheRequest,
    recordCredentialRefresh,
    recordCredentialTokenTtl,
    recordMirrorEnqueueFailure,
    recordMirrorEnqueued,
    recordMirrorJobProcessed,
    recordMirrorPublishDuration,
    recordRuleDenial,
    recordRuleEffectfulFailure,
    recordRuleEvalDuration,
    recordServeDecision,
    recordUpstreamFetch,
    recordUpstreamFetchError,
    timedSeconds,
 )
import Ecluse.Telemetry.Metrics (
    BreakerSource (CredentialMint, EffectfulRule),
    BreakerState (Closed, HalfOpen, Open),
    CacheResult (Hit, Miss),
    Cause (Connection, Decode, Timeout),
    CredentialResult (RefreshFailed, Refreshed),
    Decision (Admit, Deny, Unavailable),
    MirrorResult (Failed, Published),
    Provider (Adc, CodeArtifact, Static),
    ReasonClass (ReasonMissingIntegrity, ReasonPolicy),
    StatusClass (Status2xx, Status5xx),
    Tier (Effectful, Structural),
    Upstream (Private, Public),
 )

{- | Tests for the runtime instrument layer. With telemetry off, 'newMetrics' builds
against the SDK's no-op meter, so the handle is total and every @record*@ helper is a
silently-discarded no-op. The crux these prove is that the emit surface is __inert when
telemetry is off__: every signal can be recorded without an SDK, without a network, and
without throwing — the property the hot path relies on to instrument unconditionally.
The timing helper is exercised too. No SDK is initialised here (that is the integration
tier), so these run pure of any exporter.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Instruments (inert when telemetry is off)" $ do
    it "builds the instrument handle against the no-op meter when telemetry is disabled" $ do
        _ <- newMetrics telemetryDisabled
        pure () :: Expectation

    it "records every catalogue signal as an inert no-op without throwing" $ do
        m <- newMetrics telemetryDisabled
        -- One representative call per instrument, spanning the bounded label domains,
        -- so the whole emit surface is exercised. None must throw or block.
        traverse_ (recordServeDecision m) [Admit, Deny, Unavailable]
        recordRuleDenial m (Just "min-age") ReasonPolicy
        recordRuleDenial m Nothing ReasonMissingIntegrity
        traverse_ (recordRuleEvalDuration m Structural) [0, 0.5]
        recordRuleEvalDuration m Effectful 1.25
        traverse_ (recordRuleEffectfulFailure m) [Timeout, Connection, Decode]
        recordBreakerState m EffectfulRule Closed
        recordBreakerState m CredentialMint HalfOpen
        recordBreakerState m CredentialMint Open
        recordUpstreamFetch m Public Status2xx 0.04
        recordUpstreamFetch m Private Status5xx 0.5
        recordUpstreamFetchError m Public Connection
        traverse_ (recordCacheRequest m) [Hit, Miss]
        recordCacheEntries m 0
        recordCacheEntries m 1024
        recordMirrorEnqueued m
        recordMirrorEnqueueFailure m
        traverse_ (recordMirrorJobProcessed m) [Published, Failed]
        recordMirrorPublishDuration m 2.5
        recordCredentialRefresh m CodeArtifact Refreshed
        recordCredentialRefresh m Static RefreshFailed
        recordCredentialRefresh m Adc Refreshed
        recordCredentialTokenTtl m CodeArtifact 3600
        pure () :: Expectation

    it "times an action on the monotonic clock, never returning a negative duration" $ do
        (value, seconds) <- timedSeconds (pure (42 :: Int))
        value `shouldBe` 42
        seconds `shouldSatisfy` (>= 0)
