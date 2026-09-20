-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.InstrumentsSpec (spec) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Metric.Core (ObservableResult (ObservableResult))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
    AdvisorySyncResult (AdvisoryFetchFailed, AdvisoryNonePublished, AdvisoryRefused, AdvisorySwapped, AdvisoryUnchanged),
    BreakerSource (CredentialMint, EffectfulRule),
    BreakerState (Closed, HalfOpen, Open),
    CacheResult (Collapsed, Hit, Miss),
    CacheStore (AssembledStore, FullStore, VersionStore),
    Cause (Connection, Decode, Timeout),
    CredentialResult (RefreshFailed, Refreshed),
    Decision (Admit, Deny, Unavailable),
    Label (LCacheResult, LCacheStore, LEcosystem, LProvider),
    MirrorResult (Failed, Published),
    Provider (ProviderCodeArtifact, ProviderRegistry, ProviderVerdaccio),
    ReasonClass (ReasonMissingIntegrity, ReasonPolicy),
    StatusClass (Status2xx, Status5xx),
    Tier (Effectful, Structural),
    Upstream (Private, Public),
    metricAttributes,
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (acmpCompileAccepted, acmpCompileDropped, acmpCompileRun), MetricsPort (..), timedSeconds)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Telemetry.Instruments.Internal (
    advisoryCompileMetricsPortOf,
    metricsPortOf,
    newMetrics,
    recordAdvisoryCompileAccepted,
    recordAdvisoryCompileDropped,
    recordAdvisoryCompileRun,
    recordAdvisorySyncAttempt,
    recordAdvisorySyncDuration,
    recordBreakerState,
    recordCacheEntries,
    recordCacheRequest,
    recordCredentialRefresh,
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
    registerAdvisoryDatabaseAge,
    registerAdvisorySourceAge,
    registerCredentialTokenTtl,
    reportAdvisoryDatabaseAge,
    reportAdvisorySourceAge,
 )
import Ecluse.Runtime.Test.Telemetry (gaugePoints, sumPoints, withTestTelemetry)
import Ecluse.Test.Support (newTestClock)

{- | Tests that the instrument handle is inert when telemetry is off: every @record*@ helper
is total and silent against the no-op meter, so the hot path can instrument unconditionally.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Instruments (inert when telemetry is off)" $ do
    it "exports distinct store outcomes, refusals and warm-full selected reads" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            for_ [mpCacheRequest port, mpVersionCacheRequest port, mpAssembledCacheRequest port] $ \recordRequest ->
                traverse_ recordRequest [Hit, Miss, Collapsed]
            traverse_ (mpCacheRefused port) [FullStore, VersionStore, AssembledStore]
            mpVersionCacheFullHit port
            for_ ["ecluse.metadata_cache.requests", "ecluse.metadata_cache.version.requests", "ecluse.metadata_cache.assembled.requests"] $ \name -> do
                points <- sumPoints name meterEnv
                points `shouldMatchList` [(metricAttributes [LCacheResult result], 1) | result <- [Hit, Miss, Collapsed]]
            refused <- sumPoints "ecluse.metadata_cache.refused" meterEnv
            refused `shouldMatchList` [(metricAttributes [LCacheStore store], 1) | store <- [FullStore, VersionStore, AssembledStore]]
            sumPoints "ecluse.metadata_cache.version.full_hits" meterEnv `shouldReturn` [(metricAttributes [], 1)]

    it "collects a decreasing lifetime, floors fractional seconds and retains zero after expiry" $
        withTestTelemetry $ \telemetry meterEnv -> do
            let start = UTCTime (fromGregorian 2026 9 12) 0
            (clock, setClock) <- newTestClock start
            m <- newMetrics telemetry
            registerCredentialTokenTtl m clock (pure [(ProviderCodeArtifact, addUTCTime 10.9 start)])
            let points = gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv
                expected seconds = [(metricAttributes [LProvider ProviderCodeArtifact], seconds)]
            points `shouldReturn` expected 10
            setClock (addUTCTime 4 start)
            points `shouldReturn` expected 6
            setClock (addUTCTime 10.9 start)
            points `shouldReturn` expected 0
            setClock (addUTCTime 100 start)
            points `shouldReturn` expected 0

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
        traverse_ (recordCacheRequest m) [Hit, Miss, Collapsed]
        recordCacheEntries m 0
        recordCacheEntries m 1024
        recordMirrorEnqueued m
        recordMirrorEnqueueFailure m
        traverse_ (recordMirrorJobProcessed m) [Published, Failed]
        recordMirrorPublishDuration m 2.5
        recordCredentialRefresh m ProviderCodeArtifact Refreshed
        recordCredentialRefresh m ProviderRegistry RefreshFailed
        recordCredentialRefresh m ProviderVerdaccio Refreshed
        registerCredentialTokenTtl m getCurrentTime (pure [])
        traverse_
            (recordAdvisorySyncAttempt m Npm)
            [AdvisorySwapped, AdvisoryUnchanged, AdvisoryNonePublished, AdvisoryFetchFailed, AdvisoryRefused]
        recordAdvisorySyncDuration m Npm AdvisorySwapped 1.5
        recordAdvisorySyncDuration m PyPI AdvisoryFetchFailed 0
        recordAdvisoryCompileAccepted m Npm 12000
        traverse_ (\cause -> recordAdvisoryCompileDropped m Npm cause 3) [DropOversize, DropMalformed]
        traverse_ (recordAdvisoryCompileRun m Npm) [CompileCompleted, CompileAborted]
        stamp <- getMonotonicTime
        traverse_ (\eco -> registerAdvisoryDatabaseAge m eco (pure (Just stamp))) [Npm, PyPI]
        registerAdvisorySourceAge m Npm (pure Nothing)
        pure () :: Expectation

    it "reports the advisory database's age as whole seconds since its generation was installed" $ do
        reported <- newIORef []
        now <- getMonotonicTime
        -- An install stamp an hour and a half-second in the past. The sub-second gap between
        -- this reading and the callback's own floors away, so the answer is exactly 3600.
        reportAdvisoryDatabaseAge Npm (pure (Just (now - 3600.5))) (capture reported)
        readIORef reported `shouldReturn` [(3600, metricAttributes [LEcosystem Npm])]

    it "reports zero rather than a negative age for a stamp that is not in the past" $ do
        reported <- newIORef []
        now <- getMonotonicTime
        reportAdvisoryDatabaseAge PyPI (pure (Just (now + 60))) (capture reported)
        readIORef reported `shouldReturn` [(0, metricAttributes [LEcosystem PyPI])]

    it "reports nothing at all while no generation has ever been installed" $ do
        reported <- newIORef []
        reportAdvisoryDatabaseAge Npm (pure Nothing) (capture reported)
        readIORef reported `shouldReturn` []

    it "reports the advisory source's age as whole seconds since the artifact was published" $ do
        reported <- newIORef []
        now <- getCurrentTime
        reportAdvisorySourceAge Npm (pure (Just (addUTCTime (negate 3600) now))) (capture reported)
        readIORef reported `shouldReturn` [(3600, metricAttributes [LEcosystem Npm])]

    it "observes nothing when no artifact has been published yet" $ do
        reported <- newIORef []
        reportAdvisorySourceAge PyPI (pure Nothing) (capture reported)
        readIORef reported `shouldReturn` []

    it "reports zero rather than a negative source age for a publication time in the future" $ do
        reported <- newIORef []
        reportAdvisorySourceAge PyPI (pure (Just (UTCTime (fromGregorian 2999 1 1) 0))) (capture reported)
        readIORef reported `shouldReturn` [(0, metricAttributes [LEcosystem PyPI])]

    it "binds the compile port to one ecosystem, and stays total for a name outside the closed enum" $ do
        m <- newMetrics telemetryDisabled
        -- 'Nothing' is a compile of an ecosystem the enum does not carry, so the port has no
        -- bounded label to record under and every field must still be total.
        for_ [advisoryCompileMetricsPortOf m (Just Npm), advisoryCompileMetricsPortOf m Nothing] $ \port -> do
            acmpCompileAccepted port 10
            acmpCompileDropped port DropOversize 1
            acmpCompileRun port CompileCompleted
        pure () :: Expectation

    it "times an action on the monotonic clock, never returning a negative duration" $ do
        (value, seconds) <- timedSeconds (pure (42 :: Int))
        value `shouldBe` 42
        seconds `shouldSatisfy` (>= 0)

{- | A stand-in for the SDK's collection sink: it keeps every value the callback observes,
paired with the attributes it observed them under.
-}
capture :: IORef [(Int64, Attributes)] -> ObservableResult Int64
capture reported = ObservableResult (\value attrs -> modifyIORef' reported (<> [(value, attrs)]))
