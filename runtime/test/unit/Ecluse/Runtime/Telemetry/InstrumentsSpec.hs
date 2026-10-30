-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Enabled metric observations and disabled telemetry contracts.
module Ecluse.Runtime.Telemetry.InstrumentsSpec (spec) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import OpenTelemetry.Attributes (Attributes, emptyAttributes)
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
    CacheStore (AssembledStore, DirectoryStore, FullStore, VersionStore),
    Cause (Connection, Decode, Timeout),
    CredentialResult (RefreshFailed, Refreshed),
    Decision (Admit, Deny, Unavailable),
    Label (LCacheResult, LCacheStore, LEcosystem, LProvider, LRequestKind, LRequestStatus, LServerAddress),
    MirrorResult (Failed, Published),
    Provider (ProviderCodeArtifact, ProviderRegistry, ProviderVerdaccio),
    ReasonClass (ReasonMissingIntegrity, ReasonPolicy),
    StatusClass (Status2xx, Status5xx),
    Tier (Effectful, Structural),
    Upstream (Private, Public),
    metricAttributes,
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (acmpCompileAccepted, acmpCompileDropped, acmpCompileRun), MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome (..), PublicReadKind (..))
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
    registerOverflowQueue,
    reportAdvisoryDatabaseAge,
    reportAdvisorySourceAge,
 )
import Ecluse.Runtime.Test.Telemetry (gaugePoints, sumPoints, withTestTelemetry)
import Ecluse.Test.Support (newTestClock)

-- | Check recorded values and labels, and the disabled no-op behaviour.
spec :: Spec
spec = describe "Ecluse.Telemetry.Instruments" $ do
    admissionMetricsSpec
    cacheGaugesSpec
    it "counts public attempts separately by host, kind, and exact outcome" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            let hosts = ["registry.npmjs.org", "files.pythonhosted.org"]
                kinds = [Listing, SelectedRead, Artifact, Companion, WorkerMetadata, WorkerArtifact]
                outcomes = [ResponseStatus 200, ResponseStatus 429, ResponseStatus 503, TransportFailure, Cancelled]
            for_ hosts $ \host -> for_ kinds $ \kind -> do
                traverse_ (mpUpstreamRequest port host kind) outcomes
                mpUpstreamRequest port host kind (ResponseStatus 429)
            points <- sumPoints "ecluse.upstream.requests" meterEnv
            points
                `shouldMatchList` [ (metricAttributes [LServerAddress host, LRequestKind kind, LRequestStatus outcome], if outcome == ResponseStatus 429 then 2 else 1)
                                  | host <- hosts
                                  , kind <- kinds
                                  , outcome <- outcomes
                                  ]

    it "exports distinct store outcomes and retention refusals" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            for_ [mpCacheRequest port, mpVersionCacheRequest port, mpAssembledCacheRequest port] $ \recordRequest ->
                traverse_ recordRequest [Hit, Miss, Collapsed]
            traverse_ (mpCacheRefused port) [FullStore, VersionStore, AssembledStore]
            for_ ["ecluse.metadata_cache.requests", "ecluse.metadata_cache.version.requests", "ecluse.metadata_cache.assembled.requests"] $ \name -> do
                points <- sumPoints name meterEnv
                points `shouldMatchList` [(metricAttributes [LCacheResult result], 1) | result <- [Hit, Miss, Collapsed]]
            refused <- sumPoints "ecluse.metadata_cache.refused" meterEnv
            refused `shouldMatchList` [(metricAttributes [LCacheStore store], 1) | store <- [FullStore, VersionStore, AssembledStore]]

    it "exports directory outcomes, refusals and resident bytes under store=directory" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            traverse_ (mpDirectoryCacheRequest port) [Hit, Miss, Collapsed]
            mpCacheRefused port DirectoryStore
            mpCacheResidentBytes port 1000
            mpDirectoryCacheResidentBytes port 4000
            requests <- sumPoints "ecluse.metadata_cache.requests" meterEnv
            requests `shouldMatchList` [(metricAttributes [LCacheResult result, LCacheStore DirectoryStore], 1) | result <- [Hit, Miss, Collapsed]]
            sumPoints "ecluse.metadata_cache.refused" meterEnv `shouldReturn` [(metricAttributes [LCacheStore DirectoryStore], 1)]
            resident <- gaugePoints "ecluse.metadata_cache.resident_bytes" meterEnv
            resident `shouldMatchList` [(emptyAttributes, 1000), (metricAttributes [LCacheStore DirectoryStore], 4000)]

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
        traverse_ (recordServeDecision m) [Admit, Deny, Unavailable]
        mpUpstreamRequest (metricsPortOf m) "registry.npmjs.org" Artifact (ResponseStatus 429)
        mpUpstreamRequest (metricsPortOf m) "registry.npmjs.org" Artifact TransportFailure
        mpUpstreamRequest (metricsPortOf m) "registry.npmjs.org" Artifact Cancelled
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

admissionMetricsSpec :: Spec
admissionMetricsSpec = describe "enabled admission observations" $ do
    it "collects current overflow depth and distinct cumulative outcomes through batching" $
        withTestTelemetry $ \telemetry meterEnv -> do
            metrics <- newMetrics telemetry
            waiting <- newIORef 3
            registerOverflowQueue metrics (readIORef waiting)
            let port = metricsPortOf metrics
                observe = do
                    gaugePoints "ecluse.serve.admission.overflow.waiting" meterEnv `shouldReturn` [(emptyAttributes, 3)]
                    sumPoints "ecluse.serve.admission.overflow.admitted" meterEnv `shouldReturn` [(emptyAttributes, 2)]
                    sumPoints "ecluse.serve.admission.overflow.shed" meterEnv `shouldReturn` [(emptyAttributes, 3)]
                    sumPoints "ecluse.serve.admission.overflow.full" meterEnv `shouldReturn` [(emptyAttributes, 4)]
            replicateM_ 2 (mpOverflowAdmitted port)
            replicateM_ 3 (mpOverflowShed port)
            replicateM_ 4 (mpOverflowFull port)
            observe
            observe
            writeIORef waiting 0
            mpOverflowAdmitted port
            gaugePoints "ecluse.serve.admission.overflow.waiting" meterEnv `shouldReturn` [(emptyAttributes, 0)]
            sumPoints "ecluse.serve.admission.overflow.admitted" meterEnv `shouldReturn` [(emptyAttributes, 3)]

    it "tracks outstanding parse and publish reservations until each release" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            mpServeAdmissionInFlight port 2
            mpPublishBodyInFlightBytes port 4096
            mpServeAdmissionInFlight port (-1)
            mpPublishBodyInFlightBytes port (-1024)
            sumPoints "ecluse.serve.admission.in_flight" meterEnv `shouldReturn` [(emptyAttributes, 1)]
            sumPoints "ecluse.publish.body.in_flight_bytes" meterEnv `shouldReturn` [(emptyAttributes, 3072)]
            mpServeAdmissionInFlight port (-1)
            mpPublishBodyInFlightBytes port (-3072)
            sumPoints "ecluse.serve.admission.in_flight" meterEnv `shouldReturn` [(emptyAttributes, 0)]
            sumPoints "ecluse.publish.body.in_flight_bytes" meterEnv `shouldReturn` [(emptyAttributes, 0)]

    it "keeps admission queueing and publish refusal counts distinct" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            replicateM_ 2 (mpServeAdmissionQueued port)
            replicateM_ 3 (mpPublishBodyShed port)
            sumPoints "ecluse.serve.admission.queued" meterEnv `shouldReturn` [(emptyAttributes, 2)]
            sumPoints "ecluse.publish.body.shed" meterEnv `shouldReturn` [(emptyAttributes, 3)]

    it "distinguishes waiting, refusal, pauses and overdraw at the memory gate" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            mpMemoryAdmissionQueued port
            replicateM_ 2 (mpMemoryAdmissionShed port)
            replicateM_ 3 (mpMemoryAdmissionPause port)
            replicateM_ 4 (mpMemoryAdmissionOverdraw port)
            sumPoints "ecluse.serve.admission.memory.queued" meterEnv `shouldReturn` [(emptyAttributes, 1)]
            sumPoints "ecluse.serve.admission.memory.shed" meterEnv `shouldReturn` [(emptyAttributes, 2)]
            sumPoints "ecluse.serve.admission.memory.pauses" meterEnv `shouldReturn` [(emptyAttributes, 3)]
            sumPoints "ecluse.serve.admission.memory.overdraws" meterEnv `shouldReturn` [(emptyAttributes, 4)]

cacheGaugesSpec :: Spec
cacheGaugesSpec =
    it "reports the latest occupancy and each store's resident bytes independently" $
        withTestTelemetry $ \telemetry meterEnv -> do
            port <- metricsPortOf <$> newMetrics telemetry
            mpCacheEntries port 9
            mpCacheResidentBytes port 1000
            mpVersionCacheResidentBytes port 2000
            mpAssembledCacheResidentBytes port 3000
            gaugePoints "ecluse.metadata_cache.entries" meterEnv `shouldReturn` [(emptyAttributes, 9)]
            gaugePoints "ecluse.metadata_cache.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 1000)]
            gaugePoints "ecluse.metadata_cache.version.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 2000)]
            gaugePoints "ecluse.metadata_cache.assembled.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 3000)]
            mpCacheEntries port 4
            mpCacheResidentBytes port 700
            mpVersionCacheResidentBytes port 800
            mpAssembledCacheResidentBytes port 900
            gaugePoints "ecluse.metadata_cache.entries" meterEnv `shouldReturn` [(emptyAttributes, 4)]
            gaugePoints "ecluse.metadata_cache.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 700)]
            gaugePoints "ecluse.metadata_cache.version.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 800)]
            gaugePoints "ecluse.metadata_cache.assembled.resident_bytes" meterEnv `shouldReturn` [(emptyAttributes, 900)]

-- Keep each callback observation with its attributes.
capture :: IORef [(Int64, Attributes)] -> ObservableResult Int64
capture reported = ObservableResult (\value attrs -> modifyIORef' reported (<> [(value, attrs)]))
