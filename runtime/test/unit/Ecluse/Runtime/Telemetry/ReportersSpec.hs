-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.ReportersSpec (spec) where

import Control.Concurrent (yield)
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Breaker (Breaker (Closed, Open), BreakerReporter (BreakerReporter))
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (currentToken), mkSecret, staticProvider)
import Ecluse.Core.Credential.Refresh (
    CredentialError (BreakerOpen),
    CredentialReporters (crBreakerReporter, crRefreshReporter),
    RefreshConfig (rcClock, rcMint, rcReporters),
    RefreshReporter (onRefreshFailed, onRefreshSucceeded),
    defaultRefreshConfig,
    noCredentialReporters,
    refreshingProvider,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), CredentialResult (RefreshFailed, Refreshed), Label (LCredentialResult, LProvider), Provider (ProviderCodeArtifact), metricAttributes)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Telemetry.Instruments (newMetrics)
import Ecluse.Runtime.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Runtime.Test.Telemetry (gaugePoints, sumPoints, withTestTelemetry)
import Ecluse.Test.Support (newTestClock)

spec :: Spec
spec = describe "credential expiry collection" $ do
    it "keeps the shortest expiry per provider until that credential is replaced" $
        withTestTelemetry $ \telemetry meterEnv -> do
            (clock, setClock) <- newTestClock anInstant
            deferred <- newDeferredMetrics clock
            m <- newMetrics telemetry
            installMetrics deferred m
            let first = deferredRefreshReporter deferred Npm ProviderCodeArtifact
                second = deferredRefreshReporter deferred PyPI ProviderCodeArtifact
                points = gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv
                expected provider seconds = (metricAttributes [LProvider provider], seconds)
            points `shouldReturn` []
            onRefreshSucceeded first (expiry 30)
            onRefreshSucceeded second (expiry 80)
            points `shouldReturn` [expected ProviderCodeArtifact 30]
            setClock (addUTCTime 40 anInstant)
            points `shouldReturn` [expected ProviderCodeArtifact 0]
            onRefreshSucceeded second (expiry 120)
            points `shouldReturn` [expected ProviderCodeArtifact 0]
            onRefreshSucceeded first (expiry 100)
            points `shouldReturn` [expected ProviderCodeArtifact 60]
            onRefreshFailed first (expiry 100)
            setClock (addUTCTime 50 anInstant)
            points `shouldReturn` [expected ProviderCodeArtifact 50]
            onRefreshFailed first Nothing
            points `shouldReturn` [expected ProviderCodeArtifact 50]

    it "observes no TTL for a static provider from startup" $
        withTestTelemetry $ \telemetry meterEnv -> do
            deferred <- newDeferredMetrics (pure anInstant)
            newMetrics telemetry >>= installMetrics deferred
            let token = AuthToken (mkSecret "static") Nothing
            currentToken (staticProvider token) `shouldReturn` token
            gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv `shouldReturn` []
            sumPoints "ecluse.credential.refresh" meterEnv `shouldReturn` []

    it "retains expiry observations received before instruments are installed" $
        withTestTelemetry $ \telemetry meterEnv -> do
            (clock, setClock) <- newTestClock anInstant
            deferred <- newDeferredMetrics clock
            onRefreshSucceeded (deferredRefreshReporter deferred Npm ProviderCodeArtifact) (expiry 60)
            setClock (addUTCTime 20 anInstant)
            newMetrics telemetry >>= installMetrics deferred
            gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv
                `shouldReturn` [(metricAttributes [LProvider ProviderCodeArtifact], 40)]
            sumPoints "ecluse.credential.refresh" meterEnv `shouldReturn` []

    it "collects current TTL across active breaker refusal without counting refused attempts" $
        withTestTelemetry $ \telemetry meterEnv -> do
            (clock, setClock) <- newTestClock anInstant
            deferred <- newDeferredMetrics clock
            newMetrics telemetry >>= installMetrics deferred
            let eager = AuthToken (mkSecret "eager") (expiry 100)
                replacement = AuthToken (mkSecret "replacement") (expiry 250)
            mint <- newIORef (pure eager)
            mintCalls <- newIORef (0 :: Int)
            failures <- newIORef (0 :: Int)
            latestBreaker <- newIORef Nothing
            let BreakerReporter reportBreaker = deferredBreakerReporter deferred CredentialMint
                reporter = deferredRefreshReporter deferred Npm ProviderCodeArtifact
                cfg =
                    defaultRefreshConfig
                        { rcClock = clock
                        , rcMint = modifyIORef' mintCalls (+ 1) >> join (readIORef mint)
                        , rcReporters =
                            noCredentialReporters
                                { crBreakerReporter = BreakerReporter $ \state -> do
                                    reportBreaker state
                                    writeIORef latestBreaker (Just state)
                                , crRefreshReporter =
                                    reporter
                                        { onRefreshFailed = \stamp -> do
                                            onRefreshFailed reporter stamp
                                            atomicModifyIORef' failures (\n -> (n + 1, ()))
                                        }
                                }
                        }
                points = gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv
                counts = sumPoints "ecluse.credential.refresh" meterEnv
                expected seconds = [(metricAttributes [LProvider ProviderCodeArtifact], seconds)]
                failedCounts = [(metricAttributes [LProvider ProviderCodeArtifact, LCredentialResult RefreshFailed], 5)]
            provider <- refreshingProvider cfg
            points `shouldReturn` []
            writeIORef mint (throwIO MintFailed)
            setClock (addUTCTime 90 anInstant)
            let demandUntilOpen = do
                    completed <- readIORef failures
                    when (completed < 5) $ do
                        currentToken provider `shouldReturn` eager
                        yield
                        demandUntilOpen
            timeout 2_000_000 demandUntilOpen `shouldReturn` Just ()
            readIORef latestBreaker `shouldReturn` Just (Open (addUTCTime 150 anInstant))
            points `shouldReturn` expected 10
            counts `shouldReturn` failedCounts
            setClock (addUTCTime 95 anInstant)
            currentToken provider `shouldReturn` eager
            points `shouldReturn` expected 5
            setClock (addUTCTime 101 anInstant)
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCalls `shouldReturn` 6
            readIORef latestBreaker `shouldReturn` Just (Open (addUTCTime 150 anInstant))
            points `shouldReturn` expected 0
            setClock (addUTCTime 120 anInstant)
            currentToken provider `shouldThrow` (== BreakerOpen)
            points `shouldReturn` expected 0
            counts `shouldReturn` failedCounts
            readIORef mintCalls `shouldReturn` 6
            writeIORef mint (pure replacement)
            setClock (addUTCTime 151 anInstant)
            currentToken provider `shouldReturn` replacement
            readIORef latestBreaker `shouldReturn` Just (Closed 0)
            readIORef mintCalls `shouldReturn` 7
            points `shouldReturn` expected 99
            observed <- counts
            observed
                `shouldMatchList` ((metricAttributes [LProvider ProviderCodeArtifact, LCredentialResult Refreshed], 1) : failedCounts)

    it "emits nothing with telemetry disabled before or after installation" $
        withTestTelemetry $ \_ meterEnv -> do
            deferred <- newDeferredMetrics (pure anInstant)
            let BreakerReporter reportBreaker = deferredBreakerReporter deferred CredentialMint
                refresh = deferredRefreshReporter deferred Npm ProviderCodeArtifact
            reportBreaker (Open anInstant)
            onRefreshSucceeded refresh (expiry 3600)
            onRefreshFailed refresh Nothing
            newMetrics telemetryDisabled >>= installMetrics deferred
            reportBreaker (Closed 0)
            onRefreshSucceeded refresh (expiry 3600)
            onRefreshFailed refresh (expiry 3600)
            gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv `shouldReturn` []

anInstant :: UTCTime
anInstant = UTCTime (fromGregorian 2026 9 12) 0

expiry :: Integer -> Maybe UTCTime
expiry seconds = Just (addUTCTime (fromInteger seconds) anInstant)

data MintFailed = MintFailed
    deriving stock (Eq, Show)

instance Exception MintFailed
