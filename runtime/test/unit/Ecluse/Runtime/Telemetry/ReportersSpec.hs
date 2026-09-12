-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.ReportersSpec (spec) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Breaker (Breaker (Closed, Open), BreakerReporter (BreakerReporter))
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (currentToken), mkSecret)
import Ecluse.Core.Credential.Refresh (
    CredentialReporters (crRefreshReporter),
    RefreshConfig (rcClock, rcMint, rcReporters),
    RefreshReporter (onRefreshFailed, onRefreshSucceeded),
    defaultRefreshConfig,
    noCredentialReporters,
    refreshingProvider,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), CredentialResult (RefreshFailed, Refreshed), Label (LCredentialResult, LProvider), Provider (ProviderCodeArtifact, ProviderRegistry), metricAttributes)
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
                third = deferredRefreshReporter deferred RubyGems ProviderRegistry
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
            onRefreshSucceeded first Nothing
            points `shouldReturn` [expected ProviderCodeArtifact 70]
            onRefreshSucceeded third (expiry 90)
            observed <- points
            observed `shouldMatchList` [expected ProviderCodeArtifact 70, expected ProviderRegistry 40]
            onRefreshSucceeded second Nothing
            points `shouldReturn` [expected ProviderRegistry 40]
            onRefreshSucceeded third Nothing
            points `shouldReturn` []

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

    it "collects a real credential refresh without another token request" $
        withTestTelemetry $ \telemetry meterEnv -> do
            (clock, setClock) <- newTestClock anInstant
            deferred <- newDeferredMetrics clock
            newMetrics telemetry >>= installMetrics deferred
            token <- newIORef (pure (AuthToken (mkSecret "eager") (expiry 10)))
            let cfg =
                    defaultRefreshConfig
                        { rcClock = clock
                        , rcMint = join (readIORef token)
                        , rcReporters =
                            noCredentialReporters
                                { crRefreshReporter = deferredRefreshReporter deferred Npm ProviderCodeArtifact
                                }
                        }
                points = gaugePoints "ecluse.credential.token.ttl.seconds" meterEnv
                expected seconds = [(metricAttributes [LProvider ProviderCodeArtifact], seconds)]
            provider <- refreshingProvider cfg
            points `shouldReturn` []
            writeIORef token (pure (AuthToken (mkSecret "replacement") (expiry 100)))
            setClock (addUTCTime 20 anInstant)
            void (currentToken provider)
            points `shouldReturn` expected 80
            setClock (addUTCTime 30 anInstant)
            points `shouldReturn` expected 70
            writeIORef token (throwIO MintFailed)
            setClock (addUTCTime 101 anInstant)
            currentToken provider `shouldThrow` (== MintFailed)
            points `shouldReturn` expected 0
            setClock (addUTCTime 200 anInstant)
            points `shouldReturn` expected 0
            counts <- sumPoints "ecluse.credential.refresh" meterEnv
            counts
                `shouldMatchList` [ (metricAttributes [LProvider ProviderCodeArtifact, LCredentialResult Refreshed], 1)
                                  , (metricAttributes [LProvider ProviderCodeArtifact, LCredentialResult RefreshFailed], 1)
                                  ]

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
