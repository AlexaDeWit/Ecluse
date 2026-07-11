module Ecluse.Telemetry.ReportersSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse.Core.Breaker (Breaker (Closed, HalfOpen, Open), BreakerReporter (BreakerReporter))
import Ecluse.Core.Credential.Refresh (RefreshReporter (onRefreshFailed, onRefreshSucceeded))
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), Provider (CodeArtifact))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Telemetry.Instruments (newMetrics)
import Ecluse.Runtime.Telemetry.Reporters (
    breakerStateOf,
    deferredBreakerReporter,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )

{- | Tests for the bridge from the providers' telemetry-agnostic reporters to the live
instruments, and for the deferral that lets a pre-telemetry provider record once the
substrate exists. The crux is that the bridge is __total and inert when telemetry is
off__: a record through it before installation does nothing, and after installation of
the no-op-meter instruments ('newMetrics' on a disabled handle) still discards every
measurement -- the property the boot-time providers rely on to record unconditionally.
The state projection is checked directly. No SDK is initialised (that is the integration
tier), so these run pure of any exporter.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Reporters" $ do
    describe "breakerStateOf" $
        it "projects the runtime breaker onto the bounded gauge value" $ do
            breakerStateOf (Closed 0) `shouldBe` Metric.Closed
            breakerStateOf (Closed 7) `shouldBe` Metric.Closed -- the failure tally is not observable
            breakerStateOf HalfOpen `shouldBe` Metric.HalfOpen
            breakerStateOf (Open anInstant) `shouldBe` Metric.Open

    describe "deferred reporters are inert when telemetry is off" $
        it "records nothing before installation, and nothing through the no-op meter after" $ do
            deferred <- newDeferredMetrics
            let BreakerReporter reportBreaker = deferredBreakerReporter deferred CredentialMint
                refresh = deferredRefreshReporter deferred CodeArtifact
            -- Uninstalled: every reporter is inert and total (no throw, no SDK).
            reportBreaker (Open anInstant)
            onRefreshSucceeded refresh (Just 3600)
            onRefreshFailed refresh Nothing
            -- Installed with the no-op-meter instruments (telemetry off): still inert.
            metrics <- newMetrics telemetryDisabled
            installMetrics deferred metrics
            reportBreaker (Open anInstant)
            reportBreaker (Closed 0)
            onRefreshSucceeded refresh (Just 3600)
            onRefreshFailed refresh (Just 0)
            pure () :: Expectation

-- | An arbitrary instant for the 'Open' breaker's cooldown deadline (its value is inert).
anInstant :: UTCTime
anInstant = UTCTime (fromGregorian 2026 6 26) 0
