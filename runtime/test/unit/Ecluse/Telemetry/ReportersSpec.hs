-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Telemetry.ReportersSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse.Core.Breaker (Breaker (Closed, Open), BreakerReporter (BreakerReporter))
import Ecluse.Core.Credential.Refresh (RefreshReporter (onRefreshFailed, onRefreshSucceeded))
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), Provider (CodeArtifact))
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Telemetry.Instruments (newMetrics)
import Ecluse.Runtime.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )

{- | Tests the reporter bridge and its deferral: a record before installation, and after it
against the no-op meter, is total and silent, so boot-time providers report unconditionally.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Reporters" $ do
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
