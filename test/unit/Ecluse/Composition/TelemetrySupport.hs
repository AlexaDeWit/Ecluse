-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | In-process telemetry and advisory slots for role composition tests.
module Ecluse.Composition.TelemetrySupport (
    withRoleTelemetry,
    newAdvisoryHandles,
    advisoryAgePoints,
) where

import Katip (LogEnv, closeScribes)
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.MeterProvider (SdkMeterEnv)
import UnliftIO (bracket)

import Data.Time (getCurrentTime)

import Ecluse.Core.Cve.Slot (newCveSlot)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Rules.Freshness (MaxAdvisoryAge, maxAdvisoryAgeFor)
import Ecluse.Cve.Sync (CveSyncHandle (..))
import Ecluse.Runtime.Cve.Sync (SyncEnv (SyncEnv))
import Ecluse.Runtime.Telemetry (Telemetry)
import Ecluse.Runtime.Test.Cve (headOnlyFetch)
import Ecluse.Runtime.Test.Telemetry (gaugePoints, withTestTelemetry)
import Ecluse.Test.Log (newTestLogEnv)

-- | Keep the logger and both SDK providers alive until the role and its assertions finish.
withRoleTelemetry :: (LogEnv -> Telemetry -> SdkMeterEnv -> IO a) -> IO a
withRoleTelemetry use =
    withTestTelemetry $ \telemetry meterEnv ->
        bracket newTestLogEnv (void . closeScribes) $ \logEnv ->
            use logEnv telemetry meterEnv

-- | Ready slots whose fetch reports no published artifact, so role tasks cannot change their generation.
newAdvisoryHandles :: [Ecosystem] -> IO [(Ecosystem, CveSyncHandle)]
newAdvisoryHandles ecosystems = forM ecosystems $ \eco -> do
    slot <- newCveSlot
    ready <- newTVarIO True
    alarmed <- newTVarIO False
    let env = SyncEnv (headOnlyFetch (Right Nothing)) eco "unused.db" slot
    pure (eco, CveSyncHandle ready env derivedMaxAge getCurrentTime alarmed)

-- | The maximum push age a mount with no quarantine rule derives: the shipped floor.
derivedMaxAge :: MaxAdvisoryAge
derivedMaxAge = maxAdvisoryAgeFor Nothing []

-- | Collect the SDK's age observations without registering a callback in the test.
advisoryAgePoints :: SdkMeterEnv -> IO [(Attributes, Int64)]
advisoryAgePoints = gaugePoints "ecluse.advisory.database.age.seconds"
