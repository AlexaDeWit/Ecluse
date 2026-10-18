-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A public control with a mutable clock and recorded HTTP outcomes.
module Ecluse.Test.Registry.Public (
    PublicFixture (..),
    newPublicFixture,
) where

import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian)

import Ecluse.Core.Registry.Backoff (CooldownEvent, newPublicBackoff, reportingBackoff)
import Ecluse.Core.Registry.Public (PublicControl, publicControlWith)
import Ecluse.Core.Security.Authority (HostPort)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome, PublicReadKind)

-- | The fixture advances only when its caller requests it.
data PublicFixture = PublicFixture
    { pfControl :: PublicControl
    , pfAdvance :: NominalDiffTime -> IO ()
    , pfRequests :: IO [(Text, PublicReadKind, ExchangeOutcome)]
    , pfTransitions :: IO [(HostPort, CooldownEvent)]
    }

-- | Use fixed policy bounds and the top of each jitter interval for repeatable deadlines.
newPublicFixture :: IO PublicFixture
newPublicFixture = do
    clock <- newIORef (UTCTime (fromGregorian 2026 10 18) 0)
    requests <- newIORef []
    transitions <- newIORef []
    backoff <- newPublicBackoff (readIORef clock) (\_ ceilingDelay -> pure ceilingDelay)
    let append target value = atomicModifyIORef' target (\values -> (value : values, ()))
        report authority event = append transitions (authority, event)
        record host kind outcome = append requests (host, kind, outcome)
    pure
        PublicFixture
            { pfControl = publicControlWith (reportingBackoff report backoff) (readIORef clock) record
            , pfAdvance = modifyIORef' clock . addUTCTime
            , pfRequests = reverse <$> readIORef requests
            , pfTransitions = reverse <$> readIORef transitions
            }
