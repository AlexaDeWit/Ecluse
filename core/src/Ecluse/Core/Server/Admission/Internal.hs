-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Unit-slot admission over the shared weighted controller, and the overflow queue that both
metadata gates' refusals share. The composition root builds both with the process's sweeper. A
runtime built without the queue refuses a full room at once.
-}
module Ecluse.Core.Server.Admission.Internal (
    ServeAdmission (..),
    newServeAdmission,
    withServeAdmission,
    serveAdmissionLine,
    serveAdmissionOverflow,

    -- * Internals exported for testing
    newServeAdmissionTuned,
    withOverflow,
) where

import Control.Exception (ErrorCall (ErrorCall))
import UnliftIO (MonadUnliftIO, throwIO)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission.Overflow (Line, Overflow, newOverflow)
import Ecluse.Core.Server.Admission.Types (Shed)
import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    weightedAdmissionLine,
    withWeightedAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Process-wide serve capacity and its bounded waiting room, and the overflow queue when it is on.
data ServeAdmission = ServeAdmission
    { saSlots :: WeightedAdmission
    , saOverflow :: Maybe Overflow
    }

{- | Allocate positive capacity, a waiting room of the same size, and an overflow queue for it. The
deadlines must be swept, as the composition root's are, or a queued request never times out.
-}

-- The configuration parser guarantees capacity > 0. This bounds check is defence in depth.
newServeAdmission :: Deadlines -> Int -> IO ServeAdmission
newServeAdmission deadlines capacity
    | capacity <= 0 = throwIO (ErrorCall "ServeAdmission capacity must be positive")
    | otherwise = do
        slots <- newServeAdmissionTuned deadlines capacity capacity admissionWaitMicros
        withOverflow slots <$> newOverflow deadlines capacity

-- | Set the waiting-room bound and microsecond wait budget explicitly, with no overflow queue.
newServeAdmissionTuned :: Deadlines -> Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned deadlines capacity room waitMicros = do
    slots <- newWeightedAdmission deadlines capacity room waitMicros
    pure ServeAdmission{saSlots = slots, saOverflow = Nothing}

-- | Put an overflow queue in front of the gates.
withOverflow :: ServeAdmission -> Overflow -> ServeAdmission
withOverflow admission overflow = admission{saOverflow = Just overflow}

-- | The line of requests the serve slots' full room turned away.
serveAdmissionLine :: ServeAdmission -> Line
serveAdmissionLine = weightedAdmissionLine . saSlots

-- | The overflow queue in front of the gates, when it is on.
serveAdmissionOverflow :: ServeAdmission -> Maybe Overflow
serveAdmissionOverflow = saOverflow

-- | Hold one slot for the action, or say why the gate turned the request away.
{-# INLINE withServeAdmission #-}
withServeAdmission :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Either Shed a)
withServeAdmission metrics admission =
    withWeightedAdmission observers (saSlots admission) 1
  where
    observers =
        AdmissionObservers
            { onQueued = mpServeAdmissionQueued metrics
            , onShed = pure ()
            , onInFlightDelta = mpServeAdmissionInFlight metrics
            }
