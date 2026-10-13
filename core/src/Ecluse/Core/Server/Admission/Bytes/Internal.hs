-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Byte-weighted admission over the shared weighted controller.
module Ecluse.Core.Server.Admission.Bytes.Internal (
    ByteAdmission,
    newByteAdmission,
    withByteAdmission,

    -- * Internals exported for testing
    newByteAdmissionTuned,
    byteAdmissionWaiting,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    weightedAdmissionWaiting,
    withWeightedAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Process-wide byte capacity and bounded waiting-room state.
data ByteAdmission = ByteAdmission
    { baCore :: WeightedAdmission
    , baCapacity :: Int
    }

-- | Publishes use a small waiting room because each request can hold a large body.
byteAdmissionWaiterRoom :: Int
byteAdmissionWaiterRoom = 8

-- | Allocate byte capacity, clamped to one, with the shared wait budget.
newByteAdmission :: Int -> IO ByteAdmission
newByteAdmission capacity = newByteAdmissionTuned capacity byteAdmissionWaiterRoom admissionWaitMicros

-- | Set the waiting-room bound and microsecond wait budget explicitly.
newByteAdmissionTuned :: Int -> Int -> Int -> IO ByteAdmission
newByteAdmissionTuned capacity room waitMicros = do
    let cap = max 1 capacity
    core <- newWeightedAdmission cap room waitMicros
    pure ByteAdmission{baCore = core, baCapacity = cap}

-- | Hold a weight clamped to capacity. A full room or expired wait returns Nothing.
{-# INLINE withByteAdmission #-}
withByteAdmission :: (MonadUnliftIO m) => MetricsPort -> ByteAdmission -> Int -> m a -> m (Maybe a)
withByteAdmission metrics ba rawWeight =
    withWeightedAdmission observers (baCore ba) weight
  where
    weight = min (baCapacity ba) (max 0 rawWeight)

    observers =
        AdmissionObservers
            { onQueued = pure ()
            , onShed = mpPublishBodyShed metrics
            , onInFlightDelta = mpPublishBodyInFlightBytes metrics
            }

-- | Observe committed waiters for deterministic coordination.
byteAdmissionWaiting :: ByteAdmission -> STM Int
byteAdmissionWaiting ba = weightedAdmissionWaiting (baCore ba)
