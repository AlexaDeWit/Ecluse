-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Byte-weighted admission over the shared weighted controller.
module Ecluse.Core.Server.Admission.Bytes.Internal (
    ByteAdmission (..),
    newByteAdmission,
    withByteAdmission,

    -- * Internals exported for testing
    newByteAdmissionTuned,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
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

-- | Allocate byte capacity, clamped to one, with the shared wait budget timed on the deadlines.
newByteAdmission :: Deadlines -> Int -> IO ByteAdmission
newByteAdmission deadlines capacity = newByteAdmissionTuned deadlines capacity byteAdmissionWaiterRoom admissionWaitMicros

-- | Set the waiting-room bound and microsecond wait budget explicitly.
newByteAdmissionTuned :: Deadlines -> Int -> Int -> Int -> IO ByteAdmission
newByteAdmissionTuned deadlines capacity room waitMicros = do
    let cap = max 1 capacity
    core <- newWeightedAdmission deadlines cap room waitMicros
    pure ByteAdmission{baCore = core, baCapacity = cap}

-- | Hold a weight clamped to capacity. A full room or expired wait returns Nothing.
{-# INLINE withByteAdmission #-}
withByteAdmission :: (MonadUnliftIO m) => MetricsPort -> ByteAdmission -> Int -> m a -> m (Maybe a)
withByteAdmission metrics ba rawWeight =
    fmap rightToMaybe . withWeightedAdmission observers (baCore ba) weight
  where
    weight = min (baCapacity ba) (max 0 rawWeight)

    observers =
        AdmissionObservers
            { onQueued = pure ()
            , onShed = mpPublishBodyShed metrics
            , onInFlightDelta = mpPublishBodyInFlightBytes metrics
            }
