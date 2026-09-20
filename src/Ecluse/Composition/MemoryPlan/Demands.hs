-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Every tenant's desired share of a heap ceiling, resolved before the shed ladder in
"Ecluse.Composition.MemoryPlan.Shed" walks it. A configured value wins its own bound here,
so a pinned tenant enters the ladder at the operator's number and a computed one enters at
its share of the application heap, bracketed by the floors and caps in
"Ecluse.Composition.MemoryPlan.Bounds".
-}
module Ecluse.Composition.MemoryPlan.Demands (
    tenantDemands,
) where

import Data.Ord (clamp)

import Ecluse.Composition.MemoryPlan.Bounds (
    anyMountMirrors,
    cacheBytesCap,
    cacheBytesFloor,
    cacheSharePercent,
    fixedBufferBytes,
    materialSharePercent,
    memoryQueueCharged,
    mirrorArtifactBytesCap,
    mirrorArtifactEnvelopeMultiplier,
    mirrorArtifactSharePercent,
    publishSharePercent,
    queueDepthCap,
    queueDepthFloor,
    queueSharePercent,
    requestBytesCap,
    requestBytesFloor,
    responseBytesFallback,
    runtimeReserveFloorBytes,
    runtimeReserveShareDiv,
 )
import Ecluse.Composition.MemoryPlan.Internal (
    OverridePins (opArtifact, opCache, opDepth, opRequest),
    PlanInputs (piCache, piCpuAdmission, piLimits, piPublishConfigured, piQueueDemand),
    TenantDemands (..),
 )
import Ecluse.Composition.MemoryPlan.Override (configuredPins)
import Ecluse.Config (CacheSettings (csMaxEntries), LimitsSettings (limMaxResponseBytes))
import Ecluse.Core.Server.MemoryModel (mirrorJobEstimatedBytes)

-- | Every tenant's desired share over a heap ceiling h, before the shed ladder walks it.
tenantDemands :: PlanInputs -> Int -> TenantDemands
tenantDemands inputs h =
    TenantDemands
        { tdCeiling = h
        , tdReserve = reserve
        , tdFixedBuffers = fixedBufferBytes demand
        , tdPins = pins
        , tdCacheDesired = fromMaybe (clamp (cacheBytesFloor, cacheBytesCap) (appHeap * cacheSharePercent `div` 100)) (opCache pins)
        , tdCacheEntriesExplicit = csMaxEntries (piCache inputs)
        , tdMaterialDesired = max 1 (appHeap * materialSharePercent `div` 100)
        , tdAdmissionDesired = piCpuAdmission inputs
        , tdResponseFinal = fromMaybe responseBytesFallback (limMaxResponseBytes (piLimits inputs))
        , tdPublishConfigured = piPublishConfigured inputs
        , tdPublishDesired = max requestFinal (appHeap * publishSharePercent `div` 100)
        , tdRequestFinal = requestFinal
        , tdRequestComputed = requestComputed
        , tdDepthDesired = fromMaybe (clamp (queueDepthFloor, queueDepthCap) ((appHeap * queueSharePercent `div` 100) `div` mirrorJobEstimatedBytes)) (opDepth pins)
        , tdMemoryBacked = memoryQueueCharged demand
        , tdMirrors = anyMountMirrors demand
        , tdArtifactCapDesired = artifactCapDesired
        , tdMirrorChargeDesired = if anyMountMirrors demand then artifactCapDesired * mirrorArtifactEnvelopeMultiplier else 0
        }
  where
    demand = piQueueDemand inputs
    pins = configuredPins inputs
    reserve = max runtimeReserveFloorBytes (h `div` runtimeReserveShareDiv)
    appHeap = max 0 (h - reserve)
    requestComputed = clamp (requestBytesFloor, requestBytesCap) (appHeap * publishSharePercent `div` 100)
    requestFinal = fromMaybe requestComputed (opRequest pins)
    -- The charged envelope is the cap times the envelope multiplier, so dividing the
    -- share back down keeps the mirror tenant a bounded share of the heap.
    artifactCapDesired =
        fromMaybe
            (min mirrorArtifactBytesCap ((appHeap * mirrorArtifactSharePercent `div` 100) `div` mirrorArtifactEnvelopeMultiplier))
            (opArtifact pins)
