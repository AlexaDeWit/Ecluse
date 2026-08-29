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

import Ecluse.Composition.MemoryPlan.Bounds (
    anyMountMirrors,
    cacheBytesCap,
    cacheBytesFloor,
    cacheSharePercent,
    clamp,
    envelope,
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
    responseBytesCap,
    responseBytesFloor,
    runtimeReserveFloorBytes,
    runtimeReserveShareDiv,
 )
import Ecluse.Composition.MemoryPlan.Internal (
    OverridePins (opArtifact, opCache, opDepth, opRequest),
    PlanInputs (piCache, piCpuAdmission, piExplicitAdmission, piLimits, piPublishConfigured, piQueueDemand),
    TenantDemands (..),
 )
import Ecluse.Composition.MemoryPlan.Override (configuredPins)
import Ecluse.Config (CacheSettings (csMaxEntries), LimitsSettings (limMaxResponseBytes))
import Ecluse.Core.Server.MemoryModel (contractResidentBytes, mirrorJobEstimatedBytes, packumentOriginFanout)

-- | Every tenant's desired share over a heap ceiling h, before the shed ladder walks it.
tenantDemands :: PlanInputs -> Int -> TenantDemands
tenantDemands inputs h =
    TenantDemands
        { tdCeiling = h
        , tdReserve = reserve
        , tdFixedBuffers = fixedBufferBytes demand
        , tdPins = pins
        , tdCacheDesired = fromMaybe (clamp cacheBytesFloor cacheBytesCap (appHeap * cacheSharePercent `div` 100)) (opCache pins)
        , tdCacheEntriesExplicit = csMaxEntries (piCache inputs)
        , tdMaterialDesired = mdDesired material
        , tdMaterialMinimum = mdMinimum material
        , tdAdmissionDesired = mdAdmission material
        , tdPublishConfigured = piPublishConfigured inputs
        , tdPublishDesired = max requestFinal (appHeap * publishSharePercent `div` 100)
        , tdRequestFinal = requestFinal
        , tdRequestComputed = requestComputed
        , tdDepthDesired = fromMaybe (clamp queueDepthFloor queueDepthCap ((appHeap * queueSharePercent `div` 100) `div` mirrorJobEstimatedBytes)) (opDepth pins)
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
    material = materialDemand inputs appHeap
    requestComputed = clamp requestBytesFloor requestBytesCap (appHeap * publishSharePercent `div` 100)
    requestFinal = fromMaybe requestComputed (opRequest pins)
    -- The charged envelope is the cap times the envelope multiplier, so dividing the
    -- share back down keeps the mirror tenant a bounded share of the heap.
    artifactCapDesired =
        fromMaybe
            (min mirrorArtifactBytesCap ((appHeap * mirrorArtifactSharePercent `div` 100) `div` mirrorArtifactEnvelopeMultiplier))
            (opArtifact pins)

-- The material tenant's desired shape, and the minimum it can shed to.
data MaterialDemand = MaterialDemand
    { mdAdmission :: Int
    , mdDesired :: Int
    , mdMinimum :: Int
    }

-- Admission is bounded by the CPU capacity and by what the material share holds at the
-- floor response cap. The response cap is what that share affords at the admitted concurrency.
materialDemand :: PlanInputs -> Int -> MaterialDemand
materialDemand inputs appHeap =
    MaterialDemand
        { mdAdmission = admissionDesired
        , mdDesired = materialOf admissionDesired responseDesired
        , mdMinimum = materialOf (fromMaybe 1 explicitAdmission) (fromMaybe responseBytesFloor responseExplicit)
        }
  where
    explicitAdmission = piExplicitAdmission inputs
    responseExplicit = limMaxResponseBytes (piLimits inputs)
    shareBytes = appHeap * materialSharePercent `div` 100
    memBound = max 1 (shareBytes `div` envelope responseBytesFloor)
    admissionDesired = fromMaybe (max 1 (min (piCpuAdmission inputs) memBound)) explicitAdmission
    responseDesired =
        fromMaybe
            (clamp responseBytesFloor responseBytesCap (contractResidentBytes (shareBytes `div` max 1 (admissionDesired * packumentOriginFanout))))
            responseExplicit

materialOf :: Int -> Int -> Int
materialOf a r = a * envelope r
