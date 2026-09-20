-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Reclaim tenant bytes in priority order while preserving explicit storage pins.
CPU capacity and metadata ingest limits stay independent of material shedding.
The nursery's capability adjustment remains separate from tenant accounting.
-}
module Ecluse.Composition.MemoryPlan.Shed (
    shedToFit,
    shedCapabilityCount,
    cacheEntryBound,
) where

import Data.Ord (clamp)

import Ecluse.Composition.MemoryPlan.Bounds (
    cacheEntriesCap,
    cacheEntriesFloor,
    cacheEntryExpectedBytes,
    mirrorArtifactEnvelopeMultiplier,
    queueCharge,
    queueDepthFloor,
 )
import Ecluse.Composition.MemoryPlan.Internal (
    OverridePins (opArtifact, opCache, opDepth),
    PlanInputs (piAllocAreaBytes, piCapabilities),
    ShedOutcomes (..),
    TenantDemands (..),
 )
import Ecluse.Core.Server.MemoryModel (mirrorJobEstimatedBytes)
import Ecluse.Rts (nurseryFittedCapabilities)

{- | Walk the shed ladder: every tenant at its desired share, then shed in step order until
the sum fits or every tenant hits its minimum. The residual is what shedding cannot reclaim.
-}
shedToFit :: TenantDemands -> ShedOutcomes
shedToFit d =
    ShedOutcomes
        { soMirrorShed = stepShed mirrorStep
        , soArtifactCapFinal = artifactCapFinal
        , soCacheShed = stepShed cacheStep
        , soCacheFinal = stepFinal cacheStep
        , soMaterialShed = stepShed materialStep
        , soMaterialFinal = stepFinal materialStep
        , soAdmissionFinal = tdAdmissionDesired d
        , soResponseFinal = tdResponseFinal d
        , soPublishShed = stepShed publishStep
        , soPublishFinal = stepFinal publishStep
        , soQueueShedBytes = qoShed queue
        , soDepthFinal = qoDepthFinal queue
        , soQueueTenantBytes = qoTenantBytes queue
        , soResidualOvershoot = qoResidual queue
        }
  where
    mirrorStep = shedMirrorStep d (max 0 (desiredTenantSum d - tdCeiling d))
    -- The surviving cap after shedding: an explicit cap stands, and a computed one is
    -- the shed charge divided back down by the envelope multiplier.
    artifactCapFinal = case opArtifact (tdPins d) of
        Just n -> n
        Nothing -> stepFinal mirrorStep `div` mirrorArtifactEnvelopeMultiplier
    cacheStep = shedCacheStep d (stepResidual mirrorStep)
    materialStep = shedMaterialStep d (stepResidual cacheStep)
    publishStep = shedPublishStep d (stepResidual materialStep)
    queue = shedQueueStep d (stepResidual publishStep)

-- Every tenant at its desired share. What this overshoots is what the ladder must reclaim.
desiredTenantSum :: TenantDemands -> Int
desiredTenantSum d =
    tdReserve d
        + tdFixedBuffers d
        + tdMirrorChargeDesired d
        + tdCacheDesired d
        + tdMaterialDesired d
        + (if tdPublishConfigured d then tdPublishDesired d else 0)
        + queueCharge (tdMemoryBacked d) (tdDepthDesired d)

-- One shed-ladder step: give up as much of a tenant's reclaimable bytes as the residual
-- overshoot demands. 'stepResidual' is the overshoot the next step inherits.
data ShedStep = ShedStep
    { stepShed :: Int
    , stepFinal :: Int
    , stepResidual :: Int
    }

shedStep :: Int -> Int -> Int -> ShedStep
shedStep overshoot desired reclaimable =
    ShedStep{stepShed = shed, stepFinal = desired - shed, stepResidual = overshoot - shed}
  where
    shed = min overshoot reclaimable

-- Step 0: the mirror-artifact cap gives way first, to zero if needed. The plan surrenders
-- the background back-fill leg before the serve hot path.
shedMirrorStep :: TenantDemands -> Int -> ShedStep
shedMirrorStep d overshoot =
    shedStep overshoot desired (if isJust (opArtifact (tdPins d)) then 0 else desired)
  where
    desired = tdMirrorChargeDesired d

-- Step 1: the cache gives way next, to zero if needed (never an explicit one).
shedCacheStep :: TenantDemands -> Int -> ShedStep
shedCacheStep d overshoot =
    shedStep overshoot desired (if isJust (opCache (tdPins d)) then 0 else desired)
  where
    desired = tdCacheDesired d

-- Step 2: reclaim material estimates without changing CPU or metadata ingest controls.
shedMaterialStep :: TenantDemands -> Int -> ShedStep
shedMaterialStep d overshoot =
    shedStep overshoot (tdMaterialDesired d) (max 0 (tdMaterialDesired d - 1))

-- Step 3: the publish aggregate shrinks to one maximum request.
shedPublishStep :: TenantDemands -> Int -> ShedStep
shedPublishStep d overshoot =
    shedStep overshoot desired (if tdPublishConfigured d then max 0 (desired - tdRequestFinal d) else 0)
  where
    desired = tdPublishDesired d

-- The queue tenant after shedding: bytes shed, the depth cap (in jobs), and the bytes
-- that depth charges.
data QueueOutcome = QueueOutcome
    { qoShed :: Int
    , qoDepthFinal :: Int
    , qoTenantBytes :: Int
    , qoResidual :: Int
    }

-- Step 4: the memory-queue depth to its floor (never an explicit one).
shedQueueStep :: TenantDemands -> Int -> QueueOutcome
shedQueueStep d overshoot =
    QueueOutcome
        { qoShed = queueShedBytes
        , qoDepthFinal = depthFinal
        , qoTenantBytes = charge depthFinal
        , qoResidual = overshoot - queueShedBytes
        }
  where
    charge = queueCharge (tdMemoryBacked d)
    depthDesired = tdDepthDesired d
    depthExplicit = opDepth (tdPins d)
    depthReclaimableBytes = case depthExplicit of
        Just _ -> 0
        Nothing -> max 0 (charge depthDesired - charge queueDepthFloor)
    queueShedBytes = min overshoot depthReclaimableBytes
    depthFinal = case depthExplicit of
        Just n -> n
        Nothing
            | queueShedBytes > 0 -> max queueDepthFloor ((charge depthDesired - queueShedBytes) `div` mirrorJobEstimatedBytes)
            | otherwise -> depthDesired

{- | The cache entry bound: an explicit count, or the surviving aggregate divided by the
planning allowance per shared local metadata entry slot.
-}
cacheEntryBound :: TenantDemands -> ShedOutcomes -> Int
cacheEntryBound d o =
    fromMaybe (clamp (cacheEntriesFloor, cacheEntriesCap) (soCacheFinal o `div` cacheEntryExpectedBytes)) (tdCacheEntriesExplicit d)

{- | Where the nursery (capabilities x allocation area) exceeds a bounded share of the
ceiling, shed the capability count so it fits. 'Nothing' keeps the live count.
-}
shedCapabilityCount :: PlanInputs -> Int -> Maybe Int
shedCapabilityCount inputs h
    | fitted < piCapabilities inputs = Just fitted
    | otherwise = Nothing
  where
    fitted = nurseryFittedCapabilities h (piAllocAreaBytes inputs)
