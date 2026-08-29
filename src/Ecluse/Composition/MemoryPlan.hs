-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory plan: one solver that partitions the effective heap ceiling
("Ecluse.Rts") between named tenants whose sum stays within it. In allocation order they are the
runtime reserve, the fixed enqueue buffer, the cache aggregate, the material aggregate, the publish
aggregate, the memory-queue depth, and the mirror-artifact envelope, one 'MemoryPlan' field each.
The non-byte runtime sizings live in "Ecluse.Composition.Sizing". A configured value wins its own
bound, every bound has a shipped fallback for a pod with no ceiling datapoint, and every decision
returns a boot-log line. The shed ladder ("Ecluse.Composition.MemoryPlan.Shed") keeps a pod too small
for its tenants booting, and an explicit override ("Ecluse.Composition.MemoryPlan.Override") refuses.
-}
module Ecluse.Composition.MemoryPlan (
    -- * The plan and its tenants
    MemoryPlan (..),
    PublishTenant (..),
    MirrorArtifactTenant (..),
    QueueTenantDemand (..),
    queueTenantDemand,

    -- * Resolution
    resolveMemoryPlan,
    planCacheConfig,
    mirrorArtifactEnvelopeMultiplier,
    mirrorArtifactBytesCap,
) where

import Ecluse.Composition.MemoryPlan.Bounds (
    anyMountMirrors,
    cacheBytesFallback,
    cacheEntriesCap,
    cacheEntriesFloor,
    cacheEntryExpectedBytes,
    clamp,
    fixedBufferBytes,
    memoryQueueCharged,
    mirrorArtifactBytesCap,
    mirrorArtifactEnvelopeMultiplier,
    publishAggregateFallbackRequests,
    queueCharge,
    queueDepthFallback,
    requestBytesFallback,
    responseBytesFallback,
 )
import Ecluse.Composition.MemoryPlan.Demands (tenantDemands)
import Ecluse.Composition.MemoryPlan.Internal (OverridePins (..), PlanInputs (..), ShedOutcomes (..), TenantDemands (..))
import Ecluse.Composition.MemoryPlan.Override (configuredPins, overrideViolationsFor)
import Ecluse.Composition.MemoryPlan.Render (renderDegradations, renderPlanLines)
import Ecluse.Composition.MemoryPlan.Shed (cacheEntryBound, shedCapabilityCount, shedToFit)
import Ecluse.Composition.MemoryPlan.Types (
    MemoryPlan (..),
    MirrorArtifactTenant (..),
    PublishTenant (..),
    QueueTenantDemand (..),
    queueTenantDemand,
 )
import Ecluse.Composition.Sizing (resolveServeAdmission, resolveSized)
import Ecluse.Config (CacheSettings (..), LimitsSettings, QueueSettings)
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
import Ecluse.Rts (EffectiveRuntimePlan (erpAllocAreaBytes), effectiveCapabilities, effectiveHeapCeiling, provenanceClause)

{- | Resolve the memory plan and its boot lines. The caller selects the mirror-queue
backend first, since 'QueueTenantDemand' projects from that choice.
-}
resolveMemoryPlan ::
    CacheSettings ->
    LimitsSettings ->
    QueueSettings ->
    Maybe Int ->
    EffectiveRuntimePlan ->
    QueueTenantDemand ->
    Bool ->
    (MemoryPlan, [Text])
resolveMemoryPlan cacheSettings limitsSettings queueSettings explicitAdmission runtime queueDemand publishConfigured =
    maybe (fallbackPlan inputs) (solvedPlan inputs) heapCeiling
  where
    (heapCeiling, ceilingProvenance) = effectiveHeapCeiling runtime
    (capabilities, _) = effectiveCapabilities runtime
    -- The explicit serveMaxInFlight wins inside this one.
    (cpuAdmission, cpuAdmissionLine) = resolveServeAdmission explicitAdmission capabilities
    inputs =
        PlanInputs
            { piCache = cacheSettings
            , piLimits = limitsSettings
            , piQueue = queueSettings
            , piExplicitAdmission = explicitAdmission
            , piPublishConfigured = publishConfigured
            , piQueueDemand = queueDemand
            , piCapabilities = capabilities
            , piAllocAreaBytes = max 1 (erpAllocAreaBytes runtime)
            , piCpuAdmission = cpuAdmission
            , piCpuAdmissionLine = cpuAdmissionLine
            , piCeilingClause = provenanceClause ceilingProvenance
            }

-- The solved plan over a heap ceiling h. The arithmetic stays apart from the boot-log
-- prose, so the sum-within-ceiling invariant reads on its own.
solvedPlan :: PlanInputs -> Int -> (MemoryPlan, [Text])
solvedPlan inputs h =
    ( MemoryPlan
        { mpRuntimeReserveBytes = tdReserve demands
        , mpCacheAggregateBytes = soCacheFinal outcomes
        , mpCacheMaxEntries = cacheEntryBound demands outcomes
        , mpMaterialAggregateBytes = soMaterialFinal outcomes
        , mpMaxResponseBytes = soResponseFinal outcomes
        , mpMaxRequestBytes = tdRequestFinal demands
        , mpAdmissionCapacity = soAdmissionFinal outcomes
        , mpShedCapabilities = shedCaps
        , mpPublishTenant = publishTenantOf demands outcomes
        , mpMirrorArtifactTenant = mirrorArtifactTenantOf demands outcomes
        , mpQueueMemoryMaxDepth = soDepthFinal outcomes
        , mpQueueTenantBytes = soQueueTenantBytes outcomes
        , mpFixedBufferBytes = tdFixedBuffers demands
        , mpDegradations = renderDegradations inputs demands outcomes shedCaps
        , mpOverrideViolations = overrideViolationsFor demands outcomes
        }
    , renderPlanLines inputs demands outcomes
    )
  where
    demands = tenantDemands inputs h
    outcomes = shedToFit demands
    -- The nursery (capabilities x allocation area) lives outside the heap ceiling, so
    -- the tenant sum cannot see it. The capability count sheds on its own.
    shedCaps = shedCapabilityCount inputs h

{- No ceiling datapoint: the shipped fallback bounds and admission from the CPU alone.
Nothing bounds the sum, so there is no tenant arithmetic to check. -}
fallbackPlan :: PlanInputs -> (MemoryPlan, [Text])
fallbackPlan inputs =
    ( MemoryPlan
        { mpRuntimeReserveBytes = 0
        , mpCacheAggregateBytes = cacheBytes
        , mpCacheMaxEntries = cacheEntries
        , mpMaterialAggregateBytes = 0
        , mpMaxResponseBytes = responseBytes
        , mpMaxRequestBytes = requestBytes
        , mpAdmissionCapacity = piCpuAdmission inputs
        , mpShedCapabilities = Nothing
        , mpPublishTenant = publishTenant
        , mpMirrorArtifactTenant = mirrorArtifactTenant
        , mpQueueMemoryMaxDepth = queueDepth
        , mpQueueTenantBytes = queueCharge (memoryQueueCharged demand) queueDepth
        , mpFixedBufferBytes = fixedBufferBytes demand
        , mpDegradations = []
        , mpOverrideViolations = []
        }
    , [piCpuAdmissionLine inputs, responseLine, requestLine, cacheBytesLine, cacheEntriesLine, queueDepthLine]
        <> [artifactLine | anyMountMirrors demand]
    )
  where
    demand = piQueueDemand inputs
    pins = configuredPins inputs
    (responseBytes, responseLine) = fallbackOr "response byte cap" (opResponse pins) responseBytesFallback
    (requestBytes, requestLine) = fallbackOr "request byte cap" (opRequest pins) requestBytesFallback
    (cacheBytes, cacheBytesLine) = fallbackOr "cache byte bound" (opCache pins) cacheBytesFallback
    (cacheEntries, cacheEntriesLine) = fallbackOr "cache entry bound" (csMaxEntries (piCache inputs)) (clamp cacheEntriesFloor cacheEntriesCap (cacheBytes `div` cacheEntryExpectedBytes))
    (queueDepth, queueDepthLine) = fallbackOr "memory-queue depth" (opDepth pins) queueDepthFallback
    (artifactBytes, artifactLine) = fallbackOr "mirror artifact byte cap" (opArtifact pins) mirrorArtifactBytesCap
    publishTenant = listToMaybe [PublishTenant{ptAggregateBytes = publishAggregateFallbackRequests * requestBytes} | piPublishConfigured inputs]
    mirrorArtifactTenant = listToMaybe [MirrorArtifactTenant{matMaxBytes = artifactBytes} | anyMountMirrors demand]

fallbackOr :: Text -> Maybe Int -> Int -> (Int, Text)
fallbackOr name explicit fallback =
    resolveSized ("memory plan: " <> name) explicit fallback "built-in default; no heap-ceiling datapoint"

publishTenantOf :: TenantDemands -> ShedOutcomes -> Maybe PublishTenant
publishTenantOf d o = listToMaybe [PublishTenant{ptAggregateBytes = soPublishFinal o} | tdPublishConfigured d]

mirrorArtifactTenantOf :: TenantDemands -> ShedOutcomes -> Maybe MirrorArtifactTenant
mirrorArtifactTenantOf d o = listToMaybe [MirrorArtifactTenant{matMaxBytes = soArtifactCapFinal o} | tdMirrors d]

{- | The metadata cache's tunables: the configured TTL with the plan's cache aggregate
split across the three stores. A zero aggregate stores nothing, so the proxy serves uncached.
-}
planCacheConfig :: CacheSettings -> MemoryPlan -> CacheConfig
planCacheConfig cacheSettings plan =
    CacheConfig
        { cacheTtl = csTtl cacheSettings
        , cacheFullBudget = StoreBudget{sbMaxEntries = entries, sbMaxBytes = fullBytes}
        , cacheVersionBudget = StoreBudget{sbMaxEntries = cacheVersionEntriesFactor * entries, sbMaxBytes = versionBytes}
        , cacheAssembledBudget = StoreBudget{sbMaxEntries = entries, sbMaxBytes = aggregate - fullBytes - versionBytes}
        }
  where
    aggregate = mpCacheAggregateBytes plan
    entries = mpCacheMaxEntries plan
    fullBytes = aggregate * cacheFullSharePercent `div` 100
    versionBytes = aggregate * cacheVersionSharePercent `div` 100

-- The named split of the cache aggregate, in percent. The assembled store takes
-- the remainder, so the three sub-budgets sum to exactly the aggregate.
cacheFullSharePercent :: Int
cacheFullSharePercent = 60

cacheVersionSharePercent :: Int
cacheVersionSharePercent = 15

-- The version store's entries are flat and small (16 KiB estimates against the
-- full store's 256 KiB), so it holds several per full entry.
cacheVersionEntriesFactor :: Int
cacheVersionEntriesFactor = 4
