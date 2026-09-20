-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Divide the heap ceiling among named tenants and report the resolved bounds.
"Ecluse.Composition.MemoryPlan.Shed" reduces demands to fit the ceiling.
"Ecluse.Composition.MemoryPlan.Override" checks explicit overrides.
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

import Data.Ord (clamp)

import Ecluse.Composition.MemoryPlan.Bounds (
    anyMountMirrors,
    cacheBytesFallback,
    cacheEntriesCap,
    cacheEntriesFloor,
    cacheEntryExpectedBytes,
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
import Ecluse.Composition.MemoryPlan.Render (localCachePolicyLine, renderDegradations, renderPlanLines)
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
    , [localCachePolicyLine, piCpuAdmissionLine inputs, responseLine, requestLine, cacheBytesLine, cacheEntriesLine, queueDepthLine]
        <> [artifactLine | anyMountMirrors demand]
    )
  where
    demand = piQueueDemand inputs
    pins = configuredPins inputs
    (responseBytes, responseLine) = fallbackOr "response byte cap" (opResponse pins) responseBytesFallback
    (requestBytes, requestLine) = fallbackOr "request byte cap" (opRequest pins) requestBytesFallback
    (cacheBytes, cacheBytesLine) = fallbackOr "cache byte bound" (opCache pins) cacheBytesFallback
    (cacheEntries, cacheEntriesLine) = fallbackOr "cache entry bound" (csMaxEntries (piCache inputs)) (clamp (cacheEntriesFloor, cacheEntriesCap) (cacheBytes `div` cacheEntryExpectedBytes))
    (queueDepth, queueDepthLine) = fallbackOr "memory-queue depth" (opDepth pins) queueDepthFallback
    (artifactBytes, artifactLine) = fallbackOr "mirror artifact byte cap" (opArtifact pins) mirrorArtifactBytesCap
    publishTenant = PublishTenant{ptAggregateBytes = publishAggregateFallbackRequests * requestBytes} <$ guard (piPublishConfigured inputs)
    mirrorArtifactTenant = MirrorArtifactTenant{matMaxBytes = artifactBytes} <$ guard (anyMountMirrors demand)

fallbackOr :: Text -> Maybe Int -> Int -> (Int, Text)
fallbackOr name explicit fallback =
    resolveSized ("memory plan: " <> name) explicit fallback "built-in default; no heap-ceiling datapoint"

publishTenantOf :: TenantDemands -> ShedOutcomes -> Maybe PublishTenant
publishTenantOf d o = PublishTenant{ptAggregateBytes = soPublishFinal o} <$ guard (tdPublishConfigured d)

mirrorArtifactTenantOf :: TenantDemands -> ShedOutcomes -> Maybe MirrorArtifactTenant
mirrorArtifactTenantOf d o = MirrorArtifactTenant{matMaxBytes = soArtifactCapFinal o} <$ guard (tdMirrors d)

-- | Apply one aggregate bound to eligible stores. Zero floors reserve no static shares.
planCacheConfig :: CacheSettings -> MemoryPlan -> CacheConfig
planCacheConfig cacheSettings plan =
    CacheConfig
        { cacheTtl = csTtl cacheSettings
        , cacheMaxEntries = mpCacheMaxEntries plan
        , cacheMaxBytes = mpCacheAggregateBytes plan
        , cacheVersionBudget = StoreBudget 0 0
        , cacheAssembledBudget = StoreBudget 0 0
        }
