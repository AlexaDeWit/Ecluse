-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory plan's public vocabulary: the resolved plan and the tenants that carry a
shape of their own. 'Ecluse.Composition.MemoryPlan.resolveMemoryPlan' solves for these, and
the composition root builds every byte-valued bound from them.
-}
module Ecluse.Composition.MemoryPlan.Types (
    MemoryPlan (..),
    PublishTenant (..),
    MirrorArtifactTenant (..),
    QueueTenantDemand (..),
    queueTenantDemand,
) where

import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), MirrorRuntimePlan (MirrorWith, NoMirroring))

{- | Whether the memory plan owes the in-memory queue a tenant, projected from the
backend selection ('Ecluse.Composition.MirrorQueue.planMirrorRuntime').
-}
data QueueTenantDemand
    = -- | No mount mirrors: no queue tenant, no enqueue buffer.
      NoQueueTenant
    | -- | Mirroring rides a durable backend, so the plan charges the enqueue buffer alone.
      MirroringWithoutMemoryQueue
    | -- | Mirroring rides the in-memory queue: its depth is a tenant of this plan.
      MemoryQueueTenant
    deriving stock (Eq, Show)

-- | Project the queue-tenant demand from the resolved mirror runtime plan.
queueTenantDemand :: MirrorRuntimePlan -> QueueTenantDemand
queueTenantDemand = \case
    NoMirroring -> NoQueueTenant
    MirrorWith (SqsBackend _) -> MirroringWithoutMemoryQueue
    MirrorWith MemoryBackend -> MemoryQueueTenant

-- | The publish tenant: the aggregate byte-admission for concurrently buffered bodies.
newtype PublishTenant = PublishTenant
    { ptAggregateBytes :: Int
    }
    deriving stock (Eq, Show)

{- | The mirror-artifact tenant, present only when some mount mirrors. The plan charges its
heap as 'matMaxBytes' scaled by 'Ecluse.Composition.MemoryPlan.Bounds.mirrorArtifactEnvelopeMultiplier'.
-}
newtype MirrorArtifactTenant = MirrorArtifactTenant
    { matMaxBytes :: Int
    -- ^ The worker's per-artifact fetch byte cap (the tarball bound @B@).
    }
    deriving stock (Eq, Show)

{- | The resolved plan: every byte-valued bound the composition root builds with, each an
explicit config value or its tenant-derived default. Override violations are the one refusal.
-}
data MemoryPlan = MemoryPlan
    { mpRuntimeReserveBytes :: Int
    -- ^ Tenant 1, taken off the top. Zero with no ceiling datapoint.
    , mpCacheAggregateBytes :: Int
    -- ^ Tenant 3: one shared byte bound for all eligible local cache stores.
    , mpCacheMaxEntries :: Int
    , mpMaterialAggregateBytes :: Int
    -- ^ Tenant 4: the independent heuristic capacity for concurrent metadata work.
    , mpMaxResponseBytes :: Int
    -- ^ The fixed metadata ingest ceiling, independent of memory and CPU admission.
    , mpMaxRequestBytes :: Int
    -- ^ The per-request (publish body) wire cap @Q@, enforced at the publish read site.
    , mpAdmissionCapacity :: Int
    -- ^ CPU-derived concurrency, or the exact explicit operator pin.
    , mpShedCapabilities :: Maybe Int
    {- ^ A count to shrink to when the nursery is the memory pressure, each capability holding an
    allocation area. 'Nothing' leaves the live count.
    -}
    , mpPublishTenant :: Maybe PublishTenant
    -- ^ Tenant 5, present only when a publication target is configured.
    , mpMirrorArtifactTenant :: Maybe MirrorArtifactTenant
    -- ^ Tenant 7, present only when some mount mirrors. Carries the worker's cap.
    , mpQueueMemoryMaxDepth :: Int
    -- ^ The in-memory queue's depth cap (the build parameter, always resolved).
    , mpQueueTenantBytes :: Int
    -- ^ Tenant 6: the bytes the depth charges. Zero unless the memory backend runs.
    , mpFixedBufferBytes :: Int
    -- ^ Tenant 2: the enqueue buffer, charged whenever any mount mirrors.
    , mpDegradations :: [Text]
    -- ^ Shed-ladder and explicit-control warnings, including the limits of material estimates.
    , mpOverrideViolations :: [Text]
    {- ^ The pins the plan blames for a residual overshoot it cannot shed around. The boot and
    check-config refuse on these with exit 2.
    -}
    }
    deriving stock (Eq, Show)
