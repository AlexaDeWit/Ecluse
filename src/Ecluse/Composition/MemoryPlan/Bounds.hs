-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shipped numbers the memory plan resolves against, and the byte charge a tenant's demand
turns into. The shares carve up the application heap, the floors and caps bracket every computed
bound through @clamp@, and the fallbacks stand in for a pod with no heap-ceiling datapoint. A
configured value overrides a bound. These are what is left when none is configured.
-}
module Ecluse.Composition.MemoryPlan.Bounds (
    -- * Shares of the application heap
    runtimeReserveShareDiv,
    runtimeReserveFloorBytes,
    cacheSharePercent,
    materialSharePercent,
    publishSharePercent,
    queueSharePercent,
    mirrorArtifactSharePercent,

    -- * Byte floors, caps, and no-ceiling fallbacks
    responseBytesFallback,
    materialBytesFallback,
    materialAllowances,
    requestBytesFloor,
    requestBytesCap,
    requestBytesFallback,
    cacheBytesFloor,
    cacheBytesCap,
    cacheBytesFallback,
    cacheEntryExpectedBytes,
    cacheEntriesFloor,
    cacheEntriesCap,
    queueDepthFloor,
    queueDepthCap,
    queueDepthFallback,
    publishAggregateFallbackRequests,
    mirrorArtifactEnvelopeMultiplier,
    mirrorArtifactBytesCap,

    -- * Tenant charges
    queueCharge,
    fixedBufferBytes,
    anyMountMirrors,
    memoryQueueCharged,
) where

import Ecluse.Composition.MemoryPlan.Types (QueueTenantDemand (MemoryQueueTenant, NoQueueTenant))
import Ecluse.Composition.Sizing (mirrorEnqueueBufferDepth)
import Ecluse.Core.Security.Limits (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Material (MaterialAllowances (..))
import Ecluse.Core.Server.MemoryModel (mirrorJobEstimatedBytes)

{- | The divisor taking the runtime reserve off the ceiling. The GC and the RTS get a fifth of
whatever the pod has.
-}
runtimeReserveShareDiv :: Int
runtimeReserveShareDiv = 5

-- | The smallest runtime reserve, so a tiny pod still leaves the RTS something to breathe with.
runtimeReserveFloorBytes :: Int
runtimeReserveFloorBytes = 33554432

{- | The cache aggregate's share of the application heap, the ceiling less the runtime reserve. The
four computed shares sum to 95%, so a plan with no floor and no pin in it fits by construction.
-}
cacheSharePercent :: Int
cacheSharePercent = 30

-- | The materialisation envelope's share of the application heap.
materialSharePercent :: Int
materialSharePercent = 45

-- | The publish aggregate's share of the application heap, and the computed request cap's.
publishSharePercent :: Int
publishSharePercent = 15

-- | The memory-queue depth's share of the application heap.
queueSharePercent :: Int
queueSharePercent = 5

{- | The mirror-artifact tenant's share of the application heap. It is the charged envelope, kept
small so the background back-fill never crowds the serve hot path.
-}
mirrorArtifactSharePercent :: Int
mirrorArtifactSharePercent = 4

-- | The metadata ingest ceiling, independent of the heap and CPU controls.
responseBytesFallback :: Int
responseBytesFallback = maxMetadataBytes defaultLimits

{- | Static estimates from the declared npm/PyPI stage workload with 25% headroom, rounded up.
See <https://github.com/AlexaDeWit/Ecluse/issues/1427> for the workload and measurement limits.
-}
materialAllowances :: MaterialAllowances
materialAllowances =
    MaterialAllowances
        { maColdSelectedBytes = 9437184
        , maRetainedSelectedBytes = 262144
        , maFullOriginBytes = 38797312
        , maListingOutputBytes = 11534336
        }

-- | Two calibrated two-origin listings without a heap datapoint, independent of CPU capacity.
materialBytesFallback :: Int
materialBytesFallback = 2 * (2 * maFullOriginBytes materialAllowances + maListingOutputBytes materialAllowances)

-- | The smallest computed publish-body cap.
requestBytesFloor :: Int
requestBytesFloor = 26214400

-- | The largest computed publish-body cap.
requestBytesCap :: Int
requestBytesCap = 104857600

-- | The publish-body cap a pod with no heap-ceiling datapoint gets.
requestBytesFallback :: Int
requestBytesFallback = 26214400

{- | The floor keeps a pod that can afford one caching a useful working set. The shed ladder may
still take the aggregate to zero.
-}
cacheBytesFloor :: Int
cacheBytesFloor = 67108864

-- | The maximum computed aggregate for local metadata retention.
cacheBytesCap :: Int
cacheBytesCap = 1073741824

-- | The cache aggregate a pod with no heap-ceiling datapoint gets.
cacheBytesFallback :: Int
cacheBytesFallback = 268435456

-- | The planning allowance per shared local metadata entry slot (256 KiB).
cacheEntryExpectedBytes :: Int
cacheEntryExpectedBytes = 262144

-- | The smallest computed cache entry bound.
cacheEntriesFloor :: Int
cacheEntriesFloor = 256

-- | The largest computed cache entry bound.
cacheEntriesCap :: Int
cacheEntriesCap = 65536

-- | The depth the queue tenant sheds to, and the floor under any computed depth.
queueDepthFloor :: Int
queueDepthFloor = 5000

-- | The largest computed memory-queue depth.
queueDepthCap :: Int
queueDepthCap = 100000

-- | The memory-queue depth a pod with no heap-ceiling datapoint gets.
queueDepthFallback :: Int
queueDepthFallback = 50000

{- | With no ceiling datapoint the publish aggregate falls back to a few maximum requests' worth of
concurrent body room.
-}
publishAggregateFallbackRequests :: Int
publishAggregateFallbackRequests = 4

{- | The transient envelope one mirrored artifact holds, as a multiple of the buffered tarball B. The
tarball, its base64 'Text', and the publish document coexist at ~3.7x B, rounded up so the peak fits.
-}
mirrorArtifactEnvelopeMultiplier :: Int
mirrorArtifactEnvelopeMultiplier = 4

{- | The ceiling the plan clamps the computed artifact cap to, and the no-ceiling fallback. The
charged envelope is therefore at most this times 'mirrorArtifactEnvelopeMultiplier'.
-}
mirrorArtifactBytesCap :: Int
mirrorArtifactBytesCap = 512 * 1024 * 1024

-- | The bytes a memory-queue depth charges. Zero unless the memory backend runs.
queueCharge :: Bool -> Int -> Int
queueCharge memoryBacked d = if memoryBacked then d * mirrorJobEstimatedBytes else 0

-- | The enqueue hand-off buffer, charged whatever the backend behind it.
fixedBufferBytes :: QueueTenantDemand -> Int
fixedBufferBytes demand
    | anyMountMirrors demand = mirrorEnqueueBufferDepth * mirrorJobEstimatedBytes
    | otherwise = 0

-- | Whether any mount mirrors, whatever backend carries the jobs. The enqueue buffer rides this.
anyMountMirrors :: QueueTenantDemand -> Bool
anyMountMirrors = (/= NoQueueTenant)

-- | Whether the in-memory queue runs, so its depth charges the heap.
memoryQueueCharged :: QueueTenantDemand -> Bool
memoryQueueCharged = (== MemoryQueueTenant)
