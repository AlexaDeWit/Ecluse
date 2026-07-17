-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory budget: one place that chunks the resolved heap
ceiling between the process's memory tenants, so every byte-valued bound is a
share of the machine actually underneath it rather than a flat guess.

With a resolved ceiling @H@ (explicit @runtime.maxHeapBytes@, the container's
cgroup limit, or the RTS cap -- see "Ecluse.Rts"), the partition is:

* __Half of @H@: materialisation working space.__ Each admitted metadata
  materialisation holds a response body and its decoded form concurrently, so the
  per-response byte cap is that share divided across the admission slots and the
  decoded expansion factor (clamped: the floor keeps real-world packuments
  admissible on a small pod, where the shipped policy value is already known good).
* __A quarter of @H@: the metadata cache__, the heap's main long-lived tenant
  (clamped), with its entry bound tracking the byte budget at one slot per
  expected decoded packument.
* __The remaining quarter: slack__ -- the in-memory mirror queue (a depth cap from
  a per-job byte estimate), buffered request bodies (the request cap), and the
  RTS's own copying headroom.

With no ceiling datapoint at all, every bound falls back to the shipped values
that predate the budget. An explicit config value always wins its bound, and every
decision returns a provenance line for the boot log.
-}
module Ecluse.Composition.MemoryBudget (
    MemoryBudget (..),
    resolveMemoryBudget,
    budgetCacheConfig,
) where

import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (..))
import Ecluse.Rts (RuntimePlan (planMaxHeapBytes), provenanceClause)

{- | The resolved byte-valued bounds, each an explicit config value or its
heap-ceiling-derived default (see the module header for the partition).
-}
data MemoryBudget = MemoryBudget
    { mbMaxResponseBytes :: Int
    , mbMaxRequestBytes :: Int
    , mbCacheMaxBytes :: Int
    , mbCacheMaxEntries :: Int
    , mbQueueMemoryMaxDepth :: Int
    }
    deriving stock (Eq, Show)

{- | Resolve the memory budget from the configuration groups, the resolved runtime
posture ("Ecluse.Rts"; its heap ceiling is the partitioned quantity, and its
provenance rides into each boot line), and the resolved serve-admission capacity
("Ecluse.Composition.Sizing"; the number of concurrent materialisation slots the
response share is divided across). This is the memory half of one resolution
pipeline: posture, then capacities, then this partition over both.
-}
resolveMemoryBudget ::
    CacheSettings ->
    LimitsSettings ->
    QueueSettings ->
    RuntimePlan ->
    Int ->
    (MemoryBudget, [Text])
resolveMemoryBudget cacheSettings limitsSettings queueSettings plan admission =
    ( MemoryBudget
        { mbMaxResponseBytes = responseBytes
        , mbMaxRequestBytes = requestBytes
        , mbCacheMaxBytes = cacheBytes
        , mbCacheMaxEntries = cacheEntries
        , mbQueueMemoryMaxDepth = queueDepth
        }
    , [responseLine, requestLine, cacheBytesLine, cacheEntriesLine, queueDepthLine]
    )
  where
    (responseBytes, responseLine) =
        resolved "response byte cap" (limMaxResponseBytes limitsSettings) responseBytesFallback $ \h ->
            clampWith
                responseBytesFloor
                responseBytesCap
                ((h `div` workingSpaceHeapShare) `div` (max 1 admission * decodedExpansionFactor))
    (requestBytes, requestLine) =
        resolved "request byte cap" (limMaxRequestBytes limitsSettings) requestBytesFallback $ \h ->
            clampWith requestBytesFloor requestBytesCap (h `div` requestBytesHeapShare)
    (cacheBytes, cacheBytesLine) =
        resolved "cache byte bound" (csMaxBytes cacheSettings) cacheBytesFallback $ \h ->
            clampWith cacheBytesFloor cacheBytesCap (h `div` cacheBytesHeapShare)
    -- The entry bound tracks the resolved byte budget (not the raw ceiling), so
    -- neither cache bound alone starves the other.
    (cacheEntries, cacheEntriesLine) = case csMaxEntries cacheSettings of
        Just n -> (n, "memory budget: cache entry bound " <> show n <> " (from config)")
        Nothing ->
            let computed = clampWith cacheEntriesFloor cacheEntriesCap (cacheBytes `div` cacheEntryExpectedBytes)
             in (computed, "memory budget: cache entry bound " <> show computed <> " (computed from the cache byte budget)")
    (queueDepth, queueDepthLine) =
        resolved "memory-queue depth" (qsMemoryMaxDepth queueSettings) queueDepthFallback $ \h ->
            clampWith queueDepthFloor queueDepthCap (h `div` queueDepthHeapShareBytes)

    (heapCeiling, ceilingProvenance) = planMaxHeapBytes plan

    resolved :: Text -> Maybe Int -> Int -> (Int -> Int) -> (Int, Text)
    resolved name explicit fallback compute = case explicit of
        Just n -> (n, "memory budget: " <> name <> " " <> show n <> " (from config)")
        Nothing -> case heapCeiling of
            Just h ->
                let computed = compute h
                 in ( computed
                    , "memory budget: "
                        <> name
                        <> " "
                        <> show computed
                        <> " (computed from heap ceiling "
                        <> show h
                        <> ", "
                        <> provenanceClause ceilingProvenance
                        <> ")"
                    )
            Nothing ->
                (fallback, "memory budget: " <> name <> " " <> show fallback <> " (built-in default; no heap-ceiling datapoint)")

    clampWith :: Int -> Int -> Int -> Int
    clampWith lo hi = max lo . min hi

{- | The metadata cache's tunables: the configured TTL married to the budget's
resolved bounds, so the pairing happens once here rather than at each consumer.
-}
budgetCacheConfig :: CacheSettings -> MemoryBudget -> CacheConfig
budgetCacheConfig cacheSettings budget =
    CacheConfig
        { cacheTtl = csTtl cacheSettings
        , cacheMaxEntries = mbCacheMaxEntries budget
        , cacheMaxBytes = mbCacheMaxBytes budget
        }

-- Half the ceiling is materialisation working space (the divisor 2 below), spread
-- across the admission slots; a decoded packument runs several times its wire
-- size, folded in as a fixed expansion factor.
workingSpaceHeapShare :: Int
workingSpaceHeapShare = 2

decodedExpansionFactor :: Int
decodedExpansionFactor = 4

-- The response-cap floor is the shipped policy value: real-world packuments reach
-- multiple MiB, so a small pod must never compute itself below what is known to
-- admit them. The cap keeps one hostile document from monopolising a huge heap.
responseBytesFloor :: Int
responseBytesFloor = 12582912

responseBytesCap :: Int
responseBytesCap = 67108864

responseBytesFallback :: Int
responseBytesFallback = 12582912

-- Request bodies (publishes) buffer at most one cap each and are rarer than
-- responses, so they draw a thirty-second of the ceiling from the slack share.
requestBytesHeapShare :: Int
requestBytesHeapShare = 32

requestBytesFloor :: Int
requestBytesFloor = 26214400

requestBytesCap :: Int
requestBytesCap = 104857600

requestBytesFallback :: Int
requestBytesFallback = 26214400

-- The cache takes a quarter of the ceiling: big enough that a hot fleet's
-- metadata working set stays decoded, small enough that the working-space half
-- and the slack quarter stay whole.
cacheBytesHeapShare :: Int
cacheBytesHeapShare = 4

-- A floor so a tiny pod still caches a useful working set, a cap because past a
-- gigabyte of decoded metadata the TTL, not memory, is the effective bound.
cacheBytesFloor :: Int
cacheBytesFloor = 67108864

cacheBytesCap :: Int
cacheBytesCap = 1073741824

cacheBytesFallback :: Int
cacheBytesFallback = 268435456

-- The expected decoded footprint of one cached packument (256 KiB).
cacheEntryExpectedBytes :: Int
cacheEntryExpectedBytes = 262144

cacheEntriesFloor :: Int
cacheEntriesFloor = 256

cacheEntriesCap :: Int
cacheEntriesCap = 65536

-- One queue slot per 8 KiB of ceiling: a queued mirror job is small (a name, a
-- version, an artifact URL, ~1 KiB), so even a full queue holds well under the
-- slack quarter.
queueDepthHeapShareBytes :: Int
queueDepthHeapShareBytes = 8192

queueDepthFloor :: Int
queueDepthFloor = 5000

queueDepthCap :: Int
queueDepthCap = 100000

queueDepthFallback :: Int
queueDepthFallback = 50000
