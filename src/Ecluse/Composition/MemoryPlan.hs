-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory plan: one solver that partitions the effective
heap ceiling between __named tenants__ whose sum is bounded by the ceiling, so the
budgets compose instead of each claiming an independent share of the same bytes.

With an effective ceiling @H@ ("Ecluse.Rts", what the RTS actually runs with), the
tenants are, in allocation order:

1. __The runtime reserve__: GC copying headroom, stacks, buffers, and the RTS
   itself, taken off the top; everything else partitions the remainder (the
   application heap).
2. __Fixed buffers__: the mirror enqueue hand-off buffer, charged whenever any
   mount mirrors (whatever the backend).
3. __The cache aggregate__: ONE byte budget for all three metadata stores, split
   into their named sub-budgets at 'planCacheConfig' (the split sums to the
   aggregate, so the cache can never triple its tenant).
4. __The material aggregate__: response materialisation working space. The
   admission capacity is @max 1 (min A_cpu A_mem)@ -- the CPU-derived capacity
   ("Ecluse.Composition.Sizing") jointly bounded by what the material share can
   actually hold, one envelope ('packumentOriginFanout' concurrent origins, each
   wire+parsed at 'expandWireBytes') per admitted operation.
5. __The publish aggregate__: the total bytes concurrently buffered publish
   bodies may hold (the byte-admission the publish pipeline acquires before
   reading a body), present only when a publication target is configured.
6. __The queue tenant__: the in-memory mirror queue's depth, charged only when
   the memory backend was selected (the selection precedes this plan; an SQS
   deployment spends no heap on queued jobs).

The combined invariant -- reserve + cache + material + publish + queue + fixed
buffers within the ceiling -- is enforced by construction for computed shares.
Explicit overrides are re-checked by attribution, described under degradation
below: a pin refuses the boot only when it is what a fitting plan cannot shed
around, never on mere presence.

== Graceful degradation, never a refusal

A pod too small for the tenants' normal floors sheds in a documented priority
order, each step a loud boot warning naming what was given up and why: first the
cache aggregate shrinks below its floor, ultimately to zero (the proxy serves
uncached); then admission shrinks toward one in-flight operation (and, where the
nursery is the real pressure, the capability count sheds with it, automating the
smaller-cores recommendation); then the publish aggregate shrinks to one maximum
request; then the queue depth to its floor. One operation on one capability with
no cache is the irreducible minimum and __always boots__ -- if even that exceeds
the ceiling, the plan says so in its loudest warning and boots anyway (the cgroup
backstop is then the guard).

Only an __explicit operator override__ refuses the boot, and only when it is the
cause. The plan re-derives the override-free minimum (every pin substituted out,
every computed tenant at its floor) and refuses just when that minimum fits the
ceiling while the pinned plan does not, naming the pins whose individual removal
would fit (all of them when only their combination overshoots). A pin at or below
the value the shed ladder would compute anyway pushes the plan past nothing and
never refuses; a pod too small even without the pins boots with the loud warning
like any other. An override is an operator claim, and a claim that alone breaks
the ceiling is a misconfiguration to fix, not to shrink around.

With no ceiling datapoint at all, every bound falls back to the shipped values
that predate the plan. An explicit config value always wins its own bound, and
every decision returns a provenance line for the boot log.
-}
module Ecluse.Composition.MemoryPlan (
    MemoryPlan (..),
    PublishTenant (..),
    QueueTenantDemand (..),
    queueTenantDemand,
    resolveMemoryPlan,
    planCacheConfig,
) where

import Data.Text qualified as T

import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), MirrorRuntimePlan (MirrorWith, NoMirroring))
import Ecluse.Composition.Sizing (mirrorEnqueueBufferDepth, resolveServeAdmission)
import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
import Ecluse.Core.Server.MemoryModel (expandWireBytes, mirrorJobEstimatedBytes, packumentOriginFanout)
import Ecluse.Rts (EffectiveRuntimePlan (erpAllocAreaBytes), effectiveCapabilities, effectiveHeapCeiling, provenanceClause)

{- | Whether the memory plan owes the in-memory queue a tenant, projected from the
already-made backend selection ('Ecluse.Composition.MirrorQueue.planMirrorRuntime'):
only the memory backend spends heap on queued jobs, while any mirroring at all
charges the fixed enqueue buffer.
-}
data QueueTenantDemand
    = -- | No mount mirrors: no queue tenant, no enqueue buffer.
      NoQueueTenant
    | -- | Mirroring rides a durable backend: the enqueue buffer alone is charged.
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

{- | The resolved plan: every byte-valued bound the composition root builds with,
each an explicit config value or its tenant-derived default, plus the degradation
warnings the solver took and any explicit-override violations (the one refusal).
-}
data MemoryPlan = MemoryPlan
    { mpRuntimeReserveBytes :: Int
    -- ^ Tenant 1: taken off the top; zero with no ceiling datapoint.
    , mpCacheAggregateBytes :: Int
    -- ^ Tenant 3: the one cache aggregate, split at 'planCacheConfig'.
    , mpCacheMaxEntries :: Int
    , mpMaterialAggregateBytes :: Int
    -- ^ Tenant 4: the materialisation envelope bytes admission may hold at once.
    , mpMaxResponseBytes :: Int
    -- ^ The per-response wire cap @R@ carved from the material aggregate.
    , mpMaxRequestBytes :: Int
    -- ^ The per-request (publish body) wire cap @Q@, the WAI size limit.
    , mpAdmissionCapacity :: Int
    -- ^ @max 1 (min A_cpu A_mem)@; the composition root builds admission from it.
    , mpShedCapabilities :: Maybe Int
    {- ^ A capability count to shrink to when the nursery is the memory pressure
    (each capability holds an allocation area); 'Nothing' when the live count
    stands. Applied in-process by the composition root.
    -}
    , mpPublishTenant :: Maybe PublishTenant
    -- ^ Tenant 5, present only when a publication target is configured.
    , mpQueueMemoryMaxDepth :: Int
    -- ^ The in-memory queue's depth cap (the build parameter, always resolved).
    , mpQueueTenantBytes :: Int
    -- ^ Tenant 6: the bytes the depth charges; zero unless the memory backend runs.
    , mpFixedBufferBytes :: Int
    -- ^ Tenant 2: the enqueue buffer, charged whenever any mount mirrors.
    , mpDegradations :: [Text]
    -- ^ The shed-ladder warnings, in the order taken; empty when everything fits.
    , mpOverrideViolations :: [Text]
    {- ^ The pins named as the cause of a residual overshoot the plan cannot shed
    around: populated only when the override-free minimum fits the ceiling while
    the pinned plan does not. The boot and check-config refuse on these (exit 2).
    A pin that contributes nothing to the overshoot is never named, and a pod too
    small even without the pins boots (a degradation, not a refusal).
    -}
    }
    deriving stock (Eq, Show)

{- | Resolve the memory plan and its boot lines from the configuration groups, the
effective runtime plan, the explicit admission override, the queue-tenant demand
(the backend selection precedes this plan), and whether any mount publishes.
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
    maybe fallbackPlan solvedPlan heapCeiling
  where
    (heapCeiling, ceilingProvenance) = effectiveHeapCeiling runtime
    (capabilities, _) = effectiveCapabilities runtime

    -- The CPU-derived admission capacity (explicit serveMaxInFlight wins inside).
    (cpuAdmission, cpuAdmissionLine) = resolveServeAdmission explicitAdmission capabilities

    fixedBuffers = case queueDemand of
        NoQueueTenant -> 0
        _ -> mirrorEnqueueBufferDepth * mirrorJobEstimatedBytes

    -- One admitted operation's envelope at the response cap r: the concurrent
    -- origins' wire+parsed forms.
    envelope r = packumentOriginFanout * expandWireBytes r

    {- No ceiling datapoint: the shipped fallbacks that predate the plan, admission
    from CPU alone, and no tenant arithmetic to check (there is nothing to sum
    against). -}
    fallbackPlan :: (MemoryPlan, [Text])
    fallbackPlan =
        ( MemoryPlan
            { mpRuntimeReserveBytes = 0
            , mpCacheAggregateBytes = cacheBytes
            , mpCacheMaxEntries = cacheEntries
            , mpMaterialAggregateBytes = 0
            , mpMaxResponseBytes = responseBytes
            , mpMaxRequestBytes = requestBytes
            , mpAdmissionCapacity = cpuAdmission
            , mpShedCapabilities = Nothing
            , mpPublishTenant = publishTenant
            , mpQueueMemoryMaxDepth = queueDepth
            , mpQueueTenantBytes = if queueDemand == MemoryQueueTenant then queueDepth * mirrorJobEstimatedBytes else 0
            , mpFixedBufferBytes = fixedBuffers
            , mpDegradations = []
            , mpOverrideViolations = []
            }
        , [cpuAdmissionLine, responseLine, requestLine, cacheBytesLine, cacheEntriesLine, queueDepthLine]
        )
      where
        (responseBytes, responseLine) = fallbackOr "response byte cap" (limMaxResponseBytes limitsSettings) responseBytesFallback
        (requestBytes, requestLine) = fallbackOr "request byte cap" (limMaxRequestBytes limitsSettings) requestBytesFallback
        (cacheBytes, cacheBytesLine) = fallbackOr "cache byte bound" (csMaxBytes cacheSettings) cacheBytesFallback
        (cacheEntries, cacheEntriesLine) = fallbackOr "cache entry bound" (csMaxEntries cacheSettings) (clamp cacheEntriesFloor cacheEntriesCap (cacheBytes `div` cacheEntryExpectedBytes))
        (queueDepth, queueDepthLine) = fallbackOr "memory-queue depth" (qsMemoryMaxDepth queueSettings) queueDepthFallback
        publishTenant = listToMaybe [PublishTenant{ptAggregateBytes = publishAggregateFallbackRequests * requestBytes} | publishConfigured]

        fallbackOr name explicit fallback = case explicit of
            Just n -> (n, "memory plan: " <> name <> " " <> show n <> " (from config)")
            Nothing -> (fallback, "memory plan: " <> name <> " " <> show fallback <> " (built-in default; no heap-ceiling datapoint)")

    {- The solved plan over a ceiling h: allocate the named tenants at their
    desired shares, then walk the shed ladder until the sum fits (or every
    computed tenant is at its minimum). -}
    solvedPlan :: Int -> (MemoryPlan, [Text])
    solvedPlan h =
        ( MemoryPlan
            { mpRuntimeReserveBytes = reserve
            , mpCacheAggregateBytes = cacheFinal
            , mpCacheMaxEntries = cacheEntries
            , mpMaterialAggregateBytes = materialFinal
            , mpMaxResponseBytes = responseFinal
            , mpMaxRequestBytes = requestFinal
            , mpAdmissionCapacity = admissionFinal
            , mpShedCapabilities = shedCapabilities
            , mpPublishTenant = publishTenant
            , mpQueueMemoryMaxDepth = depthFinal
            , mpQueueTenantBytes = queueTenantBytes
            , mpFixedBufferBytes = fixedBuffers
            , mpDegradations = degradations
            , mpOverrideViolations = overrideViolations
            }
        , planLines
        )
      where
        reserve = max runtimeReserveFloorBytes (h `div` runtimeReserveShareDiv)
        appHeap = max 0 (h - reserve)

        -- Explicit-or-computed per tenant, remembering which were explicit (an
        -- explicit bound never sheds; a violation attributes to it).
        cacheExplicit = csMaxBytes cacheSettings
        cacheDesired = fromMaybe (clamp cacheBytesFloor cacheBytesCap (appHeap * cacheSharePercent `div` 100)) cacheExplicit

        requestExplicit = limMaxRequestBytes limitsSettings
        requestFinal = fromMaybe (clamp requestBytesFloor requestBytesCap (appHeap * publishSharePercent `div` 100)) requestExplicit

        publishDesired = max requestFinal (appHeap * publishSharePercent `div` 100)

        responseExplicit = limMaxResponseBytes limitsSettings

        materialShareBytes = appHeap * materialSharePercent `div` 100

        -- Admission bounded jointly: the CPU capacity and what the material share
        -- holds at the floor response cap. An explicit serveMaxInFlight is a
        -- pinned claim (cpuAdmission already carries it).
        admissionMemBound = max 1 (materialShareBytes `div` envelope responseBytesFloor)
        admissionDesired = case explicitAdmission of
            Just n -> n
            Nothing -> max 1 (min cpuAdmission admissionMemBound)

        depthExplicit = qsMemoryMaxDepth queueSettings
        depthDesired = fromMaybe (clamp queueDepthFloor queueDepthCap ((appHeap * queueSharePercent `div` 100) `div` mirrorJobEstimatedBytes)) depthExplicit

        -- Desired byte charges per shedable tenant.
        materialOf a r = a * envelope r
        responseDesired = fromMaybe (clamp responseBytesFloor responseBytesCap (materialShareBytes `div` max 1 (admissionDesired * packumentOriginFanout) * 2 `div` 15)) responseExplicit
        materialDesired = materialOf admissionDesired responseDesired
        publishCharge = if publishConfigured then publishDesired else 0
        queueCharge d = if queueDemand == MemoryQueueTenant then d * mirrorJobEstimatedBytes else 0

        desiredSum = reserve + fixedBuffers + cacheDesired + materialDesired + publishCharge + queueCharge depthDesired

        -- The shed ladder, walked only when the desired sum exceeds the ceiling.
        overshoot0 = max 0 (desiredSum - h)

        -- Step 1: the cache gives way first, to zero if needed (never an explicit one).
        cacheReclaimable = if isJust cacheExplicit then 0 else cacheDesired
        cacheShed = min overshoot0 cacheReclaimable
        cacheFinal = cacheDesired - cacheShed
        overshoot1 = overshoot0 - cacheShed

        -- Step 2: admission shrinks toward one in-flight operation at the floor
        -- response cap (never an explicit one).
        materialMinimum = case explicitAdmission of
            Just n -> materialOf n (fromMaybe responseBytesFloor responseExplicit)
            Nothing -> materialOf 1 (fromMaybe responseBytesFloor responseExplicit)
        materialReclaimable = max 0 (materialDesired - materialMinimum)
        materialShed = min overshoot1 materialReclaimable
        materialFinal = materialDesired - materialShed
        overshoot2 = overshoot1 - materialShed
        admissionFinal = case explicitAdmission of
            Just n -> n
            Nothing -> max 1 (min admissionDesired (materialFinal `div` envelope (fromMaybe responseBytesFloor responseExplicit)))
        responseFinal = case responseExplicit of
            Just r -> r
            Nothing -> clamp responseBytesFloor responseBytesCap (materialFinal `div` max 1 (admissionFinal * packumentOriginFanout) * 2 `div` 15)

        -- Where the nursery is the pressure (each capability holds an allocation
        -- area OUTSIDE the heap ceiling, which is why the tenant sum cannot see
        -- it), shed capabilities so the nursery fits a bounded share of the
        -- ceiling; the composition root applies it in-process.
        nurseryBytes = capabilities * allocArea
        allocArea = max 1 (erpAllocAreaBytes runtime)
        shedCapabilities
            | nurseryBytes > h `div` nurseryCeilingShareDiv =
                let fitted = max 1 ((h `div` nurseryCeilingShareDiv) `div` allocArea)
                 in if fitted < capabilities then Just fitted else Nothing
            | otherwise = Nothing

        -- Step 3: the publish aggregate shrinks to one maximum request.
        publishReclaimable = if publishConfigured then max 0 (publishDesired - requestFinal) else 0
        publishShed = min overshoot2 publishReclaimable
        publishFinal = publishDesired - publishShed
        overshoot3 = overshoot2 - publishShed
        publishTenant = listToMaybe [PublishTenant{ptAggregateBytes = publishFinal} | publishConfigured]

        -- Step 4: the queue depth to its floor (never an explicit one).
        depthReclaimableBytes = case depthExplicit of
            Just _ -> 0
            Nothing -> max 0 (queueCharge depthDesired - queueCharge queueDepthFloor)
        queueShedBytes = min overshoot3 depthReclaimableBytes
        depthFinal = case depthExplicit of
            Just n -> n
            Nothing
                | queueShedBytes > 0 -> max queueDepthFloor ((queueCharge depthDesired - queueShedBytes) `div` mirrorJobEstimatedBytes)
                | otherwise -> depthDesired
        queueTenantBytes = queueCharge depthFinal
        overshoot4 = overshoot3 - queueShedBytes

        -- Attribute a residual overshoot before refusing. Re-derive the fully-shed
        -- minimum for a hypothetical set of explicit pins: every un-pinned tenant
        -- at its floor (cache at zero), every pin at its claimed value. At the
        -- actual pins this reproduces overshoot4; substituting pins out isolates
        -- which of them, if any, a fitting plan cannot shed around.
        minShedSum (pinCache, pinAdmission, pinResponse, pinRequest, pinDepth) =
            reserve
                + fixedBuffers
                + fromMaybe 0 pinCache
                + materialOf (fromMaybe 1 pinAdmission) (fromMaybe responseBytesFloor pinResponse)
                + (if publishConfigured then requestFloorOr pinRequest else 0)
                + queueCharge (fromMaybe queueDepthFloor pinDepth)
          where
            requestFloorOr = fromMaybe (clamp requestBytesFloor requestBytesCap (appHeap * publishSharePercent `div` 100))
        overshootAt pins = max 0 (minShedSum pins - h)

        -- The override-free minimum: every pin substituted out at once. It fitting
        -- while the pinned plan does not is what makes the pins the cause (refuse);
        -- it overshooting too means the pod is simply too small (boot, warn).
        overshootWithoutOverrides = overshootAt (Nothing, Nothing, Nothing, Nothing, Nothing)

        -- Each explicit pin paired with the plan that substitutes only it out, for
        -- per-pin attribution (the value the shed ladder would reach without it:
        -- cache to zero, admission and response to their floors, request to the
        -- computed share, depth to the queue-depth floor).
        explicitPins =
            catMaybes
                [ ("cache.maxBytes", (Nothing, explicitAdmission, responseExplicit, requestExplicit, depthExplicit)) <$ cacheExplicit
                , ("runtime.serveMaxInFlight", (cacheExplicit, Nothing, responseExplicit, requestExplicit, depthExplicit)) <$ explicitAdmission
                , ("limits.maxResponseBytes", (cacheExplicit, explicitAdmission, Nothing, requestExplicit, depthExplicit)) <$ responseExplicit
                , ("limits.maxRequestBytes", (cacheExplicit, explicitAdmission, responseExplicit, Nothing, depthExplicit)) <$ requestExplicit
                , ("queue.memoryMaxDepth", (cacheExplicit, explicitAdmission, responseExplicit, requestExplicit, Nothing)) <$ depthExplicit
                ]

        -- Name the pins whose individual removal makes the plan fit; when none
        -- alone flips the verdict (the pins only overshoot in combination), name
        -- them all rather than under-blame.
        culpritPins = case [name | (name, pins) <- explicitPins, overshootAt pins <= 0] of
            [] -> map fst explicitPins
            flips -> flips

        overrideViolations
            | overshoot4 > 0 && overshootWithoutOverrides <= 0 =
                [ "explicit override(s) "
                    <> T.intercalate ", " culpritPins
                    <> " push the combined memory plan "
                    <> show overshoot4
                    <> " bytes past the effective heap ceiling "
                    <> show h
                    <> "; the override-free minimum fits within it, so lower them or raise the ceiling"
                ]
            | otherwise = []

        degradations =
            catMaybes
                [ listToMaybe
                    [ "memory plan: cache aggregate shed from "
                        <> show cacheDesired
                        <> " to "
                        <> show cacheFinal
                        <> " bytes to fit the heap ceiling"
                        <> (if cacheFinal == 0 then " (the proxy serves uncached)" else "")
                    | cacheShed > 0
                    ]
                , listToMaybe
                    [ "memory plan: admission shed to "
                        <> show admissionFinal
                        <> " in-flight operation(s) (the material share cannot hold more at the floor response cap)"
                    | materialShed > 0
                    ]
                , ( \c ->
                        "memory plan: capability count shed to "
                            <> show c
                            <> " (the nursery of "
                            <> show capabilities
                            <> " capabilities x "
                            <> show allocArea
                            <> " bytes allocation area is the memory pressure; fewer, or a smaller GHCRTS -A, fits this pod)"
                  )
                    <$> shedCapabilities
                , listToMaybe
                    [ "memory plan: publish aggregate shed to one maximum request (" <> show publishFinal <> " bytes)"
                    | publishShed > 0
                    ]
                , listToMaybe
                    ["memory plan: memory-queue depth shed to " <> show depthFinal | queueShedBytes > 0]
                , listToMaybe
                    [ "memory plan: the irreducible minimum (one operation on one capability, no cache) still exceeds the heap ceiling by "
                        <> show overshootWithoutOverrides
                        <> " bytes; booting anyway with the container limit as the only backstop -- give this pod more memory"
                    | overshoot4 > 0 && overshootWithoutOverrides > 0
                    ]
                ]

        cacheEntries = case csMaxEntries cacheSettings of
            Just n -> n
            Nothing -> clamp cacheEntriesFloor cacheEntriesCap (cacheFinal `div` cacheEntryExpectedBytes)

        withCeiling name value explicit =
            "memory plan: "
                <> name
                <> " "
                <> show value
                <> ( if explicit
                        then " (from config)"
                        else " (computed from heap ceiling " <> show h <> ", " <> provenanceClause ceilingProvenance <> ")"
                   )
        planLines =
            [ withCeiling "runtime reserve" reserve False
            , cpuAdmissionLine
            , withCeiling "admission capacity" admissionFinal (isJust explicitAdmission)
            , withCeiling "material aggregate" materialFinal False
            , withCeiling "response byte cap" responseFinal (isJust responseExplicit)
            , withCeiling "request byte cap" requestFinal (isJust requestExplicit)
            , withCeiling "cache byte bound" cacheFinal (isJust cacheExplicit)
            , withCeiling "cache entry bound" cacheEntries (isJust (csMaxEntries cacheSettings))
            ]
                <> [withCeiling "publish aggregate" (maybe 0 ptAggregateBytes publishTenant) False | publishConfigured]
                <> [withCeiling "memory-queue depth" depthFinal (isJust depthExplicit) | queueDemand == MemoryQueueTenant]

    clamp lo hi = max lo . min hi

{- | The metadata cache's tunables: the configured TTL married to the plan's cache
aggregate, split into the three stores' named sub-budgets __summing exactly to the
aggregate__ (the assembled share is the remainder). The shares: the full-packument
store carries the decoded working set at 60%; the single-version store holds small
flat entries at 15% but four times the entry count; the assembled store's encoded
documents take the remaining 25%. A fully-shed (zero) aggregate yields stores that
retain nothing: every value takes the oversized pass-through and the proxy serves
uncached.
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

-- The named split of the cache aggregate, in percent; the assembled store takes
-- the remainder so the three sub-budgets sum to exactly the aggregate.
cacheFullSharePercent :: Int
cacheFullSharePercent = 60

cacheVersionSharePercent :: Int
cacheVersionSharePercent = 15

-- The version store's entries are flat and small (16 KiB estimates against the
-- full store's 256 KiB), so it holds several per full entry.
cacheVersionEntriesFactor :: Int
cacheVersionEntriesFactor = 4

-- Tenant shares of the application heap (the ceiling less the reserve), summing
-- to 95% so the computed plan fits by construction; floors and explicit
-- overrides are what the shed ladder and the override check answer for.
cacheSharePercent :: Int
cacheSharePercent = 30

materialSharePercent :: Int
materialSharePercent = 45

publishSharePercent :: Int
publishSharePercent = 15

queueSharePercent :: Int
queueSharePercent = 5

-- The runtime reserve: a fifth of the ceiling, floored so a tiny pod still
-- leaves the GC and the RTS something to breathe with.
runtimeReserveShareDiv :: Int
runtimeReserveShareDiv = 5

runtimeReserveFloorBytes :: Int
runtimeReserveFloorBytes = 33554432

-- The nursery (capabilities x allocation area) may hold at most this share of
-- the ceiling before the capability count itself is the tenant to shed.
nurseryCeilingShareDiv :: Int
nurseryCeilingShareDiv = 4

-- The response-cap floor is the shipped policy value: real-world packuments reach
-- multiple MiB, so a small pod must never compute itself below what is known to
-- admit them. The cap keeps one hostile document from monopolising a huge heap.
responseBytesFloor :: Int
responseBytesFloor = 12582912

responseBytesCap :: Int
responseBytesCap = 67108864

responseBytesFallback :: Int
responseBytesFallback = 12582912

requestBytesFloor :: Int
requestBytesFloor = 26214400

requestBytesCap :: Int
requestBytesCap = 104857600

requestBytesFallback :: Int
requestBytesFallback = 26214400

-- With no ceiling datapoint the publish aggregate falls back to a few maximum
-- requests' worth of concurrent body room.
publishAggregateFallbackRequests :: Int
publishAggregateFallbackRequests = 4

-- A floor so a pod that can afford one still caches a useful working set, a cap
-- because past a gigabyte of decoded metadata the TTL, not memory, is the
-- effective bound. The shed ladder may go below the floor, to zero.
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

queueDepthFloor :: Int
queueDepthFloor = 5000

queueDepthCap :: Int
queueDepthCap = 100000

queueDepthFallback :: Int
queueDepthFallback = 50000
