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
    OverridePins,
    overrideMinShedSum,
    overrideSubstitutions,
    attributeOverrideViolations,
    planCacheConfig,
) where

import Data.Text qualified as T

import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), MirrorRuntimePlan (MirrorWith, NoMirroring))
import Ecluse.Composition.Sizing (mirrorEnqueueBufferDepth, resolveServeAdmission)
import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
import Ecluse.Core.Server.MemoryModel (contractResidentBytes, expandWireBytes, mirrorJobEstimatedBytes, packumentOriginFanout)
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
    -- ^ The per-request (publish body) wire cap @Q@, enforced at the publish read site.
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
    maybe
        (fallbackPlan cacheSettings limitsSettings queueSettings cpuAdmission cpuAdmissionLine queueDemand publishConfigured fixedBuffers)
        solvedPlan
        heapCeiling
  where
    (heapCeiling, ceilingProvenance) = effectiveHeapCeiling runtime
    (capabilities, _) = effectiveCapabilities runtime

    -- The CPU-derived admission capacity (explicit serveMaxInFlight wins inside).
    (cpuAdmission, cpuAdmissionLine) = resolveServeAdmission explicitAdmission capabilities

    fixedBuffers = case queueDemand of
        NoQueueTenant -> 0
        _ -> mirrorEnqueueBufferDepth * mirrorJobEstimatedBytes

    memoryBacked = queueDemand == MemoryQueueTenant

    {- The solved plan over a ceiling h: resolve each tenant's desired share, shed to
    fit ('shedToFit'), then render the boot log from the outcomes. The arithmetic (the
    demands, the ladder) is kept apart from the prose so the sum-within-ceiling
    invariant reads without stepping through boot-log text. -}
    solvedPlan :: Int -> (MemoryPlan, [Text])
    solvedPlan h =
        ( MemoryPlan
            { mpRuntimeReserveBytes = reserve
            , mpCacheAggregateBytes = soCacheFinal outcomes
            , mpCacheMaxEntries = cacheEntries
            , mpMaterialAggregateBytes = soMaterialFinal outcomes
            , mpMaxResponseBytes = soResponseFinal outcomes
            , mpMaxRequestBytes = requestFinal
            , mpAdmissionCapacity = soAdmissionFinal outcomes
            , mpShedCapabilities = shedCaps
            , mpPublishTenant = publishTenant
            , mpQueueMemoryMaxDepth = soDepthFinal outcomes
            , mpQueueTenantBytes = soQueueTenantBytes outcomes
            , mpFixedBufferBytes = fixedBuffers
            , mpDegradations = renderDegradations demands outcomes capabilities allocArea shedCaps overshootWithoutOverrides
            , mpOverrideViolations = overrideViolations
            }
        , renderPlanLines demands outcomes ceilingClause cpuAdmissionLine cacheEntries (isJust (csMaxEntries cacheSettings)) publishTenant
        )
      where
        reserve = max runtimeReserveFloorBytes (h `div` runtimeReserveShareDiv)
        appHeap = max 0 (h - reserve)

        -- Each shedable tenant's desired share, remembering which were pinned by an
        -- explicit config value (an explicit bound never sheds; a violation attributes
        -- to it).
        cacheExplicit = csMaxBytes cacheSettings
        cacheDesired = fromMaybe (clamp cacheBytesFloor cacheBytesCap (appHeap * cacheSharePercent `div` 100)) cacheExplicit

        requestExplicit = limMaxRequestBytes limitsSettings
        computedRequestDefault = clamp requestBytesFloor requestBytesCap (appHeap * publishSharePercent `div` 100)
        requestFinal = fromMaybe computedRequestDefault requestExplicit
        publishDesired = max requestFinal (appHeap * publishSharePercent `div` 100)

        responseExplicit = limMaxResponseBytes limitsSettings
        materialShareBytes = appHeap * materialSharePercent `div` 100

        -- Admission bounded jointly: the CPU capacity and what the material share
        -- holds at the floor response cap. An explicit serveMaxInFlight is a pinned
        -- claim (cpuAdmission already carries it).
        admissionMemBound = max 1 (materialShareBytes `div` envelope responseBytesFloor)
        admissionDesired = case explicitAdmission of
            Just n -> n
            Nothing -> max 1 (min cpuAdmission admissionMemBound)

        -- The response cap the material share affords at the desired admission, by the
        -- shared wire-to-resident ratio ('contractResidentBytes' inverts the envelope).
        responseDesired =
            fromMaybe
                (clamp responseBytesFloor responseBytesCap (contractResidentBytes (materialShareBytes `div` max 1 (admissionDesired * packumentOriginFanout))))
                responseExplicit
        materialDesired = materialOf admissionDesired responseDesired
        materialMinimum = materialOf (fromMaybe 1 explicitAdmission) (fromMaybe responseBytesFloor responseExplicit)

        depthExplicit = qsMemoryMaxDepth queueSettings
        depthDesired = fromMaybe (clamp queueDepthFloor queueDepthCap ((appHeap * queueSharePercent `div` 100) `div` mirrorJobEstimatedBytes)) depthExplicit

        demands =
            TenantDemands
                { tdCeiling = h
                , tdReserve = reserve
                , tdFixedBuffers = fixedBuffers
                , tdCacheDesired = cacheDesired
                , tdCacheExplicit = cacheExplicit
                , tdMaterialDesired = materialDesired
                , tdMaterialMinimum = materialMinimum
                , tdAdmissionDesired = admissionDesired
                , tdAdmissionExplicit = explicitAdmission
                , tdResponseExplicit = responseExplicit
                , tdPublishConfigured = publishConfigured
                , tdPublishDesired = publishDesired
                , tdRequestFinal = requestFinal
                , tdRequestExplicit = requestExplicit
                , tdDepthDesired = depthDesired
                , tdDepthExplicit = depthExplicit
                , tdMemoryBacked = memoryBacked
                }
        outcomes = shedToFit demands

        -- The nursery (capabilities x allocation area) lives outside the heap ceiling,
        -- so the tenant sum cannot see it; the capability count sheds on its own.
        allocArea = max 1 (erpAllocAreaBytes runtime)
        shedCaps = shedCapabilityCount h capabilities allocArea

        publishTenant = listToMaybe [PublishTenant{ptAggregateBytes = soPublishFinal outcomes} | publishConfigured]
        cacheEntries = case csMaxEntries cacheSettings of
            Just n -> n
            Nothing -> clamp cacheEntriesFloor cacheEntriesCap (soCacheFinal outcomes `div` cacheEntryExpectedBytes)

        ceilingClause = provenanceClause ceilingProvenance

        -- Attribute a residual overshoot to the pins that cause it before refusing:
        -- substitute the pins out (all at once for the override-free minimum, then one
        -- at a time) and let 'attributeOverrideViolations' name the culprits.
        actualPins = (cacheExplicit, explicitAdmission, responseExplicit, requestExplicit, depthExplicit)
        overshootFor pins =
            max 0 (overrideMinShedSum (reserve + fixedBuffers) computedRequestDefault publishConfigured memoryBacked pins - h)
        overshootWithoutOverrides = overshootFor (Nothing, Nothing, Nothing, Nothing, Nothing)
        overrideViolations =
            attributeOverrideViolations
                h
                (soResidualOvershoot outcomes)
                overshootWithoutOverrides
                [(name, overshootFor pins) | (name, pins) <- overrideSubstitutions actualPins]

-- One admitted operation's envelope at response cap r: the concurrent origins'
-- wire+parsed forms, by the shared wire-to-resident model.
envelope :: Int -> Int
envelope r = packumentOriginFanout * expandWireBytes r

materialOf :: Int -> Int -> Int
materialOf a r = a * envelope r

-- The bytes a memory-queue depth charges; nil unless the memory backend runs.
queueCharge :: Bool -> Int -> Int
queueCharge memoryBacked d = if memoryBacked then d * mirrorJobEstimatedBytes else 0

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi = max lo . min hi

{- No ceiling datapoint: the shipped fallbacks that predate the plan, admission from
CPU alone, and no tenant arithmetic to check (there is nothing to sum against). -}
fallbackPlan ::
    CacheSettings ->
    LimitsSettings ->
    QueueSettings ->
    Int ->
    Text ->
    QueueTenantDemand ->
    Bool ->
    Int ->
    (MemoryPlan, [Text])
fallbackPlan cacheSettings limitsSettings queueSettings cpuAdmission cpuAdmissionLine queueDemand publishConfigured fixedBuffers =
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
        , mpQueueTenantBytes = queueCharge (queueDemand == MemoryQueueTenant) queueDepth
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

-- The desired byte charges and reclaim floors the shed ladder walks, resolved from
-- the ceiling before any shedding. An explicit ('Just') bound never sheds.
data TenantDemands = TenantDemands
    { tdCeiling :: Int
    , tdReserve :: Int
    , tdFixedBuffers :: Int
    , tdCacheDesired :: Int
    , tdCacheExplicit :: Maybe Int
    , tdMaterialDesired :: Int
    , tdMaterialMinimum :: Int
    , tdAdmissionDesired :: Int
    , tdAdmissionExplicit :: Maybe Int
    , tdResponseExplicit :: Maybe Int
    , tdPublishConfigured :: Bool
    , tdPublishDesired :: Int
    , tdRequestFinal :: Int
    , tdRequestExplicit :: Maybe Int
    , tdDepthDesired :: Int
    , tdDepthExplicit :: Maybe Int
    , tdMemoryBacked :: Bool
    }

-- Every tenant's post-shed value plus the residual overshoot the ladder could not
-- reclaim. The combined invariant is a pure function of this record.
data ShedOutcomes = ShedOutcomes
    { soCacheShed :: Int
    , soCacheFinal :: Int
    , soMaterialShed :: Int
    , soMaterialFinal :: Int
    , soAdmissionFinal :: Int
    , soResponseFinal :: Int
    , soPublishShed :: Int
    , soPublishFinal :: Int
    , soQueueShedBytes :: Int
    , soDepthFinal :: Int
    , soQueueTenantBytes :: Int
    , soResidualOvershoot :: Int
    }

-- One shed-ladder step: give up as much of a tenant's reclaimable bytes as the
-- residual overshoot demands. 'stepFinal' is the value after shedding, 'stepResidual'
-- the overshoot the next step inherits.
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

-- Step 1: the cache gives way first, to zero if needed (never an explicit one).
shedCacheStep :: Int -> Int -> Maybe Int -> ShedStep
shedCacheStep overshoot cacheDesired cacheExplicit =
    shedStep overshoot cacheDesired (if isJust cacheExplicit then 0 else cacheDesired)

-- The material tenant after shedding: the shed step plus the admission and response
-- caps the surviving share affords.
data MaterialOutcome = MaterialOutcome
    { moStep :: ShedStep
    , moAdmission :: Int
    , moResponse :: Int
    }

{- Step 2: admission shrinks toward one in-flight operation at the floor response cap
(never an explicit one); the surviving material share then fixes the admission and the
response cap, the latter by the shared wire-to-resident ratio. -}
shedMaterialStep :: Int -> Int -> Int -> Int -> Maybe Int -> Maybe Int -> MaterialOutcome
shedMaterialStep overshoot materialDesired materialMinimum admissionDesired explicitAdmission responseExplicit =
    MaterialOutcome{moStep = step, moAdmission = admissionFinal, moResponse = responseFinal}
  where
    step = shedStep overshoot materialDesired (max 0 (materialDesired - materialMinimum))
    materialFinal = stepFinal step
    admissionFinal = case explicitAdmission of
        Just n -> n
        Nothing -> max 1 (min admissionDesired (materialFinal `div` envelope (fromMaybe responseBytesFloor responseExplicit)))
    responseFinal = case responseExplicit of
        Just r -> r
        Nothing -> clamp responseBytesFloor responseBytesCap (contractResidentBytes (materialFinal `div` max 1 (admissionFinal * packumentOriginFanout)))

-- Step 3: the publish aggregate shrinks to one maximum request.
shedPublishStep :: Int -> Bool -> Int -> Int -> ShedStep
shedPublishStep overshoot publishConfigured publishDesired requestFloor =
    shedStep overshoot publishDesired (if publishConfigured then max 0 (publishDesired - requestFloor) else 0)

-- The queue tenant after shedding: bytes shed, the depth cap (in jobs), and the bytes
-- that depth charges.
data QueueOutcome = QueueOutcome
    { qoShed :: Int
    , qoDepthFinal :: Int
    , qoTenantBytes :: Int
    , qoResidual :: Int
    }

-- Step 4: the memory-queue depth to its floor (never an explicit one).
shedQueueStep :: Int -> Bool -> Int -> Maybe Int -> QueueOutcome
shedQueueStep overshoot memoryBacked depthDesired depthExplicit =
    QueueOutcome
        { qoShed = queueShedBytes
        , qoDepthFinal = depthFinal
        , qoTenantBytes = charge depthFinal
        , qoResidual = overshoot - queueShedBytes
        }
  where
    charge = queueCharge memoryBacked
    depthReclaimableBytes = case depthExplicit of
        Just _ -> 0
        Nothing -> max 0 (charge depthDesired - charge queueDepthFloor)
    queueShedBytes = min overshoot depthReclaimableBytes
    depthFinal = case depthExplicit of
        Just n -> n
        Nothing
            | queueShedBytes > 0 -> max queueDepthFloor ((charge depthDesired - queueShedBytes) `div` mirrorJobEstimatedBytes)
            | otherwise -> depthDesired

{- Walk the shed ladder over the resolved demands: allocate every tenant at its desired
share, then shed in priority order (cache, then admission/material, then publish, then
queue depth) until the sum fits or every computed tenant is at its minimum. The residual
overshoot is what even the fully-shed plan cannot reclaim. -}
shedToFit :: TenantDemands -> ShedOutcomes
shedToFit d =
    ShedOutcomes
        { soCacheShed = stepShed cacheStep
        , soCacheFinal = stepFinal cacheStep
        , soMaterialShed = stepShed materialStep
        , soMaterialFinal = stepFinal materialStep
        , soAdmissionFinal = moAdmission material
        , soResponseFinal = moResponse material
        , soPublishShed = stepShed publishStep
        , soPublishFinal = stepFinal publishStep
        , soQueueShedBytes = qoShed queue
        , soDepthFinal = qoDepthFinal queue
        , soQueueTenantBytes = qoTenantBytes queue
        , soResidualOvershoot = qoResidual queue
        }
  where
    publishCharge = if tdPublishConfigured d then tdPublishDesired d else 0
    desiredSum =
        tdReserve d
            + tdFixedBuffers d
            + tdCacheDesired d
            + tdMaterialDesired d
            + publishCharge
            + queueCharge (tdMemoryBacked d) (tdDepthDesired d)
    overshoot0 = max 0 (desiredSum - tdCeiling d)

    cacheStep = shedCacheStep overshoot0 (tdCacheDesired d) (tdCacheExplicit d)
    material =
        shedMaterialStep
            (stepResidual cacheStep)
            (tdMaterialDesired d)
            (tdMaterialMinimum d)
            (tdAdmissionDesired d)
            (tdAdmissionExplicit d)
            (tdResponseExplicit d)
    materialStep = moStep material
    publishStep = shedPublishStep (stepResidual materialStep) (tdPublishConfigured d) (tdPublishDesired d) (tdRequestFinal d)
    queue = shedQueueStep (stepResidual publishStep) (tdMemoryBacked d) (tdDepthDesired d) (tdDepthExplicit d)

-- Where the nursery (capabilities x allocation area) exceeds a bounded share of the
-- ceiling, shed the capability count so it fits; 'Nothing' keeps the live count.
shedCapabilityCount :: Int -> Int -> Int -> Maybe Int
shedCapabilityCount h capabilities allocArea
    | capabilities * allocArea > h `div` nurseryCeilingShareDiv =
        let fitted = max 1 ((h `div` nurseryCeilingShareDiv) `div` allocArea)
         in if fitted < capabilities then Just fitted else Nothing
    | otherwise = Nothing

-- The shed-ladder warnings, in ladder order, each naming what was given up and why: a
-- pure function of the outcomes and the nursery and irreducible-overshoot context.
renderDegradations :: TenantDemands -> ShedOutcomes -> Int -> Int -> Maybe Int -> Int -> [Text]
renderDegradations d o capabilities allocArea shedCaps overshootWithoutOverrides =
    catMaybes
        [ listToMaybe
            [ "memory plan: cache aggregate shed from "
                <> show (tdCacheDesired d)
                <> " to "
                <> show (soCacheFinal o)
                <> " bytes to fit the heap ceiling"
                <> (if soCacheFinal o == 0 then " (the proxy serves uncached)" else "")
            | soCacheShed o > 0
            ]
        , listToMaybe
            [ "memory plan: admission shed to "
                <> show (soAdmissionFinal o)
                <> " in-flight operation(s) (the material share cannot hold more at the floor response cap)"
            | soMaterialShed o > 0
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
            <$> shedCaps
        , listToMaybe
            [ "memory plan: publish aggregate shed to one maximum request (" <> show (soPublishFinal o) <> " bytes)"
            | soPublishShed o > 0
            ]
        , listToMaybe
            ["memory plan: memory-queue depth shed to " <> show (soDepthFinal o) | soQueueShedBytes o > 0]
        , listToMaybe
            [ "memory plan: the irreducible minimum (one operation on one capability, no cache) still exceeds the heap ceiling by "
                <> show overshootWithoutOverrides
                <> " bytes; booting anyway with the container limit as the only backstop -- give this pod more memory"
            | soResidualOvershoot o > 0 && overshootWithoutOverrides > 0
            ]
        ]

-- The ordered boot lines check-config prints: one per resolved bound, tagged with its
-- provenance (an explicit config value, or the ceiling it was computed from).
renderPlanLines :: TenantDemands -> ShedOutcomes -> Text -> Text -> Int -> Bool -> Maybe PublishTenant -> [Text]
renderPlanLines d o ceilingClause cpuAdmissionLine cacheEntries entriesExplicit publishTenant =
    [ withCeiling "runtime reserve" (tdReserve d) False
    , cpuAdmissionLine
    , withCeiling "admission capacity" (soAdmissionFinal o) (isJust (tdAdmissionExplicit d))
    , withCeiling "material aggregate" (soMaterialFinal o) False
    , withCeiling "response byte cap" (soResponseFinal o) (isJust (tdResponseExplicit d))
    , withCeiling "request byte cap" (tdRequestFinal d) (isJust (tdRequestExplicit d))
    , withCeiling "cache byte bound" (soCacheFinal o) (isJust (tdCacheExplicit d))
    , withCeiling "cache entry bound" cacheEntries entriesExplicit
    ]
        <> [withCeiling "publish aggregate" (maybe 0 ptAggregateBytes publishTenant) False | tdPublishConfigured d]
        <> [withCeiling "memory-queue depth" (soDepthFinal o) (isJust (tdDepthExplicit d)) | tdMemoryBacked d]
  where
    withCeiling name value explicit =
        "memory plan: "
            <> name
            <> " "
            <> show value
            <> ( if explicit
                    then " (from config)"
                    else " (computed from heap ceiling " <> show (tdCeiling d) <> ", " <> ceilingClause <> ")"
               )

{- | A hypothetical set of explicit overrides for the memory plan, in allocation
order: the cache byte bound, the serve admission, the response byte cap, the request
byte cap, and the memory-queue depth, each pinned ('Just') or substituted out
('Nothing').
-}
type OverridePins = (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)

{- | The fully-shed minimum tenant sum for a hypothetical set of explicit pins: the
pin-independent @base@ (runtime reserve plus fixed buffers) plus each shedable tenant
at its pinned value, or un-pinned at the floor the shed ladder would reach -- cache
to zero, admission to one operation, response to its floor, the publish aggregate to
the computed one-request floor, and the queue depth to its floor. @memoryBacked@
gates the queue tenant: a durable or absent backend spends no heap on depth, whatever
its value. Comparing this across pin sets attributes a residual overshoot to the pins
that cause it.
-}
overrideMinShedSum :: Int -> Int -> Bool -> Bool -> OverridePins -> Int
overrideMinShedSum base computedRequestFloor publishPresent memoryBacked (pinCache, pinAdmission, pinResponse, pinRequest, pinDepth) =
    base
        + fromMaybe 0 pinCache
        + materialFloor
        + (if publishPresent then fromMaybe computedRequestFloor pinRequest else 0)
        + (if memoryBacked then fromMaybe queueDepthFloor pinDepth * mirrorJobEstimatedBytes else 0)
  where
    materialFloor = fromMaybe 1 pinAdmission * packumentOriginFanout * expandWireBytes (fromMaybe responseBytesFloor pinResponse)

{- | Each explicit override present in the pin set, paired with the pin set that
substitutes only it out (the value the shed ladder would reach without it) and tagged
with the operator's config-key name, in the plan's allocation order. An absent
override contributes no substitution.
-}
overrideSubstitutions :: OverridePins -> [(Text, OverridePins)]
overrideSubstitutions (pinCache, pinAdmission, pinResponse, pinRequest, pinDepth) =
    catMaybes
        [ ("cache.maxBytes", (Nothing, pinAdmission, pinResponse, pinRequest, pinDepth)) <$ pinCache
        , ("runtime.serveMaxInFlight", (pinCache, Nothing, pinResponse, pinRequest, pinDepth)) <$ pinAdmission
        , ("limits.maxResponseBytes", (pinCache, pinAdmission, Nothing, pinRequest, pinDepth)) <$ pinResponse
        , ("limits.maxRequestBytes", (pinCache, pinAdmission, pinResponse, Nothing, pinDepth)) <$ pinRequest
        , ("queue.memoryMaxDepth", (pinCache, pinAdmission, pinResponse, pinRequest, Nothing)) <$ pinDepth
        ]

{- | Decide the override refusal from the residual overshoots, and name the culprits.
Refuse only when the override-free minimum fits the heap ceiling (@freeOvershoot@ is
zero) while the pinned plan does not (@overriddenOvershoot@ is positive): the pins are
then the cause, and a pod too small even without them is a degradation the shed
ladder already warned about, never a refusal. Name the pins whose individual removal
makes the plan fit (their one-out overshoot is zero); when none alone flips the
verdict (the pins only overshoot in combination), name them all rather than
under-blame. The message reports only the overshoot the named pins are responsible
for, since the override-free minimum fits within the ceiling.
-}
attributeOverrideViolations :: Int -> Int -> Int -> [(Text, Int)] -> [Text]
attributeOverrideViolations heapCeiling overriddenOvershoot freeOvershoot perOverrideOvershoot
    | overriddenOvershoot > 0 && freeOvershoot <= 0 =
        [ "explicit override(s) "
            <> T.intercalate ", " culprits
            <> " push the combined memory plan "
            <> show overriddenOvershoot
            <> " bytes past the effective heap ceiling "
            <> show heapCeiling
            <> "; the override-free minimum fits within it, so lower them or raise the ceiling"
        ]
    | otherwise = []
  where
    culprits = case [name | (name, o) <- perOverrideOvershoot, o <= 0] of
        [] -> map fst perOverrideOvershoot
        flips -> flips

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
