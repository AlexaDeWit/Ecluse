-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory plan: one solver that partitions the effective heap
ceiling ("Ecluse.Rts") between named tenants whose sum stays within it. In allocation
order the tenants are the runtime reserve, the fixed enqueue buffer, the cache aggregate,
the material aggregate, the publish aggregate, the memory-queue depth, and the
mirror-artifact envelope, one 'MemoryPlan' field each. An explicit config value wins its
own bound, every bound falls back to a shipped default with no ceiling datapoint, and
every decision returns a boot-log line.

== Graceful degradation, never a refusal

A pod too small for the tenants' floors sheds in a fixed order, each step a loud boot
warning: the mirror-artifact cap first, then the cache aggregate, then admission toward
one in-flight operation, then the publish aggregate, then the queue depth. One operation
on one capability with no cache is the irreducible minimum, and it always boots. If even
that exceeds the ceiling, the plan says so and boots anyway, leaving the cgroup backstop
as the guard.

== The one refusal: an explicit override

An override refuses the boot only when it is the cause. The plan re-derives the
override-free minimum, with every pin substituted out and every computed tenant at its
floor, and refuses just when that minimum fits the ceiling while the pinned plan does
not. It names the pins whose individual removal would fit, or all of them when only their
combination overshoots. A pod too small even without the pins boots with the loud warning
like any other.
-}
module Ecluse.Composition.MemoryPlan (
    MemoryPlan (..),
    PublishTenant (..),
    MirrorArtifactTenant (..),
    QueueTenantDemand (..),
    queueTenantDemand,
    resolveMemoryPlan,
    TenantDemands (..),
    OverridePins (..),
    noOverridePins,
    overrideMinShedSum,
    overrideSubstitutions,
    attributeOverrideViolations,
    planCacheConfig,
    mirrorArtifactEnvelopeMultiplier,
    mirrorArtifactBytesCap,
) where

import Data.Text qualified as T

import Ecluse.Composition.MirrorQueue (MirrorQueuePlan (MemoryBackend, SqsBackend), MirrorRuntimePlan (MirrorWith, NoMirroring))
import Ecluse.Composition.Sizing (mirrorEnqueueBufferDepth, renderSized, resolveServeAdmission, resolveSized)
import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (..), StoreBudget (..))
import Ecluse.Core.Server.MemoryModel (contractResidentBytes, expandWireBytes, mirrorJobEstimatedBytes, packumentOriginFanout)
import Ecluse.Rts (EffectiveRuntimePlan (erpAllocAreaBytes), effectiveCapabilities, effectiveHeapCeiling, provenanceClause)

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

{- | The mirror-artifact tenant, present only when some mount mirrors. The plan charges
its heap as 'matMaxBytes' scaled by 'mirrorArtifactEnvelopeMultiplier'.
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
    -- ^ Tenant 3: the one cache aggregate, split at 'planCacheConfig'.
    , mpCacheMaxEntries :: Int
    , mpMaterialAggregateBytes :: Int
    -- ^ Tenant 4: the materialisation envelope bytes admission may hold at once.
    , mpMaxResponseBytes :: Int
    -- ^ The per-response wire cap @R@ carved from the material aggregate.
    , mpMaxRequestBytes :: Int
    -- ^ The per-request (publish body) wire cap @Q@, enforced at the publish read site.
    , mpAdmissionCapacity :: Int
    -- ^ @max 1 (min A_cpu A_mem)@. The composition root builds admission from it.
    , mpShedCapabilities :: Maybe Int
    {- ^ A capability count the composition root shrinks to when the nursery is the memory
    pressure, because each capability holds an allocation area. 'Nothing' leaves the live count.
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
    -- ^ The shed-ladder warnings, in the order taken. Empty when everything fits.
    , mpOverrideViolations :: [Text]
    {- ^ The pins the plan blames for a residual overshoot it cannot shed around, from
    'attributeOverrideViolations'. The boot and check-config refuse on these with exit 2.
    -}
    }
    deriving stock (Eq, Show)

-- Everything resolved before the plan knows whether a heap ceiling exists. The solved
-- path and the fallback path both read it.
data PlanInputs = PlanInputs
    { piCache :: CacheSettings
    , piLimits :: LimitsSettings
    , piQueue :: QueueSettings
    , piExplicitAdmission :: Maybe Int
    , piPublishConfigured :: Bool
    , piQueueDemand :: QueueTenantDemand
    , piCapabilities :: Int
    , piAllocAreaBytes :: Int
    , piCpuAdmission :: Int
    , piCpuAdmissionLine :: Text
    , piCeilingClause :: Text
    }

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

-- One admitted operation's envelope at response cap r: the concurrent origins'
-- wire+parsed forms, by the shared wire-to-resident model.
envelope :: Int -> Int
envelope r = packumentOriginFanout * expandWireBytes r

materialOf :: Int -> Int -> Int
materialOf a r = a * envelope r

-- The bytes a memory-queue depth charges. Zero unless the memory backend runs.
queueCharge :: Bool -> Int -> Int
queueCharge memoryBacked d = if memoryBacked then d * mirrorJobEstimatedBytes else 0

anyMountMirrors :: QueueTenantDemand -> Bool
anyMountMirrors = (/= NoQueueTenant)

memoryQueueCharged :: QueueTenantDemand -> Bool
memoryQueueCharged = (== MemoryQueueTenant)

-- The enqueue hand-off buffer, charged whatever the backend behind it.
fixedBufferBytes :: QueueTenantDemand -> Int
fixedBufferBytes demand
    | anyMountMirrors demand = mirrorEnqueueBufferDepth * mirrorJobEstimatedBytes
    | otherwise = 0

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi = max lo . min hi

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

{- | The desired byte charges and reclaim floors the shed ladder walks, resolved from the
ceiling before any shedding. A pinned ('Just') bound never sheds and answers for its own
overshoot.
-}
data TenantDemands = TenantDemands
    { tdCeiling :: Int
    , tdReserve :: Int
    , tdFixedBuffers :: Int
    , tdPins :: OverridePins
    , tdCacheDesired :: Int
    , tdCacheEntriesExplicit :: Maybe Int
    , tdMaterialDesired :: Int
    , tdMaterialMinimum :: Int
    , tdAdmissionDesired :: Int
    , tdPublishConfigured :: Bool
    , tdPublishDesired :: Int
    , tdRequestFinal :: Int
    , tdRequestComputed :: Int
    , tdDepthDesired :: Int
    , tdMemoryBacked :: Bool
    , tdMirrors :: Bool
    , tdArtifactCapDesired :: Int
    , tdMirrorChargeDesired :: Int
    }

-- Every tenant's desired share over a heap ceiling h, before the shed ladder walks it.
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
-- floor response cap. The response cap is then what that share affords at the admitted
-- concurrency ('contractResidentBytes' inverts the envelope).
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

-- Every tenant's post-shed value plus the residual overshoot the ladder could not
-- reclaim. The combined invariant is a pure function of this record.
data ShedOutcomes = ShedOutcomes
    { soMirrorShed :: Int
    , soArtifactCapFinal :: Int
    , soCacheShed :: Int
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

-- The material tenant after shedding: the shed step plus the admission and response
-- caps the surviving share affords.
data MaterialOutcome = MaterialOutcome
    { moStep :: ShedStep
    , moAdmission :: Int
    , moResponse :: Int
    }

{- Step 2: admission shrinks toward one in-flight operation at the floor response cap. The
surviving material share then fixes both the admission and the response cap. -}
shedMaterialStep :: TenantDemands -> Int -> MaterialOutcome
shedMaterialStep d overshoot =
    MaterialOutcome{moStep = step, moAdmission = admissionFinal, moResponse = responseFinal}
  where
    step = shedStep overshoot (tdMaterialDesired d) (max 0 (tdMaterialDesired d - tdMaterialMinimum d))
    materialFinal = stepFinal step
    responseExplicit = opResponse (tdPins d)
    admissionFinal = case opAdmission (tdPins d) of
        Just n -> n
        Nothing -> max 1 (min (tdAdmissionDesired d) (materialFinal `div` envelope (fromMaybe responseBytesFloor responseExplicit)))
    responseFinal = case responseExplicit of
        Just r -> r
        Nothing -> clamp responseBytesFloor responseBytesCap (contractResidentBytes (materialFinal `div` max 1 (admissionFinal * packumentOriginFanout)))

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

{- Walk the shed ladder: every tenant at its desired share, then shed in step order until
the sum fits or every tenant hits its minimum. The residual is what shedding cannot reclaim. -}
shedToFit :: TenantDemands -> ShedOutcomes
shedToFit d =
    ShedOutcomes
        { soMirrorShed = stepShed mirrorStep
        , soArtifactCapFinal = artifactCapFinal
        , soCacheShed = stepShed cacheStep
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
    mirrorStep = shedMirrorStep d (max 0 (desiredTenantSum d - tdCeiling d))
    -- The surviving cap after shedding: an explicit cap stands, and a computed one is
    -- the shed charge divided back down by the envelope multiplier.
    artifactCapFinal = case opArtifact (tdPins d) of
        Just n -> n
        Nothing -> stepFinal mirrorStep `div` mirrorArtifactEnvelopeMultiplier
    cacheStep = shedCacheStep d (stepResidual mirrorStep)
    material = shedMaterialStep d (stepResidual cacheStep)
    materialStep = moStep material
    publishStep = shedPublishStep d (stepResidual materialStep)
    queue = shedQueueStep d (stepResidual publishStep)

-- Where the nursery (capabilities x allocation area) exceeds a bounded share of the
-- ceiling, shed the capability count so it fits. 'Nothing' keeps the live count.
shedCapabilityCount :: PlanInputs -> Int -> Maybe Int
shedCapabilityCount inputs h
    | piCapabilities inputs * piAllocAreaBytes inputs > nurseryShare =
        let fitted = max 1 (nurseryShare `div` piAllocAreaBytes inputs)
         in if fitted < piCapabilities inputs then Just fitted else Nothing
    | otherwise = Nothing
  where
    nurseryShare = h `div` nurseryCeilingShareDiv

-- The cache entry bound: an explicit count, or the surviving aggregate divided by the
-- expected footprint of one cached packument.
cacheEntryBound :: TenantDemands -> ShedOutcomes -> Int
cacheEntryBound d o =
    fromMaybe (clamp cacheEntriesFloor cacheEntriesCap (soCacheFinal o `div` cacheEntryExpectedBytes)) (tdCacheEntriesExplicit d)

publishTenantOf :: TenantDemands -> ShedOutcomes -> Maybe PublishTenant
publishTenantOf d o = listToMaybe [PublishTenant{ptAggregateBytes = soPublishFinal o} | tdPublishConfigured d]

mirrorArtifactTenantOf :: TenantDemands -> ShedOutcomes -> Maybe MirrorArtifactTenant
mirrorArtifactTenantOf d o = listToMaybe [MirrorArtifactTenant{matMaxBytes = soArtifactCapFinal o} | tdMirrors d]

-- The shed-ladder warnings, in ladder order, each naming what was given up and why.
renderDegradations :: PlanInputs -> TenantDemands -> ShedOutcomes -> Maybe Int -> [Text]
renderDegradations inputs d o shedCaps =
    catMaybes
        [ listToMaybe
            [ shedWarning "mirror artifact byte cap" (tdArtifactCapDesired d) (soArtifactCapFinal o) "this pod mirrors no artifact it cannot buffer safely"
            | soMirrorShed o > 0
            ]
        , listToMaybe
            [ shedWarning "cache aggregate" (tdCacheDesired d) (soCacheFinal o) "the proxy serves uncached"
            | soCacheShed o > 0
            ]
        , listToMaybe
            [ "memory plan: admission shed to "
                <> show (soAdmissionFinal o)
                <> " in-flight operation(s) (the material share cannot hold more at the floor response cap)"
            | soMaterialShed o > 0
            ]
        , capabilityShedWarning inputs <$> shedCaps
        , listToMaybe
            [ "memory plan: publish aggregate shed to one maximum request (" <> show (soPublishFinal o) <> " bytes)"
            | soPublishShed o > 0
            ]
        , listToMaybe
            ["memory plan: memory-queue depth shed to " <> show (soDepthFinal o) | soQueueShedBytes o > 0]
        , listToMaybe
            [irreducibleMinimumWarning freeOvershoot | soResidualOvershoot o > 0 && freeOvershoot > 0]
        ]
  where
    freeOvershoot = overrideFreeOvershoot d

-- A tenant that gave up bytes, with the consequence it carries once it reaches zero.
shedWarning :: Text -> Int -> Int -> Text -> Text
shedWarning tenant desired final atZero =
    "memory plan: "
        <> tenant
        <> " shed from "
        <> show desired
        <> " to "
        <> show final
        <> " bytes to fit the heap ceiling"
        <> (if final == 0 then " (" <> atZero <> ")" else "")

capabilityShedWarning :: PlanInputs -> Int -> Text
capabilityShedWarning inputs shedTo =
    "memory plan: capability count shed to "
        <> show shedTo
        <> " (the nursery of "
        <> show (piCapabilities inputs)
        <> " capabilities x "
        <> show (piAllocAreaBytes inputs)
        <> " bytes allocation area is the memory pressure; fewer, or a smaller GHCRTS -A, fits this pod)"

irreducibleMinimumWarning :: Int -> Text
irreducibleMinimumWarning overshoot =
    "memory plan: the irreducible minimum (one operation on one capability, no cache) still exceeds the heap ceiling by "
        <> show overshoot
        <> " bytes; booting anyway with the container limit as the only backstop -- give this pod more memory"

-- The ordered boot lines check-config prints: one per resolved bound, tagged with its
-- provenance (an explicit config value, or the ceiling it was computed from).
renderPlanLines :: PlanInputs -> TenantDemands -> ShedOutcomes -> [Text]
renderPlanLines inputs d o =
    [ planLine "runtime reserve" (tdReserve d) Nothing
    , piCpuAdmissionLine inputs
    , planLine "admission capacity" (soAdmissionFinal o) (opAdmission pins)
    , planLine "material aggregate" (soMaterialFinal o) Nothing
    , planLine "response byte cap" (soResponseFinal o) (opResponse pins)
    , planLine "request byte cap" (tdRequestFinal d) (opRequest pins)
    , planLine "cache byte bound" (soCacheFinal o) (opCache pins)
    , planLine "cache entry bound" (cacheEntryBound d o) (tdCacheEntriesExplicit d)
    ]
        <> [planLine "publish aggregate" (soPublishFinal o) Nothing | tdPublishConfigured d]
        <> [planLine "memory-queue depth" (soDepthFinal o) (opDepth pins) | tdMemoryBacked d]
        <> [planLine "mirror artifact byte cap" (soArtifactCapFinal o) (opArtifact pins) | tdMirrors d]
  where
    pins = tdPins d
    planLine name value explicit = renderSized ("memory plan: " <> name) value explicit computedClause
    computedClause = "computed from heap ceiling " <> show (tdCeiling d) <> ", " <> piCeilingClause inputs

{- | Explicit overrides, each pinned ('Just') or substituted out ('Nothing'), in the plan's
allocation order.
-}
data OverridePins = OverridePins
    { opCache :: Maybe Int
    , opAdmission :: Maybe Int
    , opResponse :: Maybe Int
    , opRequest :: Maybe Int
    , opDepth :: Maybe Int
    , opArtifact :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | Every override substituted out: the pin set the override-free minimum resolves from.
noOverridePins :: OverridePins
noOverridePins =
    OverridePins
        { opCache = Nothing
        , opAdmission = Nothing
        , opResponse = Nothing
        , opRequest = Nothing
        , opDepth = Nothing
        , opArtifact = Nothing
        }

configuredPins :: PlanInputs -> OverridePins
configuredPins inputs =
    OverridePins
        { opCache = csMaxBytes (piCache inputs)
        , opAdmission = piExplicitAdmission inputs
        , opResponse = limMaxResponseBytes limits
        , opRequest = limMaxRequestBytes limits
        , opDepth = qsMemoryMaxDepth (piQueue inputs)
        , opArtifact = limMaxArtifactBytes limits
        }
  where
    limits = piLimits inputs

{- | The fully-shed minimum tenant sum for a pin set. Each unpinned tenant contributes
the floor the shed ladder reaches. Comparing pin sets attributes an overshoot to its pins.
-}
overrideMinShedSum :: TenantDemands -> OverridePins -> Int
overrideMinShedSum d pins =
    tdReserve d
        + tdFixedBuffers d
        + fromMaybe 0 (opCache pins)
        + materialFloor
        + (if tdPublishConfigured d then fromMaybe (tdRequestComputed d) (opRequest pins) else 0)
        + (if tdMemoryBacked d then fromMaybe queueDepthFloor (opDepth pins) * mirrorJobEstimatedBytes else 0)
        + (if tdMirrors d then maybe 0 (* mirrorArtifactEnvelopeMultiplier) (opArtifact pins) else 0)
  where
    materialFloor = fromMaybe 1 (opAdmission pins) * packumentOriginFanout * expandWireBytes (fromMaybe responseBytesFloor (opResponse pins))

{- | Each explicit override present in the pin set, paired with the pin set that
substitutes only it out, and with the operator's config-key name.
-}
overrideSubstitutions :: OverridePins -> [(Text, OverridePins)]
overrideSubstitutions pins =
    catMaybes
        [ ("cache.maxBytes", pins{opCache = Nothing}) <$ opCache pins
        , ("runtime.serveMaxInFlight", pins{opAdmission = Nothing}) <$ opAdmission pins
        , ("limits.maxResponseBytes", pins{opResponse = Nothing}) <$ opResponse pins
        , ("limits.maxRequestBytes", pins{opRequest = Nothing}) <$ opRequest pins
        , ("queue.memoryMaxDepth", pins{opDepth = Nothing}) <$ opDepth pins
        , ("limits.maxArtifactBytes", pins{opArtifact = Nothing}) <$ opArtifact pins
        ]

-- How far a pin set's fully-shed minimum overshoots the ceiling.
overshootFor :: TenantDemands -> OverridePins -> Int
overshootFor d pins = max 0 (overrideMinShedSum d pins - tdCeiling d)

-- The overshoot with every pin substituted out. Above zero, the pod is too small
-- whatever the operator configured, so no pin is to blame.
overrideFreeOvershoot :: TenantDemands -> Int
overrideFreeOvershoot d = overshootFor d noOverridePins

overrideViolationsFor :: TenantDemands -> ShedOutcomes -> [Text]
overrideViolationsFor d o =
    attributeOverrideViolations
        (tdCeiling d)
        (soResidualOvershoot o)
        (overrideFreeOvershoot d)
        [(name, overshootFor d pins) | (name, pins) <- overrideSubstitutions (tdPins d)]

{- | Decide the override refusal and name the culprits. A pod too small even without the
pins degrades instead of refusing. When no single pin flips the verdict, name them all.
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

-- Tenant shares of the application heap (the ceiling less the reserve), summing to 95%
-- so the computed plan fits by construction. Floors and pins are what the shed ladder answers for.
cacheSharePercent :: Int
cacheSharePercent = 30

materialSharePercent :: Int
materialSharePercent = 45

publishSharePercent :: Int
publishSharePercent = 15

queueSharePercent :: Int
queueSharePercent = 5

-- The mirror-artifact tenant's share of the application heap. It is the charged envelope,
-- kept small so the background back-fill never crowds the serve hot path.
mirrorArtifactSharePercent :: Int
mirrorArtifactSharePercent = 4

{- The transient envelope one mirrored artifact holds, as a multiple of the buffered tarball B.
The tarball, its base64 'Text', and the serialised publish document coexist at ~3.7x B before
GC, rounded up here so the charged tenant never under-provisions the peak. -}
mirrorArtifactEnvelopeMultiplier :: Int
mirrorArtifactEnvelopeMultiplier = 4

-- The ceiling the plan clamps the computed artifact cap to, and the no-ceiling fallback.
-- The charged envelope is therefore at most this times the envelope multiplier.
mirrorArtifactBytesCap :: Int
mirrorArtifactBytesCap = 512 * 1024 * 1024

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

-- Real-world packuments reach multiple MiB, so a small pod must never compute a response
-- cap below this floor. 'responseBytesCap' stops one hostile document monopolising the heap.
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

-- The floor keeps a pod that can afford one caching a useful working set. Past a gigabyte
-- of decoded metadata the TTL bounds the cache, not memory. The shed ladder may still go to zero.
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
