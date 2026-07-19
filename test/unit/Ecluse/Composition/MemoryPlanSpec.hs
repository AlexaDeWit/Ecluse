-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MemoryPlanSpec (spec) where

import Data.Text qualified as T
import Hedgehog (assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Composition.MemoryPlan (
    MemoryPlan (..),
    MirrorArtifactTenant (matMaxBytes),
    PublishTenant (ptAggregateBytes),
    QueueTenantDemand (MemoryQueueTenant, MirroringWithoutMemoryQueue, NoQueueTenant),
    attributeOverrideViolations,
    mirrorArtifactEnvelopeMultiplier,
    overrideMinShedSum,
    overrideSubstitutions,
    planCacheConfig,
    resolveMemoryPlan,
 )
import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (cacheAssembledBudget, cacheFullBudget, cacheTtl, cacheVersionBudget), StoreBudget (sbMaxBytes, sbMaxEntries))
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromCgroup, FromRts))

spec :: Spec
spec = describe "resolveMemoryPlan" $ do
    it "falls back to the shipped bounds with no heap-ceiling datapoint" $ do
        let (plan, lines') = resolve bareCache bareLimits bareQueue Nothing (planWith Nothing) MemoryQueueTenant False
        mpMaxResponseBytes plan `shouldBe` 12582912
        mpMaxRequestBytes plan `shouldBe` 26214400
        mpCacheAggregateBytes plan `shouldBe` 268435456
        mpQueueMemoryMaxDepth plan `shouldBe` 50000
        mpDegradations plan `shouldBe` []
        mpOverrideViolations plan `shouldBe` []
        lines' `shouldSatisfy` any (T.isInfixOf "built-in default; no heap-ceiling datapoint")

    it "partitions a roomy ceiling into tenants whose sum stays within it" $ do
        let h = 4 * gib
            (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just h)) MemoryQueueTenant True
        mpDegradations plan `shouldBe` []
        mpOverrideViolations plan `shouldBe` []
        tenantSum plan `shouldSatisfy` (<= h)
        mpRuntimeReserveBytes plan `shouldSatisfy` (> 0)
        mpAdmissionCapacity plan `shouldSatisfy` (>= 1)
        mpMaxResponseBytes plan `shouldSatisfy` (>= 12582912)

    it "bounds admission jointly by CPU and the material share" $ do
        -- 4 capabilities give a CPU capacity of 40, but a 1 GiB ceiling's
        -- material share holds far fewer 180 MiB envelopes at the floor cap.
        let (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just gib)) NoQueueTenant False
        mpAdmissionCapacity plan `shouldSatisfy` (< 40)
        mpAdmissionCapacity plan `shouldSatisfy` (>= 1)

    it "honours explicit bounds over the computed shares" $ do
        let cache' = bareCache{csMaxBytes = Just 123456789, csMaxEntries = Just 42}
            limits' = bareLimits{limMaxResponseBytes = Just 13000000, limMaxRequestBytes = Just 30000000}
            queue' = bareQueue{qsMemoryMaxDepth = Just 7777}
            (plan, lines') = resolve cache' limits' queue' Nothing (planWith (Just (8 * gib))) MemoryQueueTenant False
        mpCacheAggregateBytes plan `shouldBe` 123456789
        mpCacheMaxEntries plan `shouldBe` 42
        mpMaxResponseBytes plan `shouldBe` 13000000
        mpMaxRequestBytes plan `shouldBe` 30000000
        mpQueueMemoryMaxDepth plan `shouldBe` 7777
        mpOverrideViolations plan `shouldBe` []
        lines' `shouldSatisfy` any (\l -> "cache byte bound 123456789" `T.isInfixOf` l && "from config" `T.isInfixOf` l)

    it "names the ceiling's provenance in each computed boot line" $ do
        let (_, lines') = resolve bareCache bareLimits bareQueue Nothing (planWith (Just gib)) NoQueueTenant False
        lines' `shouldSatisfy` any (T.isInfixOf ("computed from heap ceiling " <> show gib <> ", derived from the cgroup limit"))

    it "charges the queue tenant only under the memory backend" $ do
        let planFor demand = fst (resolve bareCache bareLimits bareQueue Nothing (planWith (Just (2 * gib))) demand False)
        mpQueueTenantBytes (planFor MemoryQueueTenant) `shouldSatisfy` (> 0)
        mpQueueTenantBytes (planFor MirroringWithoutMemoryQueue) `shouldBe` 0
        mpQueueTenantBytes (planFor NoQueueTenant) `shouldBe` 0
        -- The fixed enqueue buffer rides any mirroring, whatever the backend.
        mpFixedBufferBytes (planFor MirroringWithoutMemoryQueue) `shouldSatisfy` (> 0)
        mpFixedBufferBytes (planFor NoQueueTenant) `shouldBe` 0

    it "carries a publish tenant only when a publication target is configured" $ do
        let planFor pub = fst (resolve bareCache bareLimits bareQueue Nothing (planWith (Just (2 * gib))) NoQueueTenant pub)
        (ptAggregateBytes <$> mpPublishTenant (planFor True)) `shouldSatisfy` maybe False (> 0)
        mpPublishTenant (planFor False) `shouldBe` Nothing

    describe "the mirror-artifact tenant (issue #846)" $ do
        it "carries a mirror-artifact tenant exactly when a mount mirrors" $ do
            let planFor demand = fst (resolve bareCache bareLimits bareQueue Nothing (planWith (Just (2 * gib))) demand False)
            (matMaxBytes <$> mpMirrorArtifactTenant (planFor MemoryQueueTenant)) `shouldSatisfy` maybe False (> 0)
            (matMaxBytes <$> mpMirrorArtifactTenant (planFor MirroringWithoutMemoryQueue)) `shouldSatisfy` maybe False (> 0)
            mpMirrorArtifactTenant (planFor NoQueueTenant) `shouldBe` Nothing

        it "charges the transient envelope, not just the tarball (at least the ~3.7x peak)" $
            -- #846: the buffered tarball, its base64 text, and the serialised publish
            -- document coexist at ~3.7x before collection; the tenant must charge at
            -- least that (the combined invariant multiplies the cap by this), never the
            -- bare cap, so the plan does not under-provision the peak.
            mirrorArtifactEnvelopeMultiplier `shouldSatisfy` (>= 4)

        it "sizes the worker cap from the heap share, below the 512 MiB constant on a modest pod" $ do
            -- The pre-fix hard-coded 512 MiB fetch cap is gone: the cap is now a
            -- plan-derived share of the heap, and a 4 GiB pod computes a far smaller one.
            let (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just (4 * gib))) MemoryQueueTenant True
            (matMaxBytes <$> mpMirrorArtifactTenant plan) `shouldBe` Just 34359738
            (matMaxBytes <$> mpMirrorArtifactTenant plan) `shouldSatisfy` maybe False (< 512 * mib)

        it "sheds the mirror-artifact cap on a small mirroring pod, warning loudly" $ do
            -- The background back-fill leg gives way first under memory pressure: the cap
            -- sheds toward zero and the boot log names it.
            let (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just (256 * mib))) MemoryQueueTenant False
            mpDegradations plan `shouldSatisfy` any (T.isInfixOf "mirror artifact byte cap shed")
            (matMaxBytes <$> mpMirrorArtifactTenant plan) `shouldBe` Just 0

        it "refuses an explicit maxArtifactBytes that alone breaks the ceiling" $ do
            -- A 2 GiB explicit artifact cap charges an 8 GiB envelope on a 2 GiB pod; the
            -- override-free plan fits, so the pin is the named cause (parallels #845).
            let limits' = bareLimits{limMaxArtifactBytes = Just (2 * gib)}
                (plan, _) = resolve bareCache limits' bareQueue Nothing (planWith (Just (2 * gib))) MemoryQueueTenant False
            mpOverrideViolations plan `shouldSatisfy` (not . null)
            mpOverrideViolations plan `shouldSatisfy` any (T.isInfixOf "limits.maxArtifactBytes")

    describe "the graceful-degradation ladder" $ do
        it "sheds the cache first on a small pod, warning loudly, and still boots" $ do
            -- 256 MiB: the floors overshoot, the cache gives way (its floor is
            -- 64 MiB), and nothing refuses.
            let (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just (256 * mib))) MemoryQueueTenant False
            mpOverrideViolations plan `shouldBe` []
            mpDegradations plan `shouldSatisfy` (not . null)
            mpDegradations plan `shouldSatisfy` any (T.isInfixOf "cache aggregate shed")
            mpCacheAggregateBytes plan `shouldSatisfy` (< 67108864)

        it "reaches the irreducible minimum on a tiny pod and boots anyway" $ do
            -- 64 MiB cannot hold even one materialisation envelope; the plan says
            -- so at its loudest and boots regardless.
            let (plan, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just (64 * mib))) NoQueueTenant False
            mpOverrideViolations plan `shouldBe` []
            mpAdmissionCapacity plan `shouldBe` 1
            mpCacheAggregateBytes plan `shouldBe` 0
            mpDegradations plan `shouldSatisfy` any (T.isInfixOf "irreducible minimum")

        it "sheds the capability count when the nursery is the pressure" $ do
            -- 8 capabilities x 64 MiB allocation area = a 512 MiB nursery over a
            -- 512 MiB ceiling: the capability count itself is the tenant to shed.
            let runtime = (planWith (Just (512 * mib))){erpCapabilities = enforcedAxis 8, erpAllocAreaBytes = 64 * mib}
                (plan, _) = resolve bareCache bareLimits bareQueue Nothing runtime NoQueueTenant False
            mpShedCapabilities plan `shouldSatisfy` maybe False (< 8)
            mpDegradations plan `shouldSatisfy` any (T.isInfixOf "capability count shed")

        it "refuses only an explicit override that breaks the combined invariant" $ do
            -- A 1 GiB explicit cache on a 256 MiB pod cannot fit however much the
            -- computed tenants shed: the override is refused, named.
            let cache' = bareCache{csMaxBytes = Just (1 * gib)}
                (plan, _) = resolve cache' bareLimits bareQueue Nothing (planWith (Just (256 * mib))) NoQueueTenant False
            mpOverrideViolations plan `shouldSatisfy` (not . null)
            mpOverrideViolations plan `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")

    describe "attributes a residual overshoot before refusing (issue #845)" $ do
        it "boots, never refuses, on an explicit queue depth equal to the floor the ladder would compute" $ do
            -- The traced regression: a 64 MiB pod overshoots and must boot with the
            -- loud warning. Pinning queue.memoryMaxDepth to the very floor the shed
            -- ladder reaches on its own adds no byte, so it must not flip the boot
            -- into an exit-2 refusal that blames a pin contributing nothing.
            let queue' = bareQueue{qsMemoryMaxDepth = Just 5000} -- the queue-depth floor
                (pinned, _) = resolve bareCache bareLimits queue' Nothing (planWith (Just (64 * mib))) MemoryQueueTenant False
                (free, _) = resolve bareCache bareLimits bareQueue Nothing (planWith (Just (64 * mib))) MemoryQueueTenant False
            mpOverrideViolations pinned `shouldBe` []
            mpDegradations pinned `shouldSatisfy` any (T.isInfixOf "irreducible minimum")
            -- The pin moved no tenant byte: the plan matches the override-free one.
            mpQueueMemoryMaxDepth pinned `shouldBe` mpQueueMemoryMaxDepth free
            mpQueueTenantBytes pinned `shouldBe` mpQueueTenantBytes free

        it "never refuses on an explicit queue depth under a non-memory backend (the depth charges no heap)" $ do
            -- Under a durable (or absent) queue backend queueCharge is identically
            -- zero, so queue.memoryMaxDepth contributes to no overshoot whatever its
            -- value; the too-small pod boots with its warning, unblamed.
            let queue' = bareQueue{qsMemoryMaxDepth = Just 100000} -- the cap, deliberately large
                (plan, _) = resolve bareCache bareLimits queue' Nothing (planWith (Just (64 * mib))) MirroringWithoutMemoryQueue False
            mpOverrideViolations plan `shouldBe` []
            mpQueueTenantBytes plan `shouldBe` 0
            mpDegradations plan `shouldSatisfy` any (T.isInfixOf "irreducible minimum")

        it "names only the contributing override when an innocent one is co-present" $ do
            -- A 1 GiB cache on a 256 MiB pod genuinely cannot fit; a queue depth
            -- pinned to the floor beside it contributes nothing and must not be
            -- named, and the message must not claim it pushes the plan past bytes.
            let cache' = bareCache{csMaxBytes = Just (1 * gib)}
                queue' = bareQueue{qsMemoryMaxDepth = Just 5000}
                (plan, _) = resolve cache' bareLimits queue' Nothing (planWith (Just (256 * mib))) MemoryQueueTenant False
            mpOverrideViolations plan `shouldSatisfy` (not . null)
            mpOverrideViolations plan `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            mpOverrideViolations plan `shouldNotSatisfy` any (T.isInfixOf "queue.memoryMaxDepth")

    describe "attributeOverrideViolations (the refusal decision, in isolation)" $ do
        it "refuses on the joint check, naming the single culprit whose removal fits" $ do
            -- The pinned plan overshoots by 40; with all pins out it fits; removing
            -- the one pin fits. It is named, and the message reports only the 40
            -- bytes past the ceiling that the pin is responsible for.
            let violations = attributeOverrideViolations 1000 40 0 [("cache.maxBytes", 0)]
            violations `shouldSatisfy` (not . null)
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldSatisfy` any (T.isInfixOf "40")
            violations `shouldSatisfy` any (T.isInfixOf "override-free minimum fits")
            -- The message no longer claims bytes the pins do not contribute.
            violations `shouldNotSatisfy` any (T.isInfixOf "even after every computed tenant")

        it "names only the pins whose individual removal flips the verdict" $ do
            -- Removing the cache fits (0); removing the depth does not (12 over).
            let violations = attributeOverrideViolations 1000 30 0 [("cache.maxBytes", 0), ("queue.memoryMaxDepth", 12)]
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldNotSatisfy` any (T.isInfixOf "queue.memoryMaxDepth")

        it "names all pins when none alone flips the verdict (they overshoot only jointly)" $ do
            -- The override-free minimum fits, but neither single removal does (each
            -- leaves 10 over): the pins overshoot only together, so blame both.
            let violations = attributeOverrideViolations 1000 50 0 [("cache.maxBytes", 10), ("queue.memoryMaxDepth", 10)]
            violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
            violations `shouldSatisfy` any (T.isInfixOf "queue.memoryMaxDepth")

        it "does not refuse when the override-free minimum also overshoots (the pod is too small)" $
            -- freeOvershoot > 0: the shed ladder's degradation owns this, not a refusal.
            attributeOverrideViolations 1000 60 25 [("cache.maxBytes", 40)] `shouldBe` []

        it "does not refuse when the pinned plan already fits" $
            attributeOverrideViolations 1000 0 0 [("cache.maxBytes", 0)] `shouldBe` []

    describe "overrideMinShedSum (the substitution arithmetic, in isolation)" $ do
        it "charges nothing extra for a queue depth pinned to the floor the ladder would compute" $ do
            -- queue.memoryMaxDepth at the queue-depth floor (5000) is zero-delta.
            let base = 100000000
                atFloor = overrideMinShedSum base 26214400 False True False (Nothing, Nothing, Nothing, Nothing, Just 5000, Nothing)
                unpinned = overrideMinShedSum base 26214400 False True False (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            atFloor `shouldBe` unpinned

        it "charges nothing for a queue depth under a non-memory backend (queueCharge is zero)" $ do
            -- With no memory-backed queue the depth pin cannot move a byte, however large.
            let base = 100000000
                huge = overrideMinShedSum base 26214400 False False False (Nothing, Nothing, Nothing, Nothing, Just 100000, Nothing)
                unpinned = overrideMinShedSum base 26214400 False False False (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            huge `shouldBe` unpinned

        it "charges an explicit cache its full value where a computed cache sheds to zero" $ do
            -- cacheReclaimable is zero for an explicit cache: it adds its whole value.
            let base = 100000000
                withCache = overrideMinShedSum base 26214400 False False False (Just 12345678, Nothing, Nothing, Nothing, Nothing, Nothing)
                without = overrideMinShedSum base 26214400 False False False (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            withCache - without `shouldBe` 12345678

        it "charges an explicit artifact cap its envelope (cap x multiplier) when mirroring, nothing otherwise" $ do
            -- An explicit limits.maxArtifactBytes never sheds; it adds cap x 4 to the
            -- minimum when mirroring, and nothing at all when no mount mirrors.
            let base = 100000000
                mirroring = overrideMinShedSum base 26214400 False False True (Nothing, Nothing, Nothing, Nothing, Nothing, Just 10000000)
                notMirroring = overrideMinShedSum base 26214400 False False False (Nothing, Nothing, Nothing, Nothing, Nothing, Just 10000000)
                unpinned = overrideMinShedSum base 26214400 False False True (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            mirroring - unpinned `shouldBe` 40000000
            notMirroring `shouldBe` unpinned

    describe "overrideSubstitutions (the per-pin substitution)" $ do
        it "pairs each present override with the pin set that substitutes only it out" $
            overrideSubstitutions (Just 1, Just 2, Just 3, Just 4, Just 5, Just 6)
                `shouldBe` [ ("cache.maxBytes", (Nothing, Just 2, Just 3, Just 4, Just 5, Just 6))
                           , ("runtime.serveMaxInFlight", (Just 1, Nothing, Just 3, Just 4, Just 5, Just 6))
                           , ("limits.maxResponseBytes", (Just 1, Just 2, Nothing, Just 4, Just 5, Just 6))
                           , ("limits.maxRequestBytes", (Just 1, Just 2, Just 3, Nothing, Just 5, Just 6))
                           , ("queue.memoryMaxDepth", (Just 1, Just 2, Just 3, Just 4, Nothing, Just 6))
                           , ("limits.maxArtifactBytes", (Just 1, Just 2, Just 3, Just 4, Just 5, Nothing))
                           ]

        it "yields no substitution for an absent override" $
            map fst (overrideSubstitutions (Nothing, Just 2, Nothing, Nothing, Nothing, Nothing)) `shouldBe` ["runtime.serveMaxInFlight"]

    it "renders the whole plan block for a pinned pod (the check-config golden)" $ do
        -- 1 GiB, 4 capabilities, memory queue, publishing: the ordered lines
        -- check-config prints, pinned so any operator-visible change is reviewed.
        let (_, lines') = resolve bareCache bareLimits bareQueue Nothing (planWith (Just gib)) MemoryQueueTenant True
            ceilingClause = " (computed from heap ceiling " <> show gib <> ", derived from the cgroup limit)"
        lines'
            `shouldBe` [ "memory plan: runtime reserve 214748364" <> ceilingClause
                       , "runtime: serve admission 40 (computed from 4 capabilities)"
                       , "memory plan: admission capacity 2" <> ceilingClause
                       , "memory plan: material aggregate 386547028" <> ceilingClause
                       , "memory plan: response byte cap 12884900" <> ceilingClause
                       , "memory plan: request byte cap 104857600" <> ceilingClause
                       , "memory plan: cache byte bound 257698038" <> ceilingClause
                       , "memory plan: cache entry bound 983" <> ceilingClause
                       , "memory plan: publish aggregate 128849019" <> ceilingClause
                       , "memory plan: memory-queue depth 41943" <> ceilingClause
                       , "memory plan: mirror artifact byte cap 8589934" <> ceilingClause
                       ]

    describe "the combined invariant (property)" $
        it "holds after the solver for every computed pod, or the plan names the irreducible overshoot" $
            hedgehog $ do
                h <- forAll (Gen.int (Range.linear (128 * mib) (64 * gib)))
                caps <- forAll (Gen.int (Range.linear 1 64))
                demand <- forAll (Gen.element [NoQueueTenant, MirroringWithoutMemoryQueue, MemoryQueueTenant])
                pub <- forAll Gen.bool
                let runtime = (planWith (Just h)){erpCapabilities = enforcedAxis caps}
                    (plan, _) = resolveMemoryPlan bareCache bareLimits bareQueue Nothing runtime demand pub
                -- Without explicit overrides the plan never refuses, always admits at
                -- least one operation, and either fits the ceiling or says at its
                -- loudest that even the irreducible minimum exceeds it.
                mpOverrideViolations plan === []
                assert (mpAdmissionCapacity plan >= 1)
                assert
                    ( tenantSum plan <= h
                        || any (T.isInfixOf "irreducible minimum") (mpDegradations plan)
                    )
                -- The ladder's order: admission never sheds before the cache gave way.
                when (any (T.isInfixOf "admission shed") (mpDegradations plan)) $
                    assert (any (T.isInfixOf "cache aggregate shed") (mpDegradations plan))

    describe "planCacheConfig" $ do
        it "marries the TTL to the plan's aggregate, split summing exactly to it" $ do
            let (plan, _) = resolve bareCache{csTtl = 45} bareLimits bareQueue Nothing (planWith (Just (2 * gib))) NoQueueTenant False
                cacheCfg = planCacheConfig bareCache{csTtl = 45} plan
            cacheTtl cacheCfg `shouldBe` 45
            sbMaxBytes (cacheFullBudget cacheCfg)
                + sbMaxBytes (cacheVersionBudget cacheCfg)
                + sbMaxBytes (cacheAssembledBudget cacheCfg)
                `shouldBe` mpCacheAggregateBytes plan
            sbMaxEntries (cacheFullBudget cacheCfg) `shouldBe` mpCacheMaxEntries plan
            sbMaxEntries (cacheVersionBudget cacheCfg) `shouldBe` 4 * mpCacheMaxEntries plan
  where
    resolve = resolveMemoryPlan

    -- Every tenant the combined invariant sums.
    tenantSum :: MemoryPlan -> Int
    tenantSum plan =
        mpRuntimeReserveBytes plan
            + mpCacheAggregateBytes plan
            + mpMaterialAggregateBytes plan
            + maybe 0 ptAggregateBytes (mpPublishTenant plan)
            + mpQueueTenantBytes plan
            + mpFixedBufferBytes plan
            + maybe 0 ((* mirrorArtifactEnvelopeMultiplier) . matMaxBytes) (mpMirrorArtifactTenant plan)

    bareCache :: CacheSettings
    bareCache = CacheSettings{csTtl = 60, csMaxEntries = Nothing, csMaxBytes = Nothing}

    bareLimits :: LimitsSettings
    bareLimits = LimitsSettings{limMaxResponseBytes = Nothing, limMaxVersionCount = 100000, limMaxNestingDepth = 64, limMaxRequestBytes = Nothing, limMaxArtifactBytes = Nothing}

    bareQueue :: QueueSettings
    bareQueue = QueueSettings{qsUrl = Nothing, qsMemoryMaxDepth = Nothing}

    enforcedAxis :: Int -> EffectiveAxis Int
    enforcedAxis n = EffectiveAxis{axDesired = n, axObserved = n, axProvenance = FromRts}

    -- A resolved posture whose ceiling came from the cgroup, the common container case.
    planWith :: Maybe Int -> EffectiveRuntimePlan
    planWith ceiling' =
        EffectiveRuntimePlan
            { erpCapabilities = enforcedAxis 4
            , erpMaxHeapBytes = EffectiveAxis{axDesired = ceiling', axObserved = ceiling', axProvenance = FromCgroup}
            , erpAllocAreaBytes = 4 * mib
            , erpNurseryChunkBytes = Nothing
            , erpContainerMemoryBytes = Nothing
            }

    mib :: Int
    mib = 1024 * 1024

    gib :: Int
    gib = 1024 * mib
