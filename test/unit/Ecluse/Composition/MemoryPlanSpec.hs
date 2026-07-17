-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MemoryPlanSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.MemoryPlan (
    MemoryPlan (..),
    PublishTenant (ptAggregateBytes),
    QueueTenantDemand (MemoryQueueTenant, MirroringWithoutMemoryQueue, NoQueueTenant),
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

    bareCache :: CacheSettings
    bareCache = CacheSettings{csTtl = 60, csMaxEntries = Nothing, csMaxBytes = Nothing}

    bareLimits :: LimitsSettings
    bareLimits = LimitsSettings{limMaxResponseBytes = Nothing, limMaxVersionCount = 100000, limMaxNestingDepth = 64, limMaxRequestBytes = Nothing}

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
