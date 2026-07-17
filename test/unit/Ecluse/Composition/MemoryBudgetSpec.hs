-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MemoryBudgetSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.MemoryBudget (MemoryBudget (..), budgetCacheConfig, resolveMemoryBudget)
import Ecluse.Config (CacheSettings (..), LimitsSettings (..), QueueSettings (..))
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxBytes, cacheMaxEntries, cacheTtl))
import Ecluse.Rts (Provenance (FromCgroup, FromRts), RuntimePlan (..))

spec :: Spec
spec = describe "resolveMemoryBudget" $ do
    it "falls back to the shipped values with no heap-ceiling datapoint" $ do
        let (budget, lines') = resolveMemoryBudget bareCache bareLimits bareQueue (planWith Nothing) 40
        mbMaxResponseBytes budget `shouldBe` 12582912
        mbMaxRequestBytes budget `shouldBe` 26214400
        mbCacheMaxBytes budget `shouldBe` 268435456
        mbCacheMaxEntries budget `shouldBe` 1024
        mbQueueMemoryMaxDepth budget `shouldBe` 50000
        lines' `shouldSatisfy` all (\l -> "built-in default" `T.isInfixOf` l || "computed from" `T.isInfixOf` l)

    it "chunks a one-gigabyte ceiling into the documented shares" $ do
        let (budget, _) = resolveMemoryBudget bareCache bareLimits bareQueue (planWith (Just 1073741824)) 40
        -- The working-space share divided over 40 slots and the decode expansion
        -- lands below the policy floor, so the floor holds.
        mbMaxResponseBytes budget `shouldBe` 12582912
        mbMaxRequestBytes budget `shouldBe` 33554432
        mbCacheMaxBytes budget `shouldBe` 268435456
        mbCacheMaxEntries budget `shouldBe` 1024
        mbQueueMemoryMaxDepth budget `shouldBe` 100000

    it "clamps every computed bound on a very large ceiling" $ do
        let (budget, _) = resolveMemoryBudget bareCache bareLimits bareQueue (planWith (Just 17179869184)) 40
        mbMaxResponseBytes budget `shouldBe` 53687091
        mbMaxRequestBytes budget `shouldBe` 104857600
        mbCacheMaxBytes budget `shouldBe` 1073741824
        mbCacheMaxEntries budget `shouldBe` 4096
        mbQueueMemoryMaxDepth budget `shouldBe` 100000

    it "lets an explicit config value win every bound" $ do
        let cache' = bareCache{csMaxEntries = Just 42, csMaxBytes = Just 1048576}
            limits' = bareLimits{limMaxResponseBytes = Just 7000000, limMaxRequestBytes = Just 8000000}
            queue' = bareQueue{qsMemoryMaxDepth = Just 1234}
            (budget, lines') = resolveMemoryBudget cache' limits' queue' (planWith (Just 1073741824)) 40
        mbMaxResponseBytes budget `shouldBe` 7000000
        mbMaxRequestBytes budget `shouldBe` 8000000
        mbCacheMaxBytes budget `shouldBe` 1048576
        mbCacheMaxEntries budget `shouldBe` 42
        mbQueueMemoryMaxDepth budget `shouldBe` 1234
        lines' `shouldSatisfy` all (T.isInfixOf "from config")

    it "tracks the entry bound against a configured byte bound (floored)" $ do
        -- One MiB of cache is four expected entries; the floor keeps a useful set.
        let (budget, _) = resolveMemoryBudget bareCache{csMaxBytes = Just 1048576} bareLimits bareQueue (planWith (Just 1073741824)) 40
        mbCacheMaxEntries budget `shouldBe` 256

    it "names each decision's provenance in its boot line" $ do
        let (_, computedLines) = resolveMemoryBudget bareCache bareLimits bareQueue (planWith (Just 1073741824)) 40
        computedLines `shouldSatisfy` all (T.isInfixOf "computed from")
        -- The ceiling's own provenance rides into each line, so the budget's log
        -- reconciles with the runtime-posture lines it derives from.
        computedLines `shouldSatisfy` any (T.isInfixOf "heap ceiling 1073741824, derived from the cgroup limit")

    it "marries the configured TTL to the budget's cache bounds" $ do
        let (budget, _) = resolveMemoryBudget bareCache{csTtl = 45} bareLimits bareQueue (planWith Nothing) 40
            cacheCfg = budgetCacheConfig bareCache{csTtl = 45} budget
        cacheTtl cacheCfg `shouldBe` 45
        cacheMaxEntries cacheCfg `shouldBe` mbCacheMaxEntries budget
        cacheMaxBytes cacheCfg `shouldBe` mbCacheMaxBytes budget
  where
    -- A resolved posture whose ceiling came from the cgroup, the common container case.
    planWith :: Maybe Int -> RuntimePlan
    planWith ceiling' = RuntimePlan{planCapabilities = (4, FromRts), planMaxHeapBytes = (ceiling', FromCgroup)}

    bareCache :: CacheSettings
    bareCache = CacheSettings{csTtl = 60, csMaxEntries = Nothing, csMaxBytes = Nothing}

    bareLimits :: LimitsSettings
    bareLimits =
        LimitsSettings
            { limMaxResponseBytes = Nothing
            , limMaxVersionCount = 100000
            , limMaxNestingDepth = 64
            , limMaxRequestBytes = Nothing
            }

    bareQueue :: QueueSettings
    bareQueue = QueueSettings{qsUrl = Nothing, qsMemoryMaxDepth = Nothing}
