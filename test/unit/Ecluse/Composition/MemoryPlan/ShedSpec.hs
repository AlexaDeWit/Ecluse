-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Computed cardinality through the real pooled local provider.
module Ecluse.Composition.MemoryPlan.ShedSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.MemoryPlan (MemoryPlan (..), QueueTenantDemand (NoQueueTenant), planCacheConfig, resolveMemoryPlan)
import Ecluse.Composition.Support (expectAppConfig, gib, mib, noCeiling, staticEnvVars)
import Ecluse.Config (AppConfig (..), CacheSettings (..))
import Ecluse.Core.Package (Availability (Deprecated), PackageDetails (pkgAvailability))
import Ecluse.Core.Registry.Metadata (VersionRead)
import Ecluse.Core.Server.Cache (CacheConfig (..))
import Ecluse.Core.Server.Cache.Backend.Internal (CacheOccupancy (..), Recency (PreserveRecency), RetentionBackend (..))
import Ecluse.Core.Server.Cache.Provider (localCacheProvider, providerAssembled, providerFull, providerVersion)
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Test.Package (sampleDetails, thingName, v1_0_0)
import Ecluse.Test.Snapshot (untaggedRead)

-- | Check count policy against retention, byte pressure, and explicit pins.
spec :: Spec
spec = describe "shared local entry allowance" $ do
    it "keeps the computed floor, cap, and exact explicit count" $ do
        for_ [(0, 256), (mib, 256), (64 * mib, 4096), (256 * mib, 16384), (gib, 65536), (2 * gib, 65536)] $ \(bytes, entries) -> do
            config <- plannedCache (Just bytes) Nothing
            cacheMaxEntries config `shouldBe` entries
        fallback <- plannedCache Nothing Nothing
        cacheMaxEntries fallback `shouldBe` 16384
        for_ [1, 42, 100000] $ \entries -> do
            config <- plannedCache (Just (64 * mib)) (Just entries)
            cacheMaxEntries config `shouldBe` entries

    it "fills the byte budget with 64 KiB present selected entries before eviction" $ do
        fixture <- localFixture Nothing
        weighVersion selected64KiB `shouldBe` 65536
        for_ [1 .. 1024 :: Int] $ \key -> insertSelected fixture (show key) selected64KiB
        occupancy fixture `shouldReturn` CacheOccupancy 1024 (64 * mib)
        lookupSelected fixture "1" `shouldReturn` Just selected64KiB
        insertSelected fixture "1025" selected64KiB
        occupancy fixture `shouldReturn` CacheOccupancy 1024 (64 * mib)
        lookupSelected fixture "1" `shouldReturn` Nothing
        lookupSelected fixture "1025" `shouldReturn` Just selected64KiB

    it "shares bytes between selected and assembled entries without premature count pressure" $ do
        fixture <- localFixture Nothing
        let assembled = BS.replicate (32 * mib - 256) 120
        insertAssembled fixture "listing" assembled
        for_ [1 .. 512 :: Int] $ \key -> insertSelected fixture (show key) selected64KiB
        occupancy fixture `shouldReturn` CacheOccupancy 513 (64 * mib)
        lookupSelected fixture "1" `shouldReturn` Just selected64KiB
        insertSelected fixture "513" selected64KiB
        occupancy fixture `shouldReturn` CacheOccupancy 513 (64 * mib)
        lookupSelected fixture "1" `shouldReturn` Nothing
        rbLookup (pfAssembled fixture) (writeIORef (pfAssembledOccupancy fixture)) unexpected PreserveRecency "listing"
            `shouldReturn` Just assembled

    it "limits 1 KiB absences by global count while leaving byte headroom" $ do
        fixture <- localFixture Nothing
        let absent = untaggedRead Nothing
        weighVersion absent `shouldBe` 1024
        insertAssembled fixture "listing" "small"
        for_ [1 .. 4095 :: Int] $ \key -> insertSelected fixture (show key) absent
        occupancy fixture `shouldReturn` CacheOccupancy 4096 (4095 * 1024 + 261)
        insertSelected fixture "4096" absent
        occupancy fixture `shouldReturn` CacheOccupancy 4096 (4095 * 1024 + 261)
        lookupSelected fixture "1" `shouldReturn` Nothing
        lookupSelected fixture "4096" `shouldReturn` Just absent

    it "enforces an explicit count across both stores without applying the computed floor" $ do
        fixture <- localFixture (Just 2)
        insertAssembled fixture "listing" "small"
        insertSelected fixture "first" (untaggedRead Nothing)
        insertSelected fixture "second" (untaggedRead Nothing)
        occupancy fixture `shouldReturn` CacheOccupancy 2 (1024 + 261)
        lookupSelected fixture "first" `shouldReturn` Nothing
        lookupSelected fixture "second" `shouldReturn` Just (untaggedRead Nothing)

plannedCache :: Maybe Int -> Maybe Int -> IO CacheConfig
plannedCache bytes entries = do
    app <- expectAppConfig staticEnvVars Nothing
    let settings = (cfgCache app){csMaxBytes = bytes, csMaxEntries = entries}
        (plan, _) = resolveMemoryPlan settings (cfgLimits app) (cfgQueue app) Nothing noCeiling NoQueueTenant False
    mpOverrideViolations plan `shouldBe` []
    pure (planCacheConfig settings plan)

data PoolFixture = PoolFixture
    { pfVersion :: RetentionBackend Text VersionRead
    , pfAssembled :: RetentionBackend Text ByteString
    , pfVersionOccupancy :: IORef CacheOccupancy
    , pfAssembledOccupancy :: IORef CacheOccupancy
    }

localFixture :: Maybe Int -> IO PoolFixture
localFixture entries = do
    config <- plannedCache (Just (64 * mib)) entries
    provider <- localCacheProvider config
    isNothing (providerFull provider) `shouldBe` True
    version <- maybe (fail "missing selected capability") pure (providerVersion provider)
    assembled <- maybe (fail "missing assembled capability") pure (providerAssembled provider)
    PoolFixture version assembled <$> newIORef (CacheOccupancy 0 0) <*> newIORef (CacheOccupancy 0 0)

occupancy :: PoolFixture -> IO CacheOccupancy
occupancy fixture = do
    selected <- readIORef (pfVersionOccupancy fixture)
    assembled <- readIORef (pfAssembledOccupancy fixture)
    pure (CacheOccupancy (occEntries selected + occEntries assembled) (occBytes selected + occBytes assembled))

insertSelected :: PoolFixture -> Text -> VersionRead -> IO ()
insertSelected fixture = rbInsert (pfVersion fixture) (writeIORef (pfVersionOccupancy fixture)) unexpected unexpected

insertAssembled :: PoolFixture -> Text -> ByteString -> IO ()
insertAssembled fixture = rbInsert (pfAssembled fixture) (writeIORef (pfAssembledOccupancy fixture)) unexpected unexpected

lookupSelected :: PoolFixture -> Text -> IO (Maybe VersionRead)
lookupSelected fixture = rbLookup (pfVersion fixture) (writeIORef (pfVersionOccupancy fixture)) unexpected PreserveRecency

unexpected :: IO ()
unexpected = expectationFailure "local retention unexpectedly refused or failed"

-- A diagnostic field grows a real present release to the requested accounted charge.
selected64KiB :: VersionRead
selected64KiB = release (T.replicate (65536 - weighVersion (release (T.singleton 'x')) + 1) "x")
  where
    release reason = untaggedRead (Just (sampleDetails thingName v1_0_0){pkgAvailability = Deprecated reason})
