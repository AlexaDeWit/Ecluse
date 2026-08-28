-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.CacheSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (mapConcurrently)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageInfo (..), PackageName)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), digestOf)
import Ecluse.Core.Server.Cache (
    CacheConfig (..),
    CacheEntry (..),
    MetadataCache,
    Source (..),
    StoreBudget (..),
    cachedMetadata,
    newMetadataCache,
    weighCacheEntry,
 )
import Ecluse.Core.Server.Cache qualified as Cache
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Port (noopMetricsPort)

{- | Resolve through the cache with an inert metrics port, over a fetch that cannot fail. A 'Left'
is a test bug, and surfaces as 'UnexpectedFault'. The store itself has its own spec,
"Ecluse.Server.Cache.StoreSpec".
-}
resolveMetadata :: MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadata c source name fetch =
    unwrapResolved =<< Cache.resolveMetadata noopMetricsPort c source name (Right <$> fetch)

-- | The success-path wrappers' unwrap: a 'Left' from a cannot-fail fetch is a test bug.
unwrapResolved :: Either MetadataError a -> IO a
unwrapResolved = either (throwIO . UnexpectedFault) pure

-- | The typed wrapper for a 'Left' no success-path case expects (see 'resolveMetadata').
newtype UnexpectedFault = UnexpectedFault MetadataError
    deriving stock (Show)

instance Exception UnexpectedFault

-- | Resolve through the assembled-representation store with an inert metrics port.
resolveAssembled :: MetadataCache -> Text -> IO ByteString -> IO ByteString
resolveAssembled = Cache.resolveAssembled noopMetricsPort

-- | A counting render: bumps the counter, then yields the given assembled bytes.
countingRender :: IORef Int -> ByteString -> IO ByteString
countingRender renders bytes = do
    atomicModifyIORef' renders (\n -> (n + 1, ()))
    pure bytes

-- | A filler body of the given size, for driving the assembled store's byte budget.
mkBytes :: Int -> Char -> ByteString
mkBytes n c = BS.replicate n (fromIntegral (ord c))

-- | The two upstream sources of a packument, keyed by base URL.
privateSource, publicSource :: Source
privateSource = Source "https://private.example"
publicSource = Source "https://public.example"

{- | A 'PackageInfo' carrying only its name. That is enough to assert which metadata
value the cache stored and returned, without building a full document.
-}
info :: PackageName -> PackageInfo
info name =
    PackageInfo
        { infoName = name
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

{- | A cache entry pairing the named metadata with a raw 'Value' tagged by a marker. A
test then asserts which exact (typed view, raw bytes) pair a hit returned.
-}
entry :: PackageName -> Text -> CacheEntry
entry name marker = CacheEntry{entryInfo = info name, entryRaw = cachedRaw marker, entryDigest = digestOf (encodeUtf8 marker)}

{- | A marker raw document injected through npm's boundary pair, so a test can build the opaque
'CachedDoc' without its private constructor.
-}
cachedRaw :: Text -> CachedDoc
cachedRaw = fst npmCached . String

{- | A cache config with the given TTL (seconds) and maximum entry count. The resident
budget is generous enough that the entry count is the binding bound.
-}
config :: NominalDiffTime -> Int -> CacheConfig
config ttl size = configBytes ttl size (1024 * 1024 * 1024)

{- | A cache config with the given TTL (seconds), entry count, and resident-byte budget. Every
store gets the same bounds, because these specs exercise one store at a time.
-}
configBytes :: NominalDiffTime -> Int -> Int -> CacheConfig
configBytes ttl size bytes =
    CacheConfig
        { cacheTtl = ttl
        , cacheFullBudget = budget
        , cacheVersionBudget = budget
        , cacheAssembledBudget = budget
        }
  where
    budget = StoreBudget{sbMaxEntries = size, sbMaxBytes = bytes}

{- | The estimated resident weight of a single empty-versions cache entry, so a byte
budget reads as a count of these entries.
-}
entryWeight :: Int
entryWeight = weighCacheEntry (entry (unscopedNpm "weight-probe") "raw")

{- | A metrics port that captures the most recent full-packument residency-gauge value, with a
reader for it. Every other field is inert.
-}
recordingResidencyPort :: IO (MetricsPort, IO (Maybe Int))
recordingResidencyPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpCacheResidentBytes = writeIORef seen . Just}
    pure (port, readIORef seen)

{- | A metrics port that captures the most recent full-packument entry-count gauge value, with a
reader for it. That gauge is the live occupancy path, not a direct size poll.
-}
recordingEntriesPort :: IO (MetricsPort, IO (Maybe Int))
recordingEntriesPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpCacheEntries = writeIORef seen . Just}
    pure (port, readIORef seen)

-- | A fresh cache with a generous TTL and ample room.
freshCache :: IO MetadataCache
freshCache = newMetadataCache (config 60 100)

{- | A counting fetch: bumps the call counter and yields the named metadata paired
with a raw 'Value' tagged by the given marker.
-}
countingFetch :: IORef Int -> PackageName -> Text -> IO CacheEntry
countingFetch calls name marker = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (entry name marker)

spec :: Spec
spec = do
    describe "resolveMetadata -- hit/miss" $ do
        it "fetches on a miss and returns the parsed metadata with its raw bytes" $ do
            c <- freshCache
            result <- resolveMetadata c publicSource (unscopedNpm "is-odd") (pure (entry (unscopedNpm "is-odd") "raw"))
            infoName (entryInfo result) `shouldBe` unscopedNpm "is-odd"
            entryRaw result `shouldBe` cachedRaw "raw"

        it "serves a second resolution from cache without re-fetching" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (unscopedNpm "left-pad") (countingFetch calls (unscopedNpm "left-pad") "raw")
            _ <- resolveMetadata c publicSource (unscopedNpm "left-pad") (countingFetch calls (unscopedNpm "left-pad") "raw")
            readIORef calls `shouldReturn` 1

        it "returns the coherent pair the entry was cached with on a hit" $ do
            -- A hit serves the cached typed view and the exact bytes it was parsed from, never the
            -- caller's later fetch, so the second marker must not appear.
            c <- freshCache
            _ <- resolveMetadata c publicSource (unscopedNpm "coherent") (pure (entry (unscopedNpm "coherent") "first"))
            hit <- resolveMetadata c publicSource (unscopedNpm "coherent") (pure (entry (unscopedNpm "coherent") "second"))
            entryRaw hit `shouldBe` cachedRaw "first"
            infoName (entryInfo hit) `shouldBe` unscopedNpm "coherent"

        it "caches per package, not globally (distinct keys both fetch)" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (unscopedNpm "a") (countingFetch calls (unscopedNpm "a") "raw")
            _ <- resolveMetadata c publicSource (unscopedNpm "b") (countingFetch calls (unscopedNpm "b") "raw")
            readIORef calls `shouldReturn` 2

        it "exposes a cached entry through cachedMetadata after a resolution" $ do
            c <- freshCache
            _ <- resolveMetadata c publicSource (unscopedNpm "react") (pure (entry (unscopedNpm "react") "raw"))
            cached <- cachedMetadata c publicSource (unscopedNpm "react")
            (infoName . entryInfo <$> cached) `shouldBe` Just (unscopedNpm "react")

        it "reports a miss through cachedMetadata before any resolution" $ do
            c <- freshCache
            cachedMetadata c publicSource (unscopedNpm "never-fetched") `shouldReturn` Nothing

        it "re-fetches after a failed fetch rather than caching the failure" $ do
            c <- freshCache
            calls <- newIORef 0
            let failing = atomicModifyIORef' calls (\n -> (n + 1, ())) $> Left MetadataUndecodable
            failed <- Cache.resolveMetadata noopMetricsPort c publicSource (unscopedNpm "flaky") failing
            failed `shouldBe` Left MetadataUndecodable
            -- The failed fetch left nothing cached, so the next resolution fetches again.
            _ <- resolveMetadata c publicSource (unscopedNpm "flaky") (countingFetch calls (unscopedNpm "flaky") "raw")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata -- per-source isolation" $ do
        it "keeps the private and public documents of one package apart" $ do
            -- Two distinct sources serve the same package. Each source has its own
            -- entry, so neither origin sees the other's bytes.
            c <- freshCache
            _ <- resolveMetadata c privateSource (unscopedNpm "shared") (pure (entry (unscopedNpm "shared") "private-doc"))
            _ <- resolveMetadata c publicSource (unscopedNpm "shared") (pure (entry (unscopedNpm "shared") "public-doc"))
            priv <- cachedMetadata c privateSource (unscopedNpm "shared")
            pub <- cachedMetadata c publicSource (unscopedNpm "shared")
            (entryRaw <$> priv) `shouldBe` Just (cachedRaw "private-doc")
            (entryRaw <$> pub) `shouldBe` Just (cachedRaw "public-doc")

        it "fetches once per source even for the same package" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c privateSource (unscopedNpm "two-origins") (countingFetch calls (unscopedNpm "two-origins") "priv")
            _ <- resolveMetadata c publicSource (unscopedNpm "two-origins") (countingFetch calls (unscopedNpm "two-origins") "pub")
            -- Two distinct (source, package) keys mean two fetches, and neither reuses
            -- the other's entry.
            readIORef calls `shouldReturn` 2

        it "a hit for one source never satisfies a miss for the other" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c privateSource (unscopedNpm "iso") (countingFetch calls (unscopedNpm "iso") "priv")
            -- The private entry is warm, but a public resolution still fetches its own.
            _ <- resolveMetadata c publicSource (unscopedNpm "iso") (countingFetch calls (unscopedNpm "iso") "pub")
            cachedMetadata c publicSource (unscopedNpm "iso") >>= \pub ->
                (entryRaw <$> pub) `shouldBe` Just (cachedRaw "pub")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata -- TTL" $
        it "re-fetches once the short TTL has elapsed" $ do
            c <- newMetadataCache (config 0.05 100) -- 50 ms TTL
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (unscopedNpm "stale") (countingFetch calls (unscopedNpm "stale") "raw")
            threadDelay 120000 -- 120 ms > TTL
            _ <- resolveMetadata c publicSource (unscopedNpm "stale") (countingFetch calls (unscopedNpm "stale") "raw")
            readIORef calls `shouldReturn` 2

    describe "size bound" $
        it "counts the two sources of one package as two entries against the bound" $ do
            -- The size bound counts (source, package) entries, so one package from both origins
            -- takes two slots. The entry-count gauge is the live occupancy path.
            (port, readEntries) <- recordingEntriesPort
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 10 :: Int] $ \i -> do
                _ <- Cache.resolveMetadata port c privateSource (unscopedNpm (show i)) (pure (Right (entry (unscopedNpm (show i)) "priv")))
                Cache.resolveMetadata port c publicSource (unscopedNpm (show i)) (pure (Right (entry (unscopedNpm (show i)) "pub")))
            n <- readEntries
            n `shouldSatisfy` maybe False (<= 4)

    describe "resident-byte budget" $
        it "reports the resident bytes through the residency gauge" $ do
            -- The residency gauge reflects the held entries' summed weight. Both bounds are
            -- generous here, so all four entries stay held.
            (port, readResidency) <- recordingResidencyPort
            c <- newMetadataCache (config 60 100)
            for_ [1 .. 4 :: Int] $ \i ->
                Cache.resolveMetadata port c publicSource (unscopedNpm (show i)) (pure (Right (entry (unscopedNpm (show i)) "raw")))
            residency <- readResidency
            residency `shouldBe` Just (4 * entryWeight)

    describe "resolveAssembled -- the assembled-representation store" $ do
        it "serves the stored bytes on a repeat key without re-rendering" $ do
            renders <- newIORef (0 :: Int)
            c <- newMetadataCache (config 60 8)
            initial <- resolveAssembled c "\"tag-a\"" (countingRender renders "assembled-bytes")
            again <- resolveAssembled c "\"tag-a\"" (countingRender renders "assembled-bytes")
            initial `shouldBe` "assembled-bytes"
            again `shouldBe` "assembled-bytes"
            readIORef renders `shouldReturn` 1

        it "keeps distinct keys distinct (a different validator never shares bytes)" $ do
            renders <- newIORef (0 :: Int)
            c <- newMetadataCache (config 60 8)
            a <- resolveAssembled c "\"tag-a\"" (countingRender renders "bytes-a")
            b <- resolveAssembled c "\"tag-b\"" (countingRender renders "bytes-b")
            (a, b) `shouldBe` ("bytes-a", "bytes-b")
            readIORef renders `shouldReturn` 2

        it "coalesces concurrent identical renders onto one leader" $ do
            renders <- newIORef (0 :: Int)
            c <- newMetadataCache (config 60 8)
            results <-
                mapConcurrently
                    (\(_ :: Int) -> resolveAssembled c "\"tag-a\"" (threadDelay 20_000 >> countingRender renders "assembled-bytes"))
                    [1 .. 8]
            results `shouldSatisfy` all (== "assembled-bytes")
            readIORef renders `shouldReturn` 1

        it "evicts to the byte budget, re-rendering an evicted entry on its next request" $ do
            renders <- newIORef (0 :: Int)
            -- A budget that holds roughly one large entry: the second insert evicts the
            -- first, so re-requesting the first key re-renders.
            let bigBytes = 4096
                budget = bigBytes + 1024
            c <- newMetadataCache (configBytes 60 8 budget)
            _ <- resolveAssembled c "\"tag-a\"" (countingRender renders (mkBytes bigBytes 'a'))
            _ <- resolveAssembled c "\"tag-b\"" (countingRender renders (mkBytes bigBytes 'b'))
            _ <- resolveAssembled c "\"tag-a\"" (countingRender renders (mkBytes bigBytes 'a'))
            readIORef renders `shouldReturn` 3

    describe "the named sub-budgets" $ do
        it "a version-store flood evicts only version entries; the full store stays resident" $ do
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheFullBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = 100 * entryWeight}
                        , cacheVersionBudget = StoreBudget{sbMaxEntries = 2, sbMaxBytes = 1024 * 1024}
                        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = 1024 * 1024}
                        }
            let name = unscopedNpm "hot-head"
            _ <- resolveMetadata c publicSource name (pure (entry name "raw"))
            for_ ([1 .. 5] :: [Int]) $ \i ->
                Cache.resolveVersion noopMetricsPort c publicSource name (mkVersion Npm (show i <> ".0.0")) (pure (Right Nothing))
            -- The flood churned the version store past its own entry bound...
            Cache.cachedVersion c publicSource name (mkVersion Npm "1.0.0") `shouldReturn` Nothing
            -- ...while the full store's resident head was untouched (class isolation:
            -- one store's eviction pressure never reaches a sibling).
            found <- cachedMetadata c publicSource name
            found `shouldSatisfy` isJust

        it "keeps the summed residency of all three stores within the summed sub-budgets" $ do
            -- Per-store sub-budgets cap the total reported residency at their sum, however hard
            -- the test floods every class.
            fullSeen <- newIORef 0
            versionSeen <- newIORef 0
            assembledSeen <- newIORef 0
            let port =
                    noopMetricsPort
                        { mpCacheResidentBytes = writeIORef fullSeen
                        , mpVersionCacheResidentBytes = writeIORef versionSeen
                        , mpAssembledCacheResidentBytes = writeIORef assembledSeen
                        }
                fullBytes = 4 * entryWeight
                versionBytes = 64 * 1024
                assembledBytes = 8 * 1024
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheFullBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = fullBytes}
                        , cacheVersionBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = versionBytes}
                        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = assembledBytes}
                        }
            for_ ([1 .. 10] :: [Int]) $ \i -> do
                let name = unscopedNpm ("filler-" <> show i)
                _ <- Cache.resolveMetadata port c publicSource name (pure (Right (entry name "raw")))
                _ <- Cache.resolveVersion port c publicSource name (mkVersion Npm "1.0.0") (pure (Right Nothing))
                _ <- Cache.resolveAssembled port c (show i) (pure (mkBytes 2048 'x'))
                pass
            total <- sum <$> traverse readIORef [fullSeen, versionSeen, assembledSeen]
            total `shouldSatisfy` (<= fullBytes + versionBytes + assembledBytes)

    describe "cachedVersion -- read recency" $
        it "a cachedVersion read bumps the version entry's recency, so a re-read entry survives eviction (LRU, not FIFO)" $ do
            -- cachedVersion is the version store's only steady-state read, and a hit short-circuits
            -- before resolveVersion's own recency bump. This read must bump recency, or a warm
            -- version entry ages out in insert order.
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheFullBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = 1024 * 1024}
                        , cacheVersionBudget = StoreBudget{sbMaxEntries = 2, sbMaxBytes = 1024 * 1024}
                        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 100, sbMaxBytes = 1024 * 1024}
                        }
            let name = unscopedNpm "recency"
                v n = mkVersion Npm (show (n :: Int) <> ".0.0")
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 1) (pure (Right Nothing))
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 2) (pure (Right Nothing))
            -- Re-read v1 through the serve-path accessor: bumps its recency, so v2 is coldest.
            _ <- Cache.cachedVersion c publicSource name (v 1)
            -- Insert v3, evicting the least-recently-used entry.
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 3) (pure (Right Nothing))
            -- v1 (re-read) survived, and the store evicted v2 (untouched, inserted later).
            Cache.cachedVersion c publicSource name (v 1) `shouldReturn` Just Nothing
            Cache.cachedVersion c publicSource name (v 2) `shouldReturn` Nothing
