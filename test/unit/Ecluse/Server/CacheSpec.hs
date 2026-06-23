module Ecluse.Server.CacheSpec (spec) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (catchAny, concurrently, mapConcurrently)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageInfo (..), PackageName, mkPackageName)
import Ecluse.Server.Cache (
    CacheConfig (..),
    CacheEntry (..),
    MetadataCache,
    Source (..),
    cacheSize,
    cachedMetadata,
    defaultCacheConfig,
    newMetadataCache,
    resolveMetadata,
 )

-- | A package name fixture; the metadata cache keys on package identity.
pkg :: Text -> PackageName
pkg = mkPackageName Npm Nothing

-- | The two upstream sources a packument is fetched from, keyed by base URL.
privateSource, publicSource :: Source
privateSource = Source "https://private.example"
publicSource = Source "https://public.example"

{- | A 'PackageInfo' carrying only its name — enough to assert which metadata
value the cache stored and returned without building a full document.
-}
info :: PackageName -> PackageInfo
info name =
    PackageInfo
        { infoName = name
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoPublishedAt = Map.empty
        }

{- | A cache entry pairing the named metadata with a raw 'Value' tagged by a marker,
so a test can assert which exact (typed view, raw bytes) pair a hit returned.
-}
entry :: PackageName -> Text -> CacheEntry
entry name marker = CacheEntry{entryInfo = info name, entryRaw = String marker}

-- | A cache config with the given TTL (seconds) and maximum entry count.
config :: NominalDiffTime -> Int -> CacheConfig
config ttl size = CacheConfig{cacheTtl = ttl, cacheMaxEntries = size}

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
    describe "resolveMetadata — hit/miss" $ do
        it "fetches on a miss and returns the parsed metadata with its raw bytes" $ do
            c <- freshCache
            result <- resolveMetadata c publicSource (pkg "is-odd") (pure (entry (pkg "is-odd") "raw"))
            infoName (entryInfo result) `shouldBe` pkg "is-odd"
            entryRaw result `shouldBe` String "raw"

        it "serves a second resolution from cache without re-fetching" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (pkg "left-pad") (countingFetch calls (pkg "left-pad") "raw")
            _ <- resolveMetadata c publicSource (pkg "left-pad") (countingFetch calls (pkg "left-pad") "raw")
            readIORef calls `shouldReturn` 1

        it "returns the coherent pair the entry was cached with on a hit" $ do
            -- A hit serves the cached typed view and the exact bytes it was parsed
            -- from, never the caller's later (would-be) fetch — the second fetch's
            -- distinct marker must not appear, since it never runs.
            c <- freshCache
            _ <- resolveMetadata c publicSource (pkg "coherent") (pure (entry (pkg "coherent") "first"))
            hit <- resolveMetadata c publicSource (pkg "coherent") (pure (entry (pkg "coherent") "second"))
            entryRaw hit `shouldBe` String "first"
            infoName (entryInfo hit) `shouldBe` pkg "coherent"

        it "caches per package, not globally (distinct keys both fetch)" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (pkg "a") (countingFetch calls (pkg "a") "raw")
            _ <- resolveMetadata c publicSource (pkg "b") (countingFetch calls (pkg "b") "raw")
            readIORef calls `shouldReturn` 2

        it "exposes a cached entry through cachedMetadata after a resolution" $ do
            c <- freshCache
            _ <- resolveMetadata c publicSource (pkg "react") (pure (entry (pkg "react") "raw"))
            cached <- cachedMetadata c publicSource (pkg "react")
            (infoName . entryInfo <$> cached) `shouldBe` Just (pkg "react")

        it "reports a miss through cachedMetadata before any resolution" $ do
            c <- freshCache
            cachedMetadata c publicSource (pkg "never-fetched") `shouldReturn` Nothing

        it "re-fetches after a failed fetch rather than caching the failure" $ do
            c <- freshCache
            calls <- newIORef 0
            let boom = atomicModifyIORef' calls (\n -> (n + 1, ())) >> throwString "boom"
            _ <- resolveMetadata c publicSource (pkg "flaky") boom `catchAny` const (pure (entry (pkg "flaky") "raw"))
            -- The failed fetch left nothing cached, so the next resolution fetches again.
            _ <- resolveMetadata c publicSource (pkg "flaky") (countingFetch calls (pkg "flaky") "raw")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata — per-source isolation" $ do
        it "keeps the private and public documents of one package apart" $ do
            -- The same package is fetched from two distinct sources; each source has its
            -- own entry, so neither leg sees the other's bytes (no cross-contamination).
            c <- freshCache
            _ <- resolveMetadata c privateSource (pkg "shared") (pure (entry (pkg "shared") "private-doc"))
            _ <- resolveMetadata c publicSource (pkg "shared") (pure (entry (pkg "shared") "public-doc"))
            priv <- cachedMetadata c privateSource (pkg "shared")
            pub <- cachedMetadata c publicSource (pkg "shared")
            (entryRaw <$> priv) `shouldBe` Just (String "private-doc")
            (entryRaw <$> pub) `shouldBe` Just (String "public-doc")

        it "fetches once per source even for the same package" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c privateSource (pkg "two-legs") (countingFetch calls (pkg "two-legs") "priv")
            _ <- resolveMetadata c publicSource (pkg "two-legs") (countingFetch calls (pkg "two-legs") "pub")
            -- Two distinct (source, package) keys → two fetches; neither reuses the
            -- other's entry.
            readIORef calls `shouldReturn` 2

        it "a hit for one source never satisfies a miss for the other" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c privateSource (pkg "iso") (countingFetch calls (pkg "iso") "priv")
            -- The private entry is warm, but a public resolution still fetches its own.
            _ <- resolveMetadata c publicSource (pkg "iso") (countingFetch calls (pkg "iso") "pub")
            cachedMetadata c publicSource (pkg "iso") >>= \pub ->
                (entryRaw <$> pub) `shouldBe` Just (String "pub")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata — TTL" $
        it "re-fetches once the short TTL has elapsed" $ do
            c <- newMetadataCache (config 0.05 100) -- 50 ms TTL
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (pkg "stale") (countingFetch calls (pkg "stale") "raw")
            threadDelay 120000 -- 120 ms > TTL
            _ <- resolveMetadata c publicSource (pkg "stale") (countingFetch calls (pkg "stale") "raw")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata — collapse" $ do
        it "collapses concurrent resolutions of one package to a single upstream call" $ do
            c <- freshCache
            calls <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            -- The fetch blocks until released, so every concurrent resolver is in
            -- flight at once; if collapse fails, more than one enters the fetch and
            -- the call counter exceeds one.
            let fetch = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    _ <- tryPutMVar started () -- signal the first fetch has begun
                    takeMVar release -- block until the test releases it
                    pure (entry (pkg "hot") "raw")
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (resolveMetadata c publicSource (pkg "hot") fetch)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started -- wait until a fetch has begun
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release () -- let the single fetch complete
                    )
            map (infoName . entryInfo) results `shouldBe` replicate 8 (pkg "hot")
            readIORef calls `shouldReturn` 1

        it "has the entry in the store the instant the leader's fetch returns" $ do
            -- The leader inserts into the store *before* de-registering its in-flight
            -- slot, so by the time resolveMetadata returns the value is already
            -- discoverable via the store (not merely via the now-removed marker).
            -- A caller racing the de-register therefore finds the store entry rather
            -- than re-leading a redundant fetch. The window between insert and
            -- de-register is internal to runLeader (under mask, no injection handle),
            -- so this asserts the observable post-condition the ordering guarantees:
            -- the store is populated as soon as the call completes.
            c <- freshCache
            _ <- resolveMetadata c publicSource (pkg "fresh") (pure (entry (pkg "fresh") "raw"))
            cached <- cachedMetadata c publicSource (pkg "fresh")
            (infoName . entryInfo <$> cached) `shouldBe` Just (pkg "fresh")

        it "does not re-fetch for a caller arriving right after the fetch returns" $ do
            -- Sequential mirror of the collapse property at the post-fetch boundary:
            -- the second resolution lands after the first has fully returned, and is
            -- served from the store with no second upstream call.
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (pkg "back-to-back") (countingFetch calls (pkg "back-to-back") "raw")
            _ <- resolveMetadata c publicSource (pkg "back-to-back") (countingFetch calls (pkg "back-to-back") "raw")
            readIORef calls `shouldReturn` 1

    describe "size bound" $ do
        it "never exceeds the configured maximum entry count" $ do
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "raw"))
            n <- cacheSize c
            n `shouldSatisfy` (<= 4)

        it "counts the two sources of one package as two entries against the bound" $ do
            -- The size bound is over (source, package) entries: caching one package
            -- from both legs occupies two slots, exercising the per-source key under
            -- the bound.
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 10 :: Int] $ \i -> do
                _ <- resolveMetadata c privateSource (pkg (show i)) (pure (entry (pkg (show i)) "priv"))
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "pub"))
            n <- cacheSize c
            n `shouldSatisfy` (<= 4)

        it "keeps serving fresh resolutions even under eviction pressure" $ do
            c <- newMetadataCache (config 60 2)
            for_ [1 .. 10 :: Int] $ \i ->
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "raw"))
            result <- resolveMetadata c publicSource (pkg "final") (pure (entry (pkg "final") "raw"))
            infoName (entryInfo result) `shouldBe` pkg "final"

    describe "defaultCacheConfig" $ do
        it "uses a short, non-zero TTL" $
            cacheTtl defaultCacheConfig `shouldSatisfy` (> 0)

        it "bounds the cache to a positive entry count" $
            cacheMaxEntries defaultCacheConfig `shouldSatisfy` (> 0)
