module Ecluse.Server.CacheSpec (spec) where

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
    MetadataCache,
    cacheSize,
    cachedMetadata,
    defaultCacheConfig,
    newMetadataCache,
    resolveMetadata,
 )

-- | A package name fixture; the metadata cache keys on package identity.
pkg :: Text -> PackageName
pkg = mkPackageName Npm Nothing

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

-- | A cache config with the given TTL (seconds) and maximum entry count.
config :: NominalDiffTime -> Int -> CacheConfig
config ttl size = CacheConfig{cacheTtl = ttl, cacheMaxEntries = size}

-- | A fresh cache with a generous TTL and ample room.
freshCache :: IO MetadataCache
freshCache = newMetadataCache (config 60 100)

-- | A counting fetch: bumps the call counter and yields the named metadata.
countingFetch :: IORef Int -> PackageName -> IO PackageInfo
countingFetch calls name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (info name)

spec :: Spec
spec = do
    describe "resolveMetadata — hit/miss" $ do
        it "fetches on a miss and returns the parsed metadata" $ do
            c <- freshCache
            result <- resolveMetadata c (pkg "is-odd") (pure (info (pkg "is-odd")))
            infoName result `shouldBe` pkg "is-odd"

        it "serves a second resolution from cache without re-fetching" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c (pkg "left-pad") (countingFetch calls (pkg "left-pad"))
            _ <- resolveMetadata c (pkg "left-pad") (countingFetch calls (pkg "left-pad"))
            readIORef calls `shouldReturn` 1

        it "caches per package, not globally (distinct keys both fetch)" $ do
            c <- freshCache
            calls <- newIORef 0
            _ <- resolveMetadata c (pkg "a") (countingFetch calls (pkg "a"))
            _ <- resolveMetadata c (pkg "b") (countingFetch calls (pkg "b"))
            readIORef calls `shouldReturn` 2

        it "exposes a cached entry through cachedMetadata after a resolution" $ do
            c <- freshCache
            _ <- resolveMetadata c (pkg "react") (pure (info (pkg "react")))
            cached <- cachedMetadata c (pkg "react")
            (infoName <$> cached) `shouldBe` Just (pkg "react")

        it "reports a miss through cachedMetadata before any resolution" $ do
            c <- freshCache
            cachedMetadata c (pkg "never-fetched") `shouldReturn` Nothing

        it "re-fetches after a failed fetch rather than caching the failure" $ do
            c <- freshCache
            calls <- newIORef 0
            let boom = atomicModifyIORef' calls (\n -> (n + 1, ())) >> throwString "boom"
            _ <- resolveMetadata c (pkg "flaky") boom `catchAny` const (pure (info (pkg "flaky")))
            -- The failed fetch left nothing cached, so the next resolution fetches again.
            _ <- resolveMetadata c (pkg "flaky") (countingFetch calls (pkg "flaky"))
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata — TTL" $
        it "re-fetches once the short TTL has elapsed" $ do
            c <- newMetadataCache (config 0.05 100) -- 50 ms TTL
            calls <- newIORef 0
            _ <- resolveMetadata c (pkg "stale") (countingFetch calls (pkg "stale"))
            threadDelay 120000 -- 120 ms > TTL
            _ <- resolveMetadata c (pkg "stale") (countingFetch calls (pkg "stale"))
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata — collapse" $
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
                    pure (info (pkg "hot"))
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (resolveMetadata c (pkg "hot") fetch)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started -- wait until a fetch has begun
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release () -- let the single fetch complete
                    )
            map infoName results `shouldBe` replicate 8 (pkg "hot")
            readIORef calls `shouldReturn` 1

    describe "size bound" $ do
        it "never exceeds the configured maximum entry count" $ do
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveMetadata c (pkg (show i)) (pure (info (pkg (show i))))
            n <- cacheSize c
            n `shouldSatisfy` (<= 4)

        it "keeps serving fresh resolutions even under eviction pressure" $ do
            c <- newMetadataCache (config 60 2)
            for_ [1 .. 10 :: Int] $ \i ->
                resolveMetadata c (pkg (show i)) (pure (info (pkg (show i))))
            result <- resolveMetadata c (pkg "final") (pure (info (pkg "final")))
            infoName result `shouldBe` pkg "final"

    describe "defaultCacheConfig" $ do
        it "uses a short, non-zero TTL" $
            cacheTtl defaultCacheConfig `shouldSatisfy` (> 0)

        it "bounds the cache to a positive entry count" $
            cacheMaxEntries defaultCacheConfig `shouldSatisfy` (> 0)
