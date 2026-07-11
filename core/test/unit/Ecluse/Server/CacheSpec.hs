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
import Ecluse.Core.Package (PackageInfo (..), PackageName, mkPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), digestOf)
import Ecluse.Core.Server.Cache (
    CacheConfig (..),
    CacheEntry (..),
    MetadataCache,
    Source (..),
    cacheSize,
    cachedMetadata,
    defaultCacheConfig,
    newMetadataCache,
    weighCacheEntry,
 )
import Ecluse.Core.Server.Cache qualified as Cache
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Port (noopMetricsPort)

{- | Resolve through the cache with an inert metrics port, so these tests assert the
cache's hit\/miss and keying behaviour without a telemetry backend. The inert port
discards every recording, so it neither records nor affects the cache. The
success-path adapter: these cases drive the cache with fetches that cannot fail, so
the wrapper lifts them into the typed channel and a 'Left' is a test bug surfaced as
'UnexpectedFault' (the typed-failure cases call "Ecluse.Core.Server.Cache" directly).
The store machinery itself (single-flight collapse, the orphan window, the entry and
byte bounds) is covered by its own spec, "Ecluse.Server.Cache.StoreSpec".
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

-- | A package name fixture; the metadata cache keys on package identity.
pkg :: Text -> PackageName
pkg = mkPackageName Npm Nothing

-- | The two upstream sources a packument is fetched from, keyed by base URL.
privateSource, publicSource :: Source
privateSource = Source "https://private.example"
publicSource = Source "https://public.example"

{- | A 'PackageInfo' carrying only its name -- enough to assert which metadata
value the cache stored and returned without building a full document.
-}
info :: PackageName -> PackageInfo
info name =
    PackageInfo
        { infoName = name
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

{- | A cache entry pairing the named metadata with a raw 'Value' tagged by a marker,
so a test can assert which exact (typed view, raw bytes) pair a hit returned.
-}
entry :: PackageName -> Text -> CacheEntry
entry name marker = CacheEntry{entryInfo = info name, entryRaw = String marker, entryDigest = digestOf (encodeUtf8 marker)}

{- | A cache config with the given TTL (seconds) and maximum entry count, with a resident
budget generous enough that the entry count is the binding bound.
-}
config :: NominalDiffTime -> Int -> CacheConfig
config ttl size = configBytes ttl size (1024 * 1024 * 1024)

-- | A cache config with the given TTL (seconds), entry count, and resident-byte budget.
configBytes :: NominalDiffTime -> Int -> Int -> CacheConfig
configBytes ttl size bytes = CacheConfig{cacheTtl = ttl, cacheMaxEntries = size, cacheMaxBytes = bytes}

{- | The resident weight a single empty-versions cache entry is estimated at, so a byte
budget can be expressed as a count of these entries.
-}
entryWeight :: Int
entryWeight = weighCacheEntry (entry (pkg "weight-probe") "raw")

{- | A metrics port that captures the most-recent full-packument residency-gauge value it
is handed, alongside a reader for it. Every other field is inert. Lets a test assert the
residency the cache last reported on a leader insert.
-}
recordingResidencyPort :: IO (MetricsPort, IO (Maybe Int))
recordingResidencyPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpCacheResidentBytes = writeIORef seen . Just}
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
            -- from, never the caller's later (would-be) fetch -- the second fetch's
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
            let failing = atomicModifyIORef' calls (\n -> (n + 1, ())) $> Left MetadataUndecodable
            failed <- Cache.resolveMetadata noopMetricsPort c publicSource (pkg "flaky") failing
            failed `shouldBe` Left MetadataUndecodable
            -- The failed fetch left nothing cached, so the next resolution fetches again.
            _ <- resolveMetadata c publicSource (pkg "flaky") (countingFetch calls (pkg "flaky") "raw")
            readIORef calls `shouldReturn` 2

    describe "resolveMetadata -- per-source isolation" $ do
        it "keeps the private and public documents of one package apart" $ do
            -- The same package is fetched from two distinct sources; each source has its
            -- own entry, so neither origin sees the other's bytes (no cross-contamination).
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
            _ <- resolveMetadata c privateSource (pkg "two-origins") (countingFetch calls (pkg "two-origins") "priv")
            _ <- resolveMetadata c publicSource (pkg "two-origins") (countingFetch calls (pkg "two-origins") "pub")
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

    describe "resolveMetadata -- TTL" $
        it "re-fetches once the short TTL has elapsed" $ do
            c <- newMetadataCache (config 0.05 100) -- 50 ms TTL
            calls <- newIORef 0
            _ <- resolveMetadata c publicSource (pkg "stale") (countingFetch calls (pkg "stale") "raw")
            threadDelay 120000 -- 120 ms > TTL
            _ <- resolveMetadata c publicSource (pkg "stale") (countingFetch calls (pkg "stale") "raw")
            readIORef calls `shouldReturn` 2

    describe "size bound" $
        it "counts the two sources of one package as two entries against the bound" $ do
            -- The size bound is over (source, package) entries: caching one package
            -- from both origins occupies two slots, exercising the per-source key under
            -- the bound.
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 10 :: Int] $ \i -> do
                _ <- resolveMetadata c privateSource (pkg (show i)) (pure (entry (pkg (show i)) "priv"))
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "pub"))
            n <- cacheSize c
            n `shouldSatisfy` (<= 4)

    describe "resident-byte budget" $
        it "reports the resident bytes through the residency gauge" $ do
            -- The residency gauge reflects the held entries' summed weight: after resolving
            -- a few distinct packages (under both bounds), the last reported value equals the
            -- entry count times the per-entry weight.
            (port, readResidency) <- recordingResidencyPort
            c <- newMetadataCache (config 60 100)
            for_ [1 .. 4 :: Int] $ \i ->
                Cache.resolveMetadata port c publicSource (pkg (show i)) (pure (Right (entry (pkg (show i)) "raw")))
            residency <- readResidency
            n <- cacheSize c
            residency `shouldBe` Just (n * entryWeight)
            n `shouldBe` 4

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

    describe "defaultCacheConfig" $ do
        it "uses a short, non-zero TTL" $
            cacheTtl defaultCacheConfig `shouldSatisfy` (> 0)

        it "bounds the cache to a positive entry count" $
            cacheMaxEntries defaultCacheConfig `shouldSatisfy` (> 0)

        it "bounds the cache to a positive resident-byte budget" $
            cacheMaxBytes defaultCacheConfig `shouldSatisfy` (> 0)
