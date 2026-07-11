{-# LANGUAGE TupleSections #-}

module Ecluse.Server.CacheSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (async, cancel, concurrently, mapConcurrently, timeout, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

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
cache's hit\/miss and single-flight behaviour without a telemetry backend. The inert
port discards every recording, so it neither records nor affects the cache. The
success-path adapter: these cases drive the cache with fetches that cannot fail, so
the wrapper lifts them into the typed channel and a 'Left' is a test bug surfaced as
'UnexpectedFault' (the typed-failure cases call "Ecluse.Core.Server.Cache" directly).
-}
resolveMetadata :: MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadata c source name fetch =
    unwrapResolved =<< Cache.resolveMetadata noopMetricsPort c source name (Right <$> fetch)

-- | As 'resolveMetadata', threading the single-flight handoff hook (an inert metrics port).
resolveMetadataWith :: IO () -> MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadataWith afterClaim c source name fetch =
    unwrapResolved =<< Cache.resolveMetadataWith afterClaim noopMetricsPort c source name (Right <$> fetch)

-- | The success-path wrappers' unwrap: a 'Left' from a cannot-fail fetch is a test bug.
unwrapResolved :: Either MetadataError a -> IO a
unwrapResolved = either (throwIO . UnexpectedFault) pure

-- | The typed wrapper for a 'Left' no success-path case expects (see 'resolveMetadata').
newtype UnexpectedFault = UnexpectedFault MetadataError
    deriving stock (Show)

instance Exception UnexpectedFault

-- | The typed escape the invariant-channel case throws from inside a leader's fetch.
data LeaderEscaped = LeaderEscaped
    deriving stock (Eq, Show)

instance Exception LeaderEscaped

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

    describe "resolveMetadata -- collapse" $ do
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

    describe "resolveMetadata -- typed failure channel" $ do
        it "hands the leader's Left to every coalesced follower, caching nothing" $ do
            -- The leader's fetch reports a typed failure; every concurrent waiter must
            -- receive that same value (never an exception, never a re-lead), and the
            -- store must stay empty so the next resolution fetches afresh.
            c <- freshCache
            calls <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            let failing = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    _ <- tryPutMVar started ()
                    takeMVar release
                    pure (Left MetadataUndecodable)
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (Cache.resolveMetadata noopMetricsPort c publicSource (pkg "shared-fault") failing)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release ()
                    )
            results `shouldBe` replicate 8 (Left MetadataUndecodable)
            readIORef calls `shouldReturn` 1
            cachedMetadata c publicSource (pkg "shared-fault") `shouldReturn` Nothing

        it "re-raises a synchronously escaping leader to its followers (the invariant channel)" $ do
            -- The fetch's contract is total, so a synchronous escape is an invariant
            -- break: the follower must see the exception re-raised, not a value, and
            -- the slot must still free for a later caller.
            result <- timeout 5_000_000 $ do
                c <- freshCache
                started <- newEmptyMVar
                release <- newEmptyMVar
                let escaping = do
                        putMVar started ()
                        () <- takeMVar release
                        throwIO LeaderEscaped
                leader <- async (try (resolveMetadata c publicSource (pkg "escape") escaping) :: IO (Either LeaderEscaped CacheEntry))
                takeMVar started
                follower <- async (try (resolveMetadata c publicSource (pkg "escape") escaping) :: IO (Either LeaderEscaped CacheEntry))
                threadDelay 30000 -- give the follower time to register on the marker
                putMVar release ()
                (,) <$> wait leader <*> wait follower
            case result of
                Nothing -> expectationFailure "wedged: an escaping leader parked its follower"
                Just (leaderOutcome, followerOutcome) -> do
                    leaderOutcome `shouldBe` Left LeaderEscaped
                    followerOutcome `shouldBe` Left LeaderEscaped

    describe "resolveMetadata -- single-flight orphan window" $ do
        it "unblocks a waiting follower and lets a later caller re-lead when the leader is cancelled at the claim handoff" $ do
            -- Regression: an async exception (request timeout, killed handler thread)
            -- landing on the leader between claiming the in-flight slot and completing
            -- must still fill the marker with the error and free the slot, or the
            -- waiting follower parks on the marker forever and the key wedges until
            -- restart. The window is otherwise a smooth, interruptible point, so it
            -- is driven deterministically through the 'resolveMetadataWith' hook, which
            -- runs on the leading thread at exactly the claim -> fetch-runner handoff:
            -- it signals it has reached the window, then parks, so the test can cancel
            -- the leader there. Everything that could wedge is wrapped in a 'timeout' so
            -- a regression fails fast instead of hanging the suite.
            result <- timeout 5_000_000 $ do
                c <- freshCache
                calls <- newIORef (0 :: Int)
                reached <- newEmptyMVar
                release <- newEmptyMVar
                armed <- newIORef True -- only the first (cancelled) leader parks
                let fetch = countingFetch calls (pkg "wedge") "raw"
                    afterClaim = do
                        wasArmed <- atomicModifyIORef' armed (False,)
                        when wasArmed $ do
                            putMVar reached () -- claimed the slot; parked at the handoff
                            takeMVar release -- block interruptibly so the cancel lands here
                            -- The leader claims the slot and parks at the handoff, holding it.
                leader <- async (resolveMetadataWith afterClaim c publicSource (pkg "wedge") fetch)
                takeMVar reached
                -- A follower arrives while the slot is held; it must become a follower
                -- on the marker, not re-lead. (It will block on the marker until the
                -- cancelled leader fills it.)
                follower <- async (try (resolveMetadata c publicSource (pkg "wedge") fetch) :: IO (Either SomeException CacheEntry))
                threadDelay 30000 -- give the follower time to register on the marker
                cancel leader -- cancel in the handoff window; the slot must still free
                wait follower
            case result of
                Nothing -> expectationFailure "wedged: a cancelled leader orphaned the in-flight slot"
                Just (Left _) -> expectationFailure "follower failed instead of recovering"
                Just (Right e) -> infoName (entryInfo e) `shouldBe` pkg "wedge"

        it "frees the slot for a later caller when the leader's fetch is cancelled mid-flight" $ do
            -- The mid-fetch analogue: the async exception lands while the leader is
            -- inside the fetch (under restore). The marker fill and de-register must
            -- still run, so a subsequent caller re-leads and fetches rather than
            -- finding a stuck slot.
            result <- timeout 5_000_000 $ do
                c <- freshCache
                calls <- newIORef (0 :: Int)
                started <- newEmptyMVar
                release <- newEmptyMVar
                let blockingFetch = do
                        atomicModifyIORef' calls (\n -> (n + 1, ()))
                        putMVar started () -- in the fetch (slot claimed)
                        () <- takeMVar release -- block so the leader can be cancelled here
                        pure (entry (pkg "midflight") "unreached")
                leader <- async (resolveMetadata c publicSource (pkg "midflight") blockingFetch)
                takeMVar started -- the leader holds the slot and is inside the fetch
                cancel leader -- async-cancel mid-fetch; the slot must still free
                -- A fresh caller must not wedge: with the slot freed it re-leads.
                recovered <- resolveMetadata c publicSource (pkg "midflight") (countingFetch calls (pkg "midflight") "raw")
                n <- readIORef calls
                pure (infoName (entryInfo recovered), n)
            case result of
                Nothing -> expectationFailure "wedged: a mid-flight cancel orphaned the in-flight slot"
                Just (recoveredName, n) -> do
                    recoveredName `shouldBe` pkg "midflight"
                    n `shouldBe` 2 -- the cancelled fetch and the recovering re-lead, no caching of the failure
    describe "size bound" $ do
        it "never exceeds the configured maximum entry count" $ do
            c <- newMetadataCache (config 60 4)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "raw"))
            n <- cacheSize c
            n `shouldSatisfy` (<= 4)

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

        it "keeps serving fresh resolutions even under eviction pressure" $ do
            c <- newMetadataCache (config 60 2)
            for_ [1 .. 10 :: Int] $ \i ->
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "raw"))
            result <- resolveMetadata c publicSource (pkg "final") (pure (entry (pkg "final") "raw"))
            infoName (entryInfo result) `shouldBe` pkg "final"

    describe "resident-byte budget" $ do
        it "evicts to keep the resident estimate under the byte budget" $ do
            -- A budget that holds three entries (the entry count is generous, so the byte
            -- budget is the binding bound): resolving many distinct packages must not let
            -- the resident estimate exceed it.
            let held = 3
            c <- newMetadataCache (configBytes 60 1000 (held * entryWeight + entryWeight `div` 2))
            for_ [1 .. 20 :: Int] $ \i ->
                resolveMetadata c publicSource (pkg (show i)) (pure (entry (pkg (show i)) "raw"))
            n <- cacheSize c
            (n * entryWeight) `shouldSatisfy` (<= held * entryWeight + entryWeight `div` 2)
            n `shouldSatisfy` (<= held)

        it "retains a repeatedly-accessed entry while evicting the one-shot tail" $ do
            -- The hot head survives pressure: a budget that holds a few entries, a hot key
            -- re-accessed on every round, and a long tail of one-shot keys. The hot key is
            -- most-recently-used each round, so the least-recently-used eviction sheds the
            -- cold tail and never the head.
            let held = 3
            c <- newMetadataCache (configBytes 60 1000 (held * entryWeight + entryWeight `div` 2))
            _ <- resolveMetadata c publicSource (pkg "hot") (pure (entry (pkg "hot") "raw"))
            for_ [1 .. 30 :: Int] $ \i -> do
                -- Touch the hot key (a hit, bumping its recency), then insert a one-shot.
                _ <- resolveMetadata c publicSource (pkg "hot") (pure (entry (pkg "hot") "unused"))
                resolveMetadata c publicSource (pkg ("cold-" <> show i)) (pure (entry (pkg ("cold-" <> show i)) "raw"))
            hot <- cachedMetadata c publicSource (pkg "hot")
            firstCold <- cachedMetadata c publicSource (pkg "cold-1")
            fmap (infoName . entryInfo) hot `shouldBe` Just (pkg "hot")
            firstCold `shouldBe` Nothing

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
