-- SPDX-FileCopyrightText: 2026 Alexandra de Wit

-- SPDX-License-Identifier: MIT

{- | Cache resolution and occupancy checks over in-process fetches.
Selected-release checks use the production PyPI projection.
-}
module Ecluse.Core.Server.CacheSpec (spec) where

import Control.Exception (throw)
import Data.Aeson (Value (String), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (mapConcurrently, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), PackageInfo (..), PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionRead, digestOf)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache (
    CacheConfig (..),
    CacheEntry (..),
    MetadataCache,
    Source (..),
    StoreBudget (..),
    newMetadataCache,
 )
import Ecluse.Core.Server.Cache qualified as Cache
import Ecluse.Core.Server.Cache.Backend (BackendStorage (..))
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Package (npmVersion, pypiVersion, sampleArtifact, sampleDetails, thingName, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIVersion)
import Ecluse.Test.Server.Cache (cachedMetadata, cachedVersion, externalOperations, newLocalRetention, weighCacheEntry)
import Ecluse.Test.Snapshot (readDetails, untaggedRead)

resolveMetadata :: MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadata c source name fetch =
    unwrapResolved =<< Cache.resolveMetadata noopMetricsPort c source name (Right <$> fetch)

unwrapResolved :: Either MetadataError a -> IO a
unwrapResolved = either (throwIO . UnexpectedFault) pure

newtype UnexpectedFault = UnexpectedFault MetadataError
    deriving stock (Show)

instance Exception UnexpectedFault

resolveAssembled :: MetadataCache -> Text -> IO ByteString -> IO ByteString
resolveAssembled = Cache.resolveAssembled noopMetricsPort

countingRender :: IORef Int -> ByteString -> IO ByteString
countingRender renders bytes = do
    atomicModifyIORef' renders (\n -> (n + 1, ()))
    pure bytes

mkBytes :: Int -> Char -> ByteString
mkBytes n c = BS.replicate n (fromIntegral (ord c))

privateSource, publicSource :: Source
privateSource = Source "https://private.example"
publicSource = Source "https://public.example"

info :: PackageName -> PackageInfo
info name =
    PackageInfo
        { infoName = name
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

entry :: PackageName -> Text -> CacheEntry
entry name marker = CacheEntry{entryInfo = info name, entryRaw = cachedRaw marker, entryBodyBytes = BS.length (encodeUtf8 marker), entryDigest = digestOf (encodeUtf8 marker)}

cachedRaw :: Text -> CachedDoc
cachedRaw = fst npmCached . String

config :: NominalDiffTime -> Int -> CacheConfig
config ttl size = configBytes ttl size (1024 * 1024 * 1024)

configBytes :: NominalDiffTime -> Int -> Int -> CacheConfig
configBytes ttl size bytes =
    CacheConfig
        { cacheTtl = ttl
        , cacheMaxEntries = size
        , cacheMaxBytes = bytes
        , cacheVersionBudget = budget
        , cacheAssembledBudget = budget
        }
  where
    budget = StoreBudget 0 0

recordingResidencyPort :: IO (MetricsPort, IO (Maybe Int))
recordingResidencyPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpCacheResidentBytes = writeIORef seen . Just}
    pure (port, readIORef seen)

recordingEntriesPort :: IO (MetricsPort, IO (Maybe Int))
recordingEntriesPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpCacheEntries = writeIORef seen . Just}
    pure (port, readIORef seen)

recordingVersionResidencyPort :: IO (MetricsPort, IO (Maybe Int))
recordingVersionResidencyPort = do
    seen <- newIORef Nothing
    let port = noopMetricsPort{mpVersionCacheResidentBytes = writeIORef seen . Just}
    pure (port, readIORef seen)

freshCache :: IO MetadataCache
freshCache = newMetadataCache (config 60 100)

countingFetch :: IORef Int -> PackageName -> Text -> IO CacheEntry
countingFetch calls name marker = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (entry name marker)

pypiName :: PackageName
pypiName = unscopedPyPI "auditdemo"

selectedRelease :: Int -> Int -> Either MetadataError VersionRead
selectedRelease count padding =
    fmap untaggedRead
        . projectPyPIVersion defaultLimits pypiName (pypiVersion "1")
        . BL.toStrict
        . encode
        $ object ["name" .= ("auditdemo" :: Text), "files" .= map file [1 .. count]]
  where
    file i =
        withFileKeys
            [("url", String ("https://files.pythonhosted.org/" <> T.replicate padding "x" <> "/" <> filename i))]
            (simpleFile (filename i))
    filename i = "auditdemo-1-" <> show i <> "-py3-none-any.whl"

spec :: Spec
spec = do
    describe "source size and cache weighting" $
        it "retains source size without changing the existing cache charge" $ do
            let original = entry (unscopedNpm "weight-probe") "raw"
                measured = original{entryBodyBytes = 1024 * 1024}
            weighCacheEntry measured `shouldBe` weighCacheEntry original

    describe "full-document entry-coordinate accounting" $
        it "charges retained key backing allocations in the typed view" $ do
            let backing = T.replicate 65536 "x"
                sliced = T.take 1 backing
                withKey key =
                    (entry thingName "raw")
                        { entryInfo = (info thingName){infoVersions = Map.singleton "1.0.0" ((sampleDetails thingName v1_0_0){pkgArtifacts = sampleArtifact{artEntryKey = ObjectEntry key} :| []})}
                        }
            weighCacheEntry (withKey sliced) `shouldSatisfy` (>= weighCacheEntry (withKey (T.copy sliced)) + 65535)

    describe "selected PyPI release accounting" $ do
        for_ [(1, 16), (100, 16), (100, 2048), (1000, 16)] $ \(files, urlLength) ->
            it ("reports retained bytes for " <> show files <> " files with URL padding " <> show urlLength) $ do
                release <- unwrapResolved (selectedRelease files urlLength)
                (length . pkgArtifacts <$> readDetails release) `shouldBe` Just files
                let accounted = weighVersion release
                accounted `shouldSatisfy` (> files * urlLength)
                (port, readResidency) <- recordingVersionResidencyPort
                c <- newMetadataCache (configBytes 60 100 accounted)
                Cache.resolveVersion port c publicSource pypiName (pypiVersion "1") (pure (Right release)) `shouldReturn` Right release
                cachedVersion noopMetricsPort c publicSource pypiName (pypiVersion "1") `shouldReturn` Just release
                readResidency `shouldReturn` Just accounted

        it "serves an oversized selected release without evicting the cached absence" $ do
            release <- unwrapResolved (selectedRelease 1000 128)
            (port, readResidency) <- recordingVersionResidencyPort
            calls <- newIORef (0 :: Int)
            let absentVersion = pypiVersion "2"
                fetch = modifyIORef' calls (+ 1) $> Right release
            c <- newMetadataCache (configBytes 60 100 16384)
            _ <- Cache.resolveVersion port c publicSource pypiName absentVersion (pure (Right (untaggedRead Nothing)))
            replicateM_ 2 $ Cache.resolveVersion port c publicSource pypiName (pypiVersion "1") fetch `shouldReturn` Right release
            cachedVersion noopMetricsPort c publicSource pypiName (pypiVersion "1") `shouldReturn` Nothing
            cachedVersion noopMetricsPort c publicSource pypiName absentVersion `shouldReturn` Just (untaggedRead Nothing)
            readIORef calls `shouldReturn` 2
            readResidency `shouldReturn` Just 1024

    describe "local full retention" $ do
        for_ [0, 1, maxBound] $ \capacity ->
            it ("fetches again without retention at capacity " <> show capacity) $ do
                c <- newMetadataCache (configBytes 60 capacity capacity)
                calls <- newIORef 0
                replicateM_ 2 $ resolveMetadata c publicSource thingName (countingFetch calls thingName "raw")
                readIORef calls `shouldReturn` 2
                cachedMetadata noopMetricsPort c publicSource thingName `shouldReturn` Nothing

        it "excludes an explicitly supplied local backend without evaluating its weigher" $ do
            operations <- newLocalRetention 60 maxBound maxBound (\_ -> throw (UnexpectedFault MetadataUndecodable))
            c <- Cache.newMetadataCacheWithProvider (cacheProvider LocalStorage (Just operations) Nothing Nothing)
            resolveMetadata c publicSource thingName (pure (entry thingName "raw")) `shouldReturn` entry thingName "raw"
            cachedMetadata noopMetricsPort c publicSource thingName `shouldReturn` Nothing

        it "reports zero full occupancy without counting a capacity refusal" $ do
            (residencyPort, readResidency) <- recordingResidencyPort
            (entryPort, readEntries) <- recordingEntriesPort
            refused <- newIORef (0 :: Int)
            let port = residencyPort{mpCacheEntries = mpCacheEntries entryPort, mpCacheRefused = \_ -> modifyIORef' refused (+ 1)}
            c <- freshCache
            Cache.resolveMetadata port c publicSource thingName (pure (Right (entry thingName "raw"))) `shouldReturn` Right (entry thingName "raw")
            readResidency `shouldReturn` Just 0
            readEntries `shouldReturn` Just 0
            readIORef refused `shouldReturn` 0

        it "shares one active full fetch and retains nothing after both callers finish" $ do
            result <- timeout 1000000 $ do
                c <- freshCache
                started <- newEmptyMVar
                joined <- newEmptyMVar
                release <- newEmptyMVar
                let expected = entry thingName "shared"
                    port = noopMetricsPort{mpCacheRequest = \request -> when (request == Metric.Collapsed) (putMVar joined ())}
                    fetch = putMVar started () >> takeMVar release $> Right expected
                    run = Cache.resolveMetadata port c publicSource thingName
                withAsync (run fetch) $ \leader -> do
                    takeMVar started
                    withAsync (run fetch) $ \follower -> do
                        takeMVar joined
                        putMVar release ()
                        wait leader `shouldReturn` Right expected
                        wait follower `shouldReturn` Right expected
                cachedMetadata noopMetricsPort c publicSource thingName `shouldReturn` Nothing
                run (pure (Right (entry thingName "next"))) `shouldReturn` Right (entry thingName "next")
            result `shouldBe` Just ()

    describe "optional external full retention" $ do
        it "partitions retained values by source, ecosystem, and package" $ do
            values <- newIORef Map.empty
            let operations = externalOperations (\_ key -> Map.lookup key <$> readIORef values) (\key value -> modifyIORef' values (Map.insert key value))
                identities = [(publicSource, thingName), (privateSource, thingName), (publicSource, unscopedPyPI "thing"), (publicSource, unscopedNpm "other")]
            c <- Cache.newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) (Just operations) Nothing Nothing)
            for_ (zip identities [1 ..]) $ \((source, name), marker :: Int) -> do
                let expected = entry name (show marker)
                resolveMetadata c source name (pure expected) `shouldReturn` expected
            for_ (zip identities [1 ..]) $ \((source, name), marker :: Int) -> do
                cachedMetadata noopMetricsPort c source name `shouldReturn` Just (entry name (show marker))
                resolveMetadata c source name (throwIO (UnexpectedFault MetadataUndecodable)) `shouldReturn` entry name (show marker)

        it "preserves origin success and reports a failed backend read and write" $ do
            failures <- newIORef []
            let operations = externalOperations (\_ _ -> throwIO (UnexpectedFault MetadataUndecodable)) (\_ _ -> throwIO (UnexpectedFault MetadataUndecodable))
                port = noopMetricsPort{mpCacheRefused = \store -> modifyIORef' failures (store :)}
                expected = entry thingName "fresh"
            c <- Cache.newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) (Just operations) Nothing Nothing)
            Cache.resolveMetadata port c publicSource thingName (pure (Right expected)) `shouldReturn` Right expected
            readIORef failures `shouldReturn` [Metric.FullStore, Metric.FullStore]

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

            let bigBytes = 4096
                budget = bigBytes + 1024
            c <- newMetadataCache (configBytes 60 8 budget)
            _ <- resolveAssembled c "\"tag-a\"" (countingRender renders (mkBytes bigBytes 'a'))
            _ <- resolveAssembled c "\"tag-b\"" (countingRender renders (mkBytes bigBytes 'b'))
            _ <- resolveAssembled c "\"tag-a\"" (countingRender renders (mkBytes bigBytes 'a'))
            readIORef renders `shouldReturn` 3

    describe "per-store telemetry" $ do
        it "records misses and retained hits independently for all three stores" $ do
            full <- newIORef []
            version <- newIORef []
            assembled <- newIORef []
            let port =
                    noopMetricsPort
                        { mpCacheRequest = \r -> modifyIORef' full (r :)
                        , mpVersionCacheRequest = \r -> modifyIORef' version (r :)
                        , mpAssembledCacheRequest = \r -> modifyIORef' assembled (r :)
                        }
                name = unscopedNpm "observed"
            c <- newMetadataCache (config 60 8)
            replicateM_ 2 $ do
                _ <- Cache.resolveMetadata port c publicSource name (pure (Right (entry name "raw")))
                _ <- Cache.resolveVersion port c publicSource name v1_0_0 (pure (Right (untaggedRead Nothing)))
                Cache.resolveAssembled port c "assembled" (pure "raw")
            readIORef full `shouldReturn` [Metric.Miss, Metric.Miss]
            for_ [version, assembled] $ \seen ->
                readIORef seen `shouldReturn` [Metric.Hit, Metric.Miss]

        it "attributes oversized non-retention to the store that refused it" $ do
            refused <- newIORef []
            let port = noopMetricsPort{mpCacheRefused = \store -> modifyIORef' refused (store :)}
                name = unscopedNpm "oversized"
            c <- newMetadataCache (configBytes 60 8 1)
            replicateM_ 2 $ do
                Cache.resolveMetadata port c publicSource name (pure (Right (entry name "raw")))
                    `shouldReturn` Right (entry name "raw")
                Cache.resolveVersion port c publicSource name v1_0_0 (pure (Right (untaggedRead Nothing)))
                    `shouldReturn` Right (untaggedRead Nothing)
                Cache.resolveAssembled port c "assembled" (pure "raw") `shouldReturn` "raw"
            readIORef refused `shouldReturn` concat (replicate 2 [Metric.AssembledStore, Metric.VersionStore])

        it "reports assembled expiry before retaining a replacement" $ do
            seen <- newIORef []
            let port = noopMetricsPort{mpAssembledCacheResidentBytes = \bytes -> modifyIORef' seen (bytes :)}
            c <- newMetadataCache (config 0 8)
            Cache.resolveAssembled port c "expired" (pure "raw") `shouldReturn` "raw"
            weight <- sum <$> readIORef seen
            weight `shouldSatisfy` (> 0)
            threadDelay 1000
            Cache.resolveAssembled port c "expired" (pure "new") `shouldReturn` "new"
            readIORef seen `shouldReturn` [weight, 0, weight]

        it "keeps full residency zero and reports selected-version expiry" $ do
            full <- newIORef 0
            version <- newIORef 0
            entries <- newIORef 0
            let port =
                    noopMetricsPort
                        { mpCacheResidentBytes = writeIORef full
                        , mpVersionCacheResidentBytes = writeIORef version
                        , mpCacheEntries = writeIORef entries
                        }
                name = unscopedNpm "expired"
            c <- newMetadataCache (config 0 8)
            _ <- Cache.resolveMetadata port c publicSource name (pure (Right (entry name "raw")))
            _ <- Cache.resolveVersion port c publicSource name v1_0_0 (pure (Right (untaggedRead Nothing)))
            readIORef full `shouldReturn` 0
            readIORef version >>= (`shouldSatisfy` (> 0))
            threadDelay 1000
            cachedMetadata port c publicSource name `shouldReturn` Nothing
            cachedVersion port c publicSource name v1_0_0 `shouldReturn` Nothing
            traverse readIORef [full, version, entries] `shouldReturn` [0, 0, 0]

    describe "the pooled local budget" $ do
        it "shares the entry bound across selected and assembled capabilities" $ do
            c <- newMetadataCache (config 60 1)
            calls <- newIORef (0 :: Int)
            let name = unscopedNpm "shared-bound"
                render = modifyIORef' calls (+ 1) $> "body"
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name v1_0_0 (pure (Right (untaggedRead Nothing)))
            replicateM_ 2 (resolveAssembled c "digest" render `shouldReturn` "body")
            readIORef calls `shouldReturn` 2
            cachedVersion noopMetricsPort c publicSource name v1_0_0 `shouldReturn` Just (untaggedRead Nothing)

        it "a version-store flood preserves assembled entries without retaining full metadata" $ do
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheMaxEntries = 3
                        , cacheMaxBytes = 1024 * 1024
                        , cacheVersionBudget = StoreBudget 0 0
                        , cacheAssembledBudget = StoreBudget 0 0
                        }
            let name = unscopedNpm "hot-head"
            _ <- resolveMetadata c publicSource name (pure (entry name "raw"))
            _ <- resolveAssembled c "stable" (pure "assembled")
            for_ ([1 .. 5] :: [Int]) $ \i ->
                Cache.resolveVersion noopMetricsPort c publicSource name (npmVersion (show i <> ".0.0")) (pure (Right (untaggedRead Nothing)))

            cachedVersion noopMetricsPort c publicSource name (npmVersion "1.0.0") `shouldReturn` Nothing

            found <- cachedMetadata noopMetricsPort c publicSource name
            found `shouldBe` Nothing
            resolveAssembled c "stable" (pure "wrong") `shouldReturn` "assembled"

        it "bounds eligible residency while full occupancy remains zero" $ do
            fullSeen <- newIORef 0
            versionSeen <- newIORef 0
            assembledSeen <- newIORef 0
            let port =
                    noopMetricsPort
                        { mpCacheResidentBytes = writeIORef fullSeen
                        , mpVersionCacheResidentBytes = writeIORef versionSeen
                        , mpAssembledCacheResidentBytes = writeIORef assembledSeen
                        }
                aggregateBytes = 72 * 1024
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheMaxEntries = 100
                        , cacheMaxBytes = aggregateBytes
                        , cacheVersionBudget = StoreBudget 0 0
                        , cacheAssembledBudget = StoreBudget 0 0
                        }
            for_ ([1 .. 10] :: [Int]) $ \i -> do
                let name = unscopedNpm ("filler-" <> show i)
                _ <- Cache.resolveMetadata port c publicSource name (pure (Right (entry name "raw")))
                _ <- Cache.resolveVersion port c publicSource name (npmVersion "1.0.0") (pure (Right (untaggedRead Nothing)))
                _ <- Cache.resolveAssembled port c (show i) (pure (mkBytes 2048 'x'))
                pass
            readIORef fullSeen `shouldReturn` 0
            total <- sum <$> traverse readIORef [versionSeen, assembledSeen]
            total `shouldSatisfy` (<= aggregateBytes)

    describe "cachedVersion -- read recency" $
        it "a cachedVersion read bumps the version entry's recency, so a re-read entry survives eviction (LRU, not FIFO)" $ do
            c <-
                newMetadataCache
                    CacheConfig
                        { cacheTtl = 60
                        , cacheMaxEntries = 2
                        , cacheMaxBytes = 1024 * 1024
                        , cacheVersionBudget = StoreBudget 0 0
                        , cacheAssembledBudget = StoreBudget 0 0
                        }
            let name = unscopedNpm "recency"
                v n = npmVersion (show (n :: Int) <> ".0.0")
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 1) (pure (Right (untaggedRead Nothing)))
            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 2) (pure (Right (untaggedRead Nothing)))

            _ <- cachedVersion noopMetricsPort c publicSource name (v 1)

            _ <- Cache.resolveVersion noopMetricsPort c publicSource name (v 3) (pure (Right (untaggedRead Nothing)))

            cachedVersion noopMetricsPort c publicSource name (v 1) `shouldReturn` Just (untaggedRead Nothing)
            cachedVersion noopMetricsPort c publicSource name (v 2) `shouldReturn` Nothing
