-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | One adapter owns all retained representations, with no hidden local fallback.
module Ecluse.Core.Server.Cache.ProviderSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.Metadata (Manifest (..), VersionRead (..))
import Ecluse.Core.Server.Cache
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage), RetentionOperations)
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Package (npmVersion, pypiVersion, sampleManifest, scopedNpm, thingName, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Cache (externalOperations)
import Ecluse.Test.Snapshot (untaggedRead)

data AdapterFault = AdapterFault
    deriving stock (Show)

instance Exception AdapterFault

data RecordingStore value = RecordingStore
    { rsOperations :: RetentionOperations Text value
    , rsValues :: IORef (Map Text value)
    , rsReads :: IORef Int
    , rsWrites :: IORef Int
    }

recordingStore :: IO (RecordingStore value)
recordingStore = do
    values <- newIORef Map.empty
    reads <- newIORef 0
    writes <- newIORef 0
    let readValue _ key = modifyIORef' reads (+ 1) >> Map.lookup key <$> readIORef values
        writeValue key value = modifyIORef' writes (+ 1) >> modifyIORef' values (Map.insert key value)
    pure (RecordingStore (externalOperations readValue writeValue) values reads writes)

sampleEntry :: CacheEntry
sampleEntry = CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestDigest manifest)
  where
    manifest = sampleManifest thingName [v1_0_0]

source :: Source
source = Source "https://public.example"

spec :: Spec
spec = describe "cacheProvider" $ do
    it "uses the chosen adapter for every representation and observes its invalidation" $ do
        full <- recordingStore
        version <- recordingStore
        assembled <- recordingStore
        calls <- newIORef (0 :: Int)
        let provider = cacheProvider (ExternalStorage 100000) (Just (rsOperations full)) (Just (rsOperations version)) (Just (rsOperations assembled))
            fetch value = modifyIORef' calls (+ 1) $> value
            selected = untaggedRead Nothing
        cache <- newMetadataCacheWithProvider provider
        replicateM_ 2 $ do
            resolveMetadata noopMetricsPort cache source thingName (fetch (Right sampleEntry)) `shouldReturn` Right sampleEntry
            resolveVersion noopMetricsPort cache source thingName v1_0_0 (fetch (Right selected)) `shouldReturn` Right selected
            resolveAssembled noopMetricsPort cache "digest" (fetch "assembled") `shouldReturn` "assembled"
        readIORef calls `shouldReturn` 3
        traverse readIORef [rsReads full, rsReads version, rsReads assembled] `shouldReturn` [2, 2, 2]
        traverse readIORef [rsWrites full, rsWrites version, rsWrites assembled] `shouldReturn` [1, 1, 1]
        writeIORef (rsValues full) Map.empty
        writeIORef (rsValues version) Map.empty
        writeIORef (rsValues assembled) Map.empty
        resolveMetadata noopMetricsPort cache source thingName (fetch (Right sampleEntry)) `shouldReturn` Right sampleEntry
        resolveVersion noopMetricsPort cache source thingName v1_0_0 (fetch (Right selected)) `shouldReturn` Right selected
        resolveAssembled noopMetricsPort cache "digest" (fetch "fresh") `shouldReturn` "fresh"
        readIORef calls `shouldReturn` 6

    it "leaves absent capabilities uncached instead of allocating local stores" $ do
        cache <- newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) Nothing Nothing Nothing)
        calls <- newIORef (0 :: Int)
        let fetch value = modifyIORef' calls (+ 1) $> value
        replicateM_ 2 $ do
            _ <- resolveMetadata noopMetricsPort cache source thingName (fetch (Right sampleEntry))
            _ <- resolveVersion noopMetricsPort cache source thingName v1_0_0 (fetch (Right (untaggedRead Nothing)))
            resolveAssembled noopMetricsPort cache "digest" (fetch "assembled") `shouldReturn` "assembled"
        readIORef calls `shouldReturn` 6

    it "refetches after adapter failure without retaining the origin result locally" $ do
        calls <- newIORef (0 :: Int)
        failures <- newIORef (0 :: Int)
        let failedOperations = externalOperations (\_ _ -> throwIO AdapterFault) (\_ _ -> throwIO AdapterFault)
            provider = cacheProvider (ExternalStorage 100000) (Just failedOperations) (Just failedOperations) (Just failedOperations)
            metrics = noopMetricsPort{mpCacheRefused = \_ -> modifyIORef' failures (+ 1)}
            fetch value = modifyIORef' calls (+ 1) $> value
        cache <- newMetadataCacheWithProvider provider
        replicateM_ 2 $ do
            resolveMetadata metrics cache source thingName (fetch (Right sampleEntry)) `shouldReturn` Right sampleEntry
            resolveVersion metrics cache source thingName v1_0_0 (fetch (Right (untaggedRead Nothing))) `shouldReturn` Right (untaggedRead Nothing)
            resolveAssembled metrics cache "digest" (fetch "assembled") `shouldReturn` "assembled"
        readIORef calls `shouldReturn` 6
        readIORef failures `shouldReturn` 12

    it "partitions selected identities by source, ecosystem, scope, package, and version" $ do
        version <- recordingStore
        cache <- newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) Nothing (Just (rsOperations version)) Nothing)
        let identities =
                [ (source, unscopedNpm "shared", npmVersion "1")
                , (Source "https://other.example", unscopedNpm "shared", npmVersion "1")
                , (source, unscopedPyPI "shared", pypiVersion "1")
                , (source, scopedNpm "scope" "shared", npmVersion "1")
                , (source, unscopedNpm "different", npmVersion "1")
                , (source, unscopedNpm "shared", npmVersion "2")
                ]
        for_ (zip identities [1 ..]) $ \((origin, name, release), marker) -> do
            let value = (untaggedRead Nothing){vrBodyBytes = marker}
            resolveVersion noopMetricsPort cache origin name release (pure (Right value)) `shouldReturn` Right value
        for_ (zip identities [1 ..]) $ \((origin, name, release), marker) -> do
            let value = (untaggedRead Nothing){vrBodyBytes = marker}
            resolveVersion noopMetricsPort cache origin name release (throwIO AdapterFault) `shouldReturn` Right value
        readIORef (rsWrites version) `shouldReturn` length identities
