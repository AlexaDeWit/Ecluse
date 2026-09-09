-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Metadata caching and failure observations across full and selective reads.
Failures remain uncached and retain their typed cause.
-}
module Ecluse.Core.Server.MetadataSpec (spec) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Test.Hspec
import UnliftIO (concurrently, mapConcurrently)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    InvalidEntry,
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (TrustUnknown),
 )
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataFetch, MetadataHttpFailure, MetadataUndecodable),
    digestOf,
 )
import Ecluse.Core.Server.Cache (MetadataCache, Source (Source), cachedMetadata, newMetadataCache)
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached, Uncached), newMetadataClient)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpUpstreamFetchError))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Cache (defaultCacheConfig)

-- | Tests for the serve-path read handle, whose single-version op is hybrid.
spec :: Spec
spec = do
    describe "newMetadataClient -- single-version hybrid topology" $ do
        it "reuses the warm full-packument cache: a GET then its version select is one upstream call" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0", "2.0.0"]
                client = publicClient cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 1
            found <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) found `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1

        it "cold: leads a selective single-version fetch, caches it, and a repeat hits the version cache" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient cache (countingFull calls info) (countingVersion calls info)
            cold <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) cold `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1
            warmHit <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) warmHit `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1
            -- The cold single-version path stays isolated on writes: it never populated the
            -- shared full-packument cache (only the version cache).
            cachedMetadata cache source name `shouldReturn` Nothing

        it "caches a determined absence: an absent version is a Nothing re-served without a re-fetch" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient cache (countingFull calls info) (countingVersion calls info)
            absent <- fetchVersionMetadata client name (ver "2.0.0")
            fmap (fmap pkgVersion) absent `shouldBe` Right Nothing
            readIORef calls `shouldReturn` 1
            absentHit <- fetchVersionMetadata client name (ver "2.0.0")
            fmap (fmap pkgVersion) absentHit `shouldBe` Right Nothing
            readIORef calls `shouldReturn` 1

    describe "newMetadataClient -- caching policy" $
        it "an uncached handle fetches on every call (the per-client private origin)" $ do
            calls <- newIORef (0 :: Int)
            let info = manifest name ["1.0.0"]
                client =
                    newMetadataClient noopMetricsPort Metric.Private Uncached noLog noInvalidLog noFetchLog (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest client name
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 2

    describe "newMetadataClient -- failure propagation" $ do
        for_ httpFailures $ \(refusal, expectedCause) ->
            it ("records and preserves " <> show refusal <> " on every read") $ do
                causes <- newIORef []
                failures <- newIORef []
                let port = noopMetricsPort{mpUpstreamFetchError = \upstream cause -> modifyIORef' causes ((upstream, cause) :)}
                    recordFailure who err = modifyIORef' failures ((who, err) :)
                    client = newMetadataClient port Metric.Private Uncached recordFailure noInvalidLog noFetchLog (const (pure (Left refusal))) (\_ _ -> pure (Left refusal))
                replicateM_ 2 $ do
                    full <- fetchFullManifest client name
                    void full `shouldBe` Left refusal
                    single <- fetchVersionMetadata client name (ver "1.0.0")
                    void single `shouldBe` Left refusal
                readIORef causes `shouldReturn` replicate 4 (Metric.Private, expectedCause)
                readIORef failures `shouldReturn` replicate 4 (name, refusal)

        for_ httpFailures $ \(failure, _) ->
            it ("caches neither full nor selective " <> show failure <> " responses") $ do
                calls <- newIORef (0 :: Int)
                cache <- newMetadataCache defaultCacheConfig
                let failRead _ = modifyIORef' calls (+ 1) $> Left failure
                    client = publicClient cache failRead (\_ _ -> failRead ())
                replicateM_ 2 $ do
                    full <- fetchFullManifest client name
                    void full `shouldBe` Left failure
                    single <- fetchVersionMetadata client name (ver "1.0.0")
                    void single `shouldBe` Left failure
                readIORef calls `shouldReturn` 4

        it "an unreachable upstream is not cached: the next resolve fetches afresh" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                outage = unreachableFull calls
                recovered = countingFull calls info
            first' <- fetchFullManifest (publicClient cache outage (failingVersion calls)) name
            isUnreachable first' `shouldBe` True
            cachedMetadata cache source name `shouldReturn` Nothing
            second' <- fetchFullManifest (publicClient cache recovered (failingVersion calls)) name
            fmap (infoName . manifestInfo) second' `shouldBe` Right name
            readIORef calls `shouldReturn` 2

        it "records the Connection error cause for an unreachable upstream" $ do
            calls <- newIORef (0 :: Int)
            causes <- newIORef ([] :: [Metric.Cause])
            cache <- newMetadataCache defaultCacheConfig
            let port = noopMetricsPort{mpUpstreamFetchError = \_ cause -> atomicModifyIORef' causes (\cs -> (cause : cs, ()))}
                client =
                    newMetadataClient port Metric.Public (Cached cache source) noLog noInvalidLog noFetchLog (unreachableFull calls) (failingVersion calls)
            _ <- fetchFullManifest client name
            readIORef causes `shouldReturn` [Metric.Connection]

        it "logs a failure once per real fetch: coalesced followers never re-log" $ do
            -- Coalesced followers share the failing leader's typed Left, and the failure log fires
            -- once inside the leader, never per follower.
            fetches <- newIORef (0 :: Int)
            failureLogs <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            cache <- newMetadataCache defaultCacheConfig
            let blockingOutage _name = do
                    atomicModifyIORef' fetches (\n -> (n + 1, ()))
                    _ <- tryPutMVar started ()
                    takeMVar release
                    pure (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))
                countingLog _name _err = atomicModifyIORef' failureLogs (\n -> (n + 1, ()))
                client =
                    newMetadataClient noopMetricsPort Metric.Public (Cached cache source) countingLog noInvalidLog noFetchLog blockingOutage (failingVersion fetches)
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (fetchFullManifest client name)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release ()
                    )
            map isUnreachable results `shouldBe` replicate 8 True
            readIORef fetches `shouldReturn` 1
            readIORef failureLogs `shouldReturn` 1

httpFailures :: [(MetadataError, Metric.Cause)]
httpFailures =
    [(MetadataAuthorisationFailure code, Metric.OtherCause) | code <- [401, 403]]
        <> [(MetadataAbsent, Metric.UpstreamStatus)]
        <> [(MetadataHttpFailure code, Metric.UpstreamStatus) | code <- [301, 400, 408, 429, 500, 503]]
        <> [(MetadataUndecodable, Metric.Decode)]

name :: PackageName
name = unscopedNpm "is-odd"

ver :: Text -> Version
ver = mkVersion Npm

source :: Source
source = Source "https://public.example"

noLog :: PackageName -> MetadataError -> IO ()
noLog _ _ = pure ()

noInvalidLog :: PackageName -> [InvalidEntry] -> IO ()
noInvalidLog _ _ = pure ()

noFetchLog :: PackageName -> IO ()
noFetchLog _ = pure ()

publicClient ::
    MetadataCache ->
    (PackageName -> IO (Either MetadataError Manifest)) ->
    (PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))) ->
    MetadataClient
publicClient cache =
    newMetadataClient noopMetricsPort Metric.Public (Cached cache source) noLog noInvalidLog noFetchLog

countingFull :: IORef Int -> PackageInfo -> PackageName -> IO (Either MetadataError Manifest)
countingFull calls info _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right Manifest{manifestInfo = info, manifestRaw = fst npmCached (String "raw"), manifestDigest = digestOf "raw-bytes"})

countingVersion :: IORef Int -> PackageInfo -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
countingVersion calls info _name version = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right (Map.lookup (renderVersion version) (infoVersions info)))

unreachableFull :: IORef Int -> PackageName -> IO (Either MetadataError Manifest)
unreachableFull calls _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))

isUnreachable :: Either MetadataError Manifest -> Bool
isUnreachable = \case
    Left (MetadataFetch (FetchTransport _)) -> True
    _ -> False

failingVersion :: IORef Int -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
failingVersion calls _name _version = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left MetadataUndecodable)

manifest :: PackageName -> [Text] -> PackageInfo
manifest who versions =
    PackageInfo
        { infoName = who
        , infoVersions = Map.fromList [(v, details who v) | v <- versions]
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

details :: PackageName -> Text -> PackageDetails
details who rawVer =
    PackageDetails
        { pkgName = who
        , pkgVersion = ver rawVer
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = TrustUnknown
        , pkgAvailability = Available
        , pkgArtifacts = artifact :| []
        , pkgLicenses = []
        , pkgPublisher = Nothing
        }
  where
    artifact =
        Artifact
            { artEntryKey = ObjectEntry rawVer
            , artFilename = "pkg-" <> rawVer <> ".tgz"
            , artUrl = "https://example.test/pkg-" <> rawVer <> ".tgz"
            , artKind = Tarball
            , artHashes = []
            , artSize = Nothing
            , artInterpreter = Nothing
            , artYanked = False
            , artProvenance = Nothing
            }
