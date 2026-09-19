-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Metadata caching and failure observations across full and selective reads.
Failures remain uncached and retain their typed cause. A version read pairs its typed view with
the raw object of the one body it came from, on every path of the hybrid.
-}
module Ecluse.Core.Server.MetadataSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec
import UnliftIO (concurrently, mapConcurrently)
import UnliftIO.Concurrent (threadDelay)

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
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataFetch, MetadataHttpFailure, MetadataUndecodable),
    VersionDoc (VersionDoc, vdDetails, vdRaw),
    VersionRead (VersionRead, vrUpstreamLatest, vrVersion),
    digestOf,
 )
import Ecluse.Core.Registry.Npm.Metadata (selectNpmVersionDoc)
import Ecluse.Core.Registry.Origin (OriginFor, Public, anonymousOrigin, perCallerOrigin)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (MetadataCache, Source (Source), cachedMetadata, newMetadataCache)
import Ecluse.Core.Server.Metadata (newMetadataReads, privateMetadataClient, publicMetadataClient, selectVersion)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpUpstreamFetchError))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Package (npmVersion, unscopedNpm)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Snapshot (readDetails)

-- | Tests for the serve-path read handle, whose single-version op is hybrid.
spec :: Spec
spec = do
    -- Every case here drives the read functions directly, so neither origin is ever dialled:
    -- only the posture its builder fixes is under test.
    manager <- runIO (newManager defaultManagerSettings)
    let anonymous = anonymousOrigin defaultLimits manager stubUrl
        perCaller = perCallerOrigin defaultLimits manager stubUrl Nothing

    describe "publicMetadataClient -- single-version hybrid topology" $ do
        it "reuses the warm full-packument cache: a GET then its version select is one upstream call" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0", "2.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 1
            found <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap pkgVersion . readDetails) found `shouldBe` Right (Just (npmVersion "1.0.0"))
            readIORef calls `shouldReturn` 1

        it "pairs a warm full-cache select with that entry's own raw object, with no upstream call" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0", "2.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest client name
            found <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap vdRaw . vrVersion) found `shouldBe` Right (Just (Just (markedObject "1.0.0")))
            readIORef calls `shouldReturn` 1

        it "keys a warm pair by version: a sibling select pairs its own raw object, never a neighbour's" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0", "2.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest client name
            older <- fetchVersionMetadata client name (npmVersion "1.0.0")
            newer <- fetchVersionMetadata client name (npmVersion "2.0.0")
            fmap (fmap vdRaw . vrVersion) older `shouldBe` Right (Just (Just (markedObject "1.0.0")))
            fmap (fmap vdRaw . vrVersion) newer `shouldBe` Right (Just (Just (markedObject "2.0.0")))
            fmap (fmap (pkgVersion . vdDetails) . vrVersion) newer `shouldBe` Right (Just (npmVersion "2.0.0"))
            readIORef calls `shouldReturn` 1

        it "cold: leads a selective single-version fetch, caches it, and a repeat hits the version cache" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            cold <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap pkgVersion . readDetails) cold `shouldBe` Right (Just (npmVersion "1.0.0"))
            readIORef calls `shouldReturn` 1
            warmHit <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap pkgVersion . readDetails) warmHit `shouldBe` Right (Just (npmVersion "1.0.0"))
            readIORef calls `shouldReturn` 1
            -- The cold single-version path stays isolated on writes: it never populated the
            -- shared full-packument cache (only the version cache).
            cachedMetadata cache source name `shouldReturn` Nothing

        it "re-serves the cold pair whole from the version cache: the selected raw object, no re-fetch" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            cold <- fetchVersionMetadata client name (npmVersion "1.0.0")
            warmHit <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap vdRaw . vrVersion) cold `shouldBe` Right (Just (Just (markedObject "cold")))
            warmHit `shouldBe` cold
            readIORef calls `shouldReturn` 1

        it "keeps a cached pair as built: a later full fetch never re-pairs it" $ do
            -- A pair's two sides always come from one fetch, so the version-cache hit wins over
            -- a full entry that arrived later, rather than mixing the two bodies.
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchVersionMetadata client name (npmVersion "1.0.0")
            _ <- fetchFullManifest client name
            again <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap (fmap vdRaw . vrVersion) again `shouldBe` Right (Just (Just (markedObject "cold")))
            readIORef calls `shouldReturn` 2

        it "partitions pairs by source: another origin's warm entry never pairs this origin's select" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                here = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
                elsewhere = publicClientAt (Source "https://other.example") anonymous cache (countingFull calls info) (countingVersion calls info)
            _ <- fetchFullManifest here name
            found <- fetchVersionMetadata elsewhere name (npmVersion "1.0.0")
            fmap (fmap vdRaw . vrVersion) found `shouldBe` Right (Just (Just (markedObject "cold")))
            readIORef calls `shouldReturn` 2

        it "carries the document's own latest on both the cold read and the warm select" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = tagged (manifest name ["1.0.0", "2.0.0"]) "2.0.0"
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            cold <- fetchVersionMetadata client name (npmVersion "1.0.0")
            fmap vrUpstreamLatest cold `shouldBe` Right (Just (npmVersion "2.0.0"))
            _ <- fetchFullManifest client name
            warm <- fetchVersionMetadata client name (npmVersion "2.0.0")
            fmap vrUpstreamLatest warm `shouldBe` Right (Just (npmVersion "2.0.0"))

        it "caches a determined absence: an absent version is a Nothing re-served without a re-fetch" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient anonymous cache (countingFull calls info) (countingVersion calls info)
            absent <- fetchVersionMetadata client name (npmVersion "2.0.0")
            fmap (fmap pkgVersion . readDetails) absent `shouldBe` Right Nothing
            readIORef calls `shouldReturn` 1
            absentHit <- fetchVersionMetadata client name (npmVersion "2.0.0")
            fmap (fmap pkgVersion . readDetails) absentHit `shouldBe` Right Nothing
            readIORef calls `shouldReturn` 1

    describe "the caching policy each builder settles" $
        it "an uncached handle fetches on every call (the per-client private origin)" $ do
            calls <- newIORef (0 :: Int)
            let info = manifest name ["1.0.0"]
                client =
                    privateMetadataClient (newMetadataReads noopMetricsPort noLog noInvalidLog noFetchLog (const (countingFull calls info)) (const (countingVersion calls info)) selectNpmVersionDoc perCaller)
            _ <- fetchFullManifest client name
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 2

    describe "metadata read handles -- failure propagation" $ do
        for_ httpFailures $ \(refusal, expectedCause) ->
            it ("records and preserves " <> show refusal <> " on every read") $ do
                causes <- newIORef []
                failures <- newIORef []
                let port = noopMetricsPort{mpUpstreamFetchError = \upstream cause -> modifyIORef' causes ((upstream, cause) :)}
                    recordFailure who err = modifyIORef' failures ((who, err) :)
                    client = privateMetadataClient (newMetadataReads port recordFailure noInvalidLog noFetchLog (\_ _ -> pure (Left refusal)) (\_ _ _ -> pure (Left refusal)) selectNpmVersionDoc perCaller)
                replicateM_ 2 $ do
                    full <- fetchFullManifest client name
                    void full `shouldBe` Left refusal
                    single <- fetchVersionMetadata client name (npmVersion "1.0.0")
                    void single `shouldBe` Left refusal
                readIORef causes `shouldReturn` replicate 4 (Metric.Private, expectedCause)
                readIORef failures `shouldReturn` replicate 4 (name, refusal)

        for_ httpFailures $ \(failure, _) ->
            it ("caches neither full nor selective " <> show failure <> " responses") $ do
                calls <- newIORef (0 :: Int)
                cache <- newMetadataCache defaultCacheConfig
                let failRead _ = modifyIORef' calls (+ 1) $> Left failure
                    client = publicClient anonymous cache failRead (\_ _ -> failRead ())
                replicateM_ 2 $ do
                    full <- fetchFullManifest client name
                    void full `shouldBe` Left failure
                    single <- fetchVersionMetadata client name (npmVersion "1.0.0")
                    void single `shouldBe` Left failure
                readIORef calls `shouldReturn` 4

        it "an unreachable upstream is not cached: the next resolve fetches afresh" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                outage = unreachableFull calls
                recovered = countingFull calls info
            first' <- fetchFullManifest (publicClient anonymous cache outage (failingVersion calls)) name
            isUnreachable first' `shouldBe` True
            cachedMetadata cache source name `shouldReturn` Nothing
            second' <- fetchFullManifest (publicClient anonymous cache recovered (failingVersion calls)) name
            fmap (infoName . manifestInfo) second' `shouldBe` Right name
            readIORef calls `shouldReturn` 2

        it "records the public upstream and the Connection error cause for an unreachable upstream" $ do
            calls <- newIORef (0 :: Int)
            causes <- newIORef ([] :: [(Metric.Upstream, Metric.Cause)])
            cache <- newMetadataCache defaultCacheConfig
            let port = noopMetricsPort{mpUpstreamFetchError = \upstream cause -> atomicModifyIORef' causes (\cs -> ((upstream, cause) : cs, ()))}
                client =
                    publicMetadataClient cache source (newMetadataReads port noLog noInvalidLog noFetchLog (const (unreachableFull calls)) (const (failingVersion calls)) selectNpmVersionDoc anonymous)
            _ <- fetchFullManifest client name
            readIORef causes `shouldReturn` [(Metric.Public, Metric.Connection)]

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
                    publicMetadataClient cache source (newMetadataReads noopMetricsPort countingLog noInvalidLog noFetchLog (const blockingOutage) (const (failingVersion fetches)) selectNpmVersionDoc anonymous)
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

source :: Source
source = Source "https://public.example"

noLog :: PackageName -> MetadataError -> IO ()
noLog _ _ = pure ()

noInvalidLog :: PackageName -> [InvalidEntry] -> IO ()
noInvalidLog _ _ = pure ()

noFetchLog :: PackageName -> IO ()
noFetchLog _ = pure ()

publicClient ::
    OriginFor Public ->
    MetadataCache ->
    (PackageName -> IO (Either MetadataError Manifest)) ->
    (PackageName -> Version -> IO (Either MetadataError VersionRead)) ->
    MetadataClient
publicClient = publicClientAt source

publicClientAt ::
    Source ->
    OriginFor Public ->
    MetadataCache ->
    (PackageName -> IO (Either MetadataError Manifest)) ->
    (PackageName -> Version -> IO (Either MetadataError VersionRead)) ->
    MetadataClient
publicClientAt at origin cache full version =
    publicMetadataClient cache at (newMetadataReads noopMetricsPort noLog noInvalidLog noFetchLog (const full) (const version) selectNpmVersionDoc origin)

-- The raw object the fixtures mark each version with, so a case can tell which body it came from.
markedObject :: Text -> CachedDoc
markedObject marker = fst npmCached (object ["marker" .= marker])

-- A never-dialled loopback origin, so no fixture here reaches the network.
stubUrl :: RegistryUrl
stubUrl = loopbackRegistryUrl "http://localhost:1"

-- The full fetch's raw document marks every version object with its own key.
countingFull :: IORef Int -> PackageInfo -> PackageName -> IO (Either MetadataError Manifest)
countingFull calls info _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right Manifest{manifestInfo = info, manifestRaw = fst npmCached packument, manifestDigest = digestOf "raw-bytes"})
  where
    packument :: Value
    packument = object ["versions" .= object [Key.fromText v .= object ["marker" .= v] | v <- Map.keys (infoVersions info)]]

-- The selective fetch marks its raw object as the cold path's.
countingVersion :: IORef Int -> PackageInfo -> PackageName -> Version -> IO (Either MetadataError VersionRead)
countingVersion calls info _name version = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure . Right $
        VersionRead
            { vrVersion = (\selected -> VersionDoc{vdDetails = selected, vdRaw = Just (markedObject "cold")}) <$> selectVersion version info
            , vrUpstreamLatest = Map.lookup "latest" (infoDistTags info)
            }

unreachableFull :: IORef Int -> PackageName -> IO (Either MetadataError Manifest)
unreachableFull calls _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))

isUnreachable :: Either MetadataError Manifest -> Bool
isUnreachable = \case
    Left (MetadataFetch (FetchTransport _)) -> True
    _ -> False

failingVersion :: IORef Int -> PackageName -> Version -> IO (Either MetadataError VersionRead)
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

-- The same snapshot with a declared release tag, for the tag-carrying cases.
tagged :: PackageInfo -> Text -> PackageInfo
tagged info raw = info{infoDistTags = Map.singleton "latest" (npmVersion raw)}

details :: PackageName -> Text -> PackageDetails
details who rawVer =
    PackageDetails
        { pkgName = who
        , pkgVersion = npmVersion rawVer
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
