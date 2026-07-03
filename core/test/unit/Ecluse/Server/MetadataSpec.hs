module Ecluse.Server.MetadataSpec (spec) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
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
    mkPackageName,
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataUndecodable),
 )
import Ecluse.Core.Server.Cache (MetadataCache, Source (Source), cachedMetadata, defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached, Uncached), newMetadataClient)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Port (noopMetricsPort)

{- | Tests for the serve-path read handle's wiring: the full-manifest op resolving through
the shared cache, and the single-version op's __hybrid__ topology -- the small
@(package, version)@ cache, then the warm full-packument cache read-only (so a @GET@ then
its tarball gate stay one upstream call), then a cold selective fetch that populates the
version cache without writing the whole packument back to the shared one. Also that an
uncached handle re-fetches, and that a failure propagates and caches nothing.

The handle is exercised over __injected counting fetches__ (not real HTTP), one per leg,
so the cache-sharing and failure semantics are asserted directly. The two fetches share one
call counter, so an assertion on it is the total number of upstream calls across both legs.
-}
spec :: Spec
spec = do
    describe "newMetadataClient -- single-version hybrid topology" $ do
        it "reuses the warm full-packument cache: a GET then its version select is one upstream call" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0", "2.0.0"]
                client = publicClient cache (countingFull calls info) (countingVersion calls info)
            -- Populate the full cache (one upstream call) ...
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 1
            -- ... then the single-version op selects from that warm entry: no second call,
            -- and no selective version fetch.
            found <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) found `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1

        it "cold: leads a selective single-version fetch, caches it, and a repeat hits the version cache" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let info = manifest name ["1.0.0"]
                client = publicClient cache (countingFull calls info) (countingVersion calls info)
            -- No preceding GET: the version op leads its own selective fetch (one call) ...
            cold <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) cold `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1
            -- ... and a repeat is served from the version cache, no second call.
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
            -- A version the metadata does not carry is a forwarded miss (a 404), and it is
            -- cached as a determined absence ...
            absent <- fetchVersionMetadata client name (ver "2.0.0")
            fmap (fmap pkgVersion) absent `shouldBe` Right Nothing
            readIORef calls `shouldReturn` 1
            -- ... so a repeat is served from the negative cache entry, no second call.
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

    describe "newMetadataClient -- failure propagation" $
        it "propagates a MetadataError from both operations and caches nothing on failure" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let client = publicClient cache (failingFull calls) (failingVersion calls)
            full <- fetchFullManifest client name
            case full of
                Left err -> err `shouldBe` MetadataUndecodable
                Right _ -> expectationFailure "expected the failure to propagate"
            single <- fetchVersionMetadata client name (ver "1.0.0")
            case single of
                Left err -> err `shouldBe` MetadataUndecodable
                Right _ -> expectationFailure "expected the failure to propagate"
            -- A failed fetch caches nothing, so each op (the full leg, then the cold
            -- single-version leg) re-ran its fetch.
            readIORef calls `shouldReturn` 2

name :: PackageName
name = unscoped "is-odd"

unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

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

{- | A public (cached, anonymous) read handle over an injected full and single-version
fetch.
-}
publicClient ::
    MetadataCache ->
    (PackageName -> IO (Either MetadataError (PackageInfo, Value))) ->
    (PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))) ->
    MetadataClient
publicClient cache =
    newMetadataClient noopMetricsPort Metric.Public (Cached cache source) noLog noInvalidLog noFetchLog

{- | A counting full-manifest fetch: bumps the call counter, then yields the given manifest
paired with a marker raw 'Value' (so a test can confirm a hit returned the cached pair).
-}
countingFull :: IORef Int -> PackageInfo -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
countingFull calls info _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right (info, String "raw"))

{- | A counting single-version fetch: bumps the call counter, then selects the version from
the given manifest (so an absent version is a 'Nothing'), as the npm selective fetch would.
-}
countingVersion :: IORef Int -> PackageInfo -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
countingVersion calls info _name version = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right (Map.lookup (renderVersion version) (infoVersions info)))

-- | A counting full-manifest fetch that always fails, so a test can assert nothing is cached.
failingFull :: IORef Int -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
failingFull calls _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left MetadataUndecodable)

-- | A counting single-version fetch that always fails.
failingVersion :: IORef Int -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
failingVersion calls _name _version = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left MetadataUndecodable)

-- | A manifest self-reporting @name@ with the given versions, each an inert snapshot.
manifest :: PackageName -> [Text] -> PackageInfo
manifest who versions =
    PackageInfo
        { infoName = who
        , infoVersions = Map.fromList [(v, details who v) | v <- versions]
        , infoDistTags = Map.empty
        , infoInvalidEntries = []
        }

-- | A minimal per-version snapshot, identifiable by its parsed version.
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
            { artFilename = "pkg-" <> rawVer <> ".tgz"
            , artUrl = "https://example.test/pkg-" <> rawVer <> ".tgz"
            , artKind = Tarball
            , artHashes = []
            , artSize = Nothing
            , artInterpreter = Nothing
            , artYanked = False
            , artProvenance = Nothing
            }
