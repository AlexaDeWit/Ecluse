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
import Ecluse.Core.Server.Cache (MetadataCache, Source (Source), defaultCacheConfig, newMetadataCache)
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached, Uncached), newMetadataClient)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Port (noopMetricsPort)

{- | Tests for the serve-path read handle's wiring: that both intent operations resolve
through one shared cache entry (so the tarball gate's single-version read does not
re-fetch a packument the @GET@ path already resolved), that the single-version op
selects the right version (or reports a genuine absence as 'Nothing'), that an uncached
handle re-fetches every call, and that a failure propagates and caches nothing.

The handle is exercised over an __injected counting fetch__ (not real HTTP), so the
cache-sharing and failure semantics are asserted directly, without a registry.
-}
spec :: Spec
spec = do
    describe "newMetadataClient — shared cache across the two operations" $ do
        it "serves the single-version op from the full manifest's cache entry (one upstream call)" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let name = unscoped "is-odd"
                client = publicClient cache (countingFetch calls (manifest name ["1.0.0", "2.0.0"]))
            full <- fetchFullManifest client name
            case full of
                Right (info, raw) -> do
                    Map.keys (infoVersions info) `shouldBe` ["1.0.0", "2.0.0"]
                    raw `shouldBe` String "raw"
                Left err -> expectationFailure ("expected the full manifest, got: " <> show err)
            readIORef calls `shouldReturn` 1
            -- The single-version read shares the cache the full op populated: no second
            -- upstream call, and it picks the requested version.
            found <- fetchVersionMetadata client name (ver "1.0.0")
            fmap (fmap pkgVersion) found `shouldBe` Right (Just (ver "1.0.0"))
            readIORef calls `shouldReturn` 1

        it "reports a version absent from the resolved manifest as Nothing (a forwarded miss, not an error)" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let name = unscoped "is-odd"
                client = publicClient cache (countingFetch calls (manifest name ["1.0.0"]))
            absent <- fetchVersionMetadata client name (ver "2.0.0")
            fmap (fmap pkgVersion) absent `shouldBe` Right Nothing

    describe "newMetadataClient — caching policy" $
        it "an uncached handle fetches on every call (the per-client private origin)" $ do
            calls <- newIORef (0 :: Int)
            let name = unscoped "is-odd"
                client =
                    newMetadataClient noopMetricsPort Metric.Private Uncached noLog (countingFetch calls (manifest name ["1.0.0"]))
            _ <- fetchFullManifest client name
            _ <- fetchFullManifest client name
            readIORef calls `shouldReturn` 2

    describe "newMetadataClient — failure propagation" $
        it "propagates a MetadataError from both operations and caches nothing on failure" $ do
            calls <- newIORef (0 :: Int)
            cache <- newMetadataCache defaultCacheConfig
            let name = unscoped "is-odd"
                client = publicClient cache (failingFetch calls)
            full <- fetchFullManifest client name
            case full of
                Left err -> err `shouldBe` MetadataUndecodable
                Right _ -> expectationFailure "expected the failure to propagate"
            single <- fetchVersionMetadata client name (ver "1.0.0")
            case single of
                Left err -> err `shouldBe` MetadataUndecodable
                Right _ -> expectationFailure "expected the failure to propagate"
            -- A failed fetch caches nothing, so each of the two calls re-ran the fetch.
            readIORef calls `shouldReturn` 2

-- ── fixtures ──────────────────────────────────────────────────────────────────

unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

ver :: Text -> Version
ver = mkVersion Npm

noLog :: PackageName -> MetadataError -> IO ()
noLog _ _ = pure ()

-- | A public (cached, anonymous) read handle over an injected raw fetch.
publicClient :: MetadataCache -> (PackageName -> IO (Either MetadataError (PackageInfo, Value))) -> MetadataClient
publicClient cache =
    newMetadataClient noopMetricsPort Metric.Public (Cached cache (Source "https://public.example")) noLog

{- | A counting raw fetch: bumps the call counter, then yields the given manifest paired
with a marker raw 'Value' (so a test can confirm a hit returned the cached pair).
-}
countingFetch :: IORef Int -> PackageInfo -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
countingFetch calls info _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Right (info, String "raw"))

-- | A counting raw fetch that always fails, so a test can assert nothing is cached.
failingFetch :: IORef Int -> PackageName -> IO (Either MetadataError (PackageInfo, Value))
failingFetch calls _name = do
    atomicModifyIORef' calls (\n -> (n + 1, ()))
    pure (Left MetadataUndecodable)

-- | A manifest self-reporting @name@ with the given versions, each an inert snapshot.
manifest :: PackageName -> [Text] -> PackageInfo
manifest name versions =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, details name v) | v <- versions]
        , infoDistTags = Map.empty
        , infoPublishedAt = Map.empty
        }

-- | A minimal per-version snapshot, identifiable by its parsed version.
details :: PackageName -> Text -> PackageDetails
details name rawVer =
    PackageDetails
        { pkgName = name
        , pkgVersion = ver rawVer
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = TrustUnknown
        , pkgAvailability = Available
        , pkgArtifacts = artifact :| []
        , pkgLicenses = []
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
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
