-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RoleAnnotations #-}

{- | Metadata observations and caching with credential posture enforced by the origin type.
Private reads bypass shared retention and coalescing. Preparation runs only within raw reads.
-}
module Ecluse.Core.Server.Metadata (
    -- * Constructing a per-request read handle
    MetadataReads,
    newMetadataReads,
    ecosystemMetadataReads,
    withinRequestCap,
    publicMetadataClient,
    uncachedPublicMetadataClient,
    preparePublicVersion,
    privateMetadataClient,

    -- * Recording one upstream read
    recordedFetch,

    -- * Projecting one version
    selectVersion,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (InvalidEntry, PackageDetails, PackageInfo (infoInvalidEntries, infoName, infoVersions), PackageName, pkgEcosystem)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchCooldown, FetchTransport, FetchUrlUnformable))
import Ecluse.Core.Registry.Exchange (withinServeCap)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestBodyBytes, manifestDigest, manifestInfo, manifestRaw, manifestRetainedEntries),
    MetadataClient (..),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionRead,
 )
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead, prepareManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation qualified as Preparation
import Ecluse.Core.Registry.Origin (OriginClient, OriginFor, Private, Public, originClientOf)
import Ecluse.Core.Security (ProgressFloor)

import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryBodyBytes, entryDigest, entryInfo, entryRaw, entryRetainedEntries),
    MetadataCache,
    Source,
    prepareVersion,
    resolveMetadata,
    resolveVersion,
 )
import Ecluse.Core.Server.Cache.Store (PreparedStore)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version, renderVersion, renderVersionKey, versionKeyIn)

-- Private reads re-authorise the caller at the upstream, so only anonymous public metadata
-- resolves through the shared cache, keyed by the origin's Source.
data ManifestCaching
    = Uncached
    | Cached MetadataCache Source

-- | Bind raw origin reads to observers before choosing caching. The phantom retains the origin's credential posture.
newtype MetadataReads (posture :: Type) = MetadataReads (Metric.Upstream -> ManifestCaching -> ClientWiring)

-- As on OriginFor in Ecluse.Core.Registry.Origin: the default phantom role would let coerce
-- turn per-caller reads into the ones the public builder accepts.
type role MetadataReads nominal

-- | Bind one origin's raw reads to the metrics port and the failure, invalid-entry, and fetch logs.
newMetadataReads ::
    MetricsPort ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    (OriginClient -> PackageName -> IO (Either MetadataError Manifest)) ->
    (OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)) ->
    OriginFor posture ->
    MetadataReads posture
newMetadataReads metrics logFailure logInvalid logFetch rawFetch rawFetchVersion origin =
    MetadataReads $ \upstream caching ->
        ClientWiring
            { cwMetrics = metrics
            , cwUpstream = upstream
            , cwCaching = caching
            , cwFetch = rawFetch client
            , cwFetchVersion = rawFetchVersion client
            , cwLogFailure = logFailure
            , cwLogInvalid = logInvalid
            , cwLogFetch = logFetch
            }
  where
    client = originClientOf origin

-- | Bind origin preparation to raw reads, inside cache leaders and request deadlines.
ecosystemMetadataReads ::
    PrepareRead ->
    TracingPort ->
    MetricsPort ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    OriginFor posture ->
    MetadataReads posture
ecosystemMetadataReads factory tracing metrics logFailure logInvalid logFetch =
    newMetadataReads metrics logFailure logInvalid logFetch (prepareManifest factory tracing) (Preparation.prepareVersion factory tracing)

-- | Cap each raw read so its single-flight leader fails before the request timeout.
withinRequestCap :: Deadlines -> ProgressFloor -> MetadataReads posture -> MetadataReads posture
withinRequestCap deadlines progress (MetadataReads settle) = MetadataReads $ \upstream caching ->
    let wiring = settle upstream caching
     in wiring
            { cwFetch = withinServeCap deadlines progress MetadataFetch . cwFetch wiring
            , cwFetchVersion = \name -> withinServeCap deadlines progress MetadataFetch . cwFetchVersion wiring name
            }

-- | The anonymous origin's handle, resolving through the shared cache under its 'Source' key.
publicMetadataClient :: MetadataCache -> Source -> MetadataReads Public -> MetadataClient
publicMetadataClient cache source (MetadataReads settle) = newMetadataClient (settle Metric.Public (Cached cache source))

-- | Keep worker upload reads out of the narrower serving-version cache.
uncachedPublicMetadataClient :: MetadataReads Public -> MetadataClient
uncachedPublicMetadataClient (MetadataReads settle) = newMetadataClient (settle Metric.Public Uncached)

-- | Prepare anonymous selected metadata while keeping its logs and upstream metrics on the leader.
preparePublicVersion :: MetadataCache -> Source -> MetadataReads Public -> PackageName -> Version -> IO (PreparedStore MetadataError VersionRead)
preparePublicVersion cache source (MetadataReads settle) name version =
    prepareVersion (cwMetrics wiring) cache source name version (versionLeader wiring name version)
  where
    wiring = settle Metric.Public (Cached cache source)

-- | The per-caller origin's handle. It takes no cache, so the upstream re-authorises every caller.
privateMetadataClient :: MetadataReads Private -> MetadataClient
privateMetadataClient (MetadataReads settle) = newMetadataClient (settle Metric.Private Uncached)

-- One origin's raw reads and observers, already settled by a caching policy and an upstream
-- label. Bundled so each read below takes it whole rather than nine positional parameters.
data ClientWiring = ClientWiring
    { cwMetrics :: MetricsPort
    , cwUpstream :: Metric.Upstream
    , cwCaching :: ManifestCaching
    , cwFetch :: PackageName -> IO (Either MetadataError Manifest)
    , cwFetchVersion :: PackageName -> Version -> IO (Either MetadataError VersionRead)
    , cwLogFailure :: PackageName -> MetadataError -> IO ()
    , cwLogInvalid :: PackageName -> [InvalidEntry] -> IO ()
    , cwLogFetch :: PackageName -> IO ()
    }

newMetadataClient :: ClientWiring -> MetadataClient
newMetadataClient wiring =
    MetadataClient
        { fetchFullManifest = fmap (fmap entryToManifest) . resolveEntry wiring
        , fetchVersionMetadata = resolveSelectedVersion wiring
        }

resolveEntry :: ClientWiring -> PackageName -> IO (Either MetadataError CacheEntry)
resolveEntry wiring name = case cwCaching wiring of
    Uncached -> manifestLeader wiring name
    Cached cache source -> resolveMetadata (cwMetrics wiring) cache source name (manifestLeader wiring name)

manifestLeader :: ClientWiring -> PackageName -> IO (Either MetadataError CacheEntry)
manifestLeader wiring name = do
    cwLogFetch wiring name
    recordedFetch (cwMetrics wiring) (cwUpstream wiring) $
        traverse (entryOfManifest wiring name) =<< loggingFailure wiring name (cwFetch wiring name)

entryOfManifest :: ClientWiring -> PackageName -> Manifest -> IO CacheEntry
entryOfManifest wiring name manifest = do
    let invalid = infoInvalidEntries (manifestInfo manifest)
    unless (null invalid) (cwLogInvalid wiring name invalid)
    pure (CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestRetainedEntries manifest) (manifestDigest manifest))

resolveSelectedVersion :: ClientWiring -> PackageName -> Version -> IO (Either MetadataError VersionRead)
resolveSelectedVersion wiring name version = case cwCaching wiring of
    Uncached -> versionLeader wiring name version
    Cached cache source -> resolveVersion (cwMetrics wiring) cache source name version (versionLeader wiring name version)

versionLeader :: ClientWiring -> PackageName -> Version -> IO (Either MetadataError VersionRead)
versionLeader wiring name version = do
    cwLogFetch wiring name
    recordedFetch (cwMetrics wiring) (cwUpstream wiring) $
        loggingFailure wiring name (cwFetchVersion wiring name version)

loggingFailure :: ClientWiring -> PackageName -> IO (Either MetadataError a) -> IO (Either MetadataError a)
loggingFailure wiring name action = do
    result <- action
    whenLeft_ result (cwLogFailure wiring name)
    pure result

-- | Prefer an exact version key, then associate PyPI spellings through the canonical key.
selectVersion :: Version -> PackageInfo -> Maybe PackageDetails
selectVersion version info =
    Map.lookup (renderVersion version) versions <|> do
        guard (pkgEcosystem (infoName info) == PyPI)
        key <- versionKeyIn PyPI version
        Map.lookup (renderVersionKey key) versions
  where
    versions = infoVersions info

entryToManifest :: CacheEntry -> Manifest
entryToManifest entry =
    Manifest
        { manifestInfo = entryInfo entry
        , manifestRaw = entryRaw entry
        , manifestBodyBytes = entryBodyBytes entry
        , manifestRetainedEntries = entryRetainedEntries entry
        , manifestDigest = entryDigest entry
        }

-- | Record one upstream metadata read's latency or error cause. Only leaders call it, so followers and hits do not inflate it.
recordedFetch :: MetricsPort -> Metric.Upstream -> IO (Either MetadataError a) -> IO (Either MetadataError a)
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds action
    case result of
        Right _ -> mpUpstreamFetch metrics upstream Metric.Status2xx seconds
        Left err -> mpUpstreamFetchError metrics upstream (metadataErrorCause err)
    pure result

metadataErrorCause :: MetadataError -> Metric.Cause
metadataErrorCause = \case
    MetadataAbsent -> Metric.UpstreamStatus
    MetadataHttpFailure _ -> Metric.UpstreamStatus
    MetadataAuthorisationFailure _ -> Metric.OtherCause
    MetadataUndecodable -> Metric.Decode
    MetadataNameMismatch _ -> Metric.Decode
    MetadataBoundExceeded _ -> Metric.OtherCause
    MetadataFetch (FetchUrlUnformable _) -> Metric.OtherCause
    MetadataFetch (FetchBoundExceeded _) -> Metric.OtherCause
    MetadataFetch (FetchTransport _) -> Metric.Connection
    MetadataFetch (FetchCooldown _) -> Metric.UpstreamStatus
