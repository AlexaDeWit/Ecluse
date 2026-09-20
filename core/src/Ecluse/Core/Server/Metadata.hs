-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RoleAnnotations #-}

{- | Caching, metrics, and failure logs around registry metadata reads.
The caching policy is not exported, and an origin carries its credential posture in its type.
'publicMetadataClient' takes reads over a 'Public' origin, which only
'Ecluse.Core.Registry.Origin.anonymousOrigin' builds and which presents no credential, so reads
that carry a caller's credential cannot reach the shared cache. 'privateMetadataClient' takes no
cache at all. Anonymous public reads share full-document and version caches.
-}
module Ecluse.Core.Server.Metadata (
    -- * Constructing a per-request read handle
    MetadataReads,
    newMetadataReads,
    publicMetadataClient,
    privateMetadataClient,

    -- * Projecting one version
    selectVersion,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageDetails, PackageInfo (infoDistTags, infoInvalidEntries, infoVersions), PackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestBodyBytes, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (..),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionDoc (VersionDoc, vdDetails, vdRaw),
    VersionRead (VersionRead, vrBodyBytes, vrUpstreamLatest, vrVersion),
 )
import Ecluse.Core.Registry.Origin (OriginClient, OriginFor, Private, Public, originClientOf)

import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryBodyBytes, entryDigest, entryInfo, entryRaw),
    MetadataCache,
    Source,
    cachedMetadata,
    cachedVersion,
    resolveMetadata,
    resolveVersion,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Version (Version, renderVersion)

-- Private reads re-authorise the caller at the upstream, so only anonymous public metadata
-- resolves through the shared cache, keyed by the origin's Source.
data ManifestCaching
    = Uncached
    | Cached MetadataCache Source

{- | One origin's raw reads bound to their observers, before a caching policy settles them into a
'MetadataClient'. The phantom is the posture of the origin the reads were bound to.
-}
newtype MetadataReads (posture :: Type) = MetadataReads (Metric.Upstream -> ManifestCaching -> MetadataClient)

-- As on OriginFor in Ecluse.Core.Registry.Origin: the default phantom role would let coerce
-- turn per-caller reads into the ones the public builder accepts.
type role MetadataReads nominal

{- | Bind one origin's raw reads to the metrics port and the failure, invalid-entry, and fetch logs.
The selector pairs a warm full-cache hit with one version's raw object, as a selective read does.
-}
newMetadataReads ::
    MetricsPort ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    (OriginClient -> PackageName -> IO (Either MetadataError Manifest)) ->
    (OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)) ->
    (Version -> CachedDoc -> Maybe CachedDoc) ->
    OriginFor posture ->
    MetadataReads posture
newMetadataReads metrics logFailure logInvalid logFetch rawFetch rawFetchVersion selectRaw origin =
    MetadataReads $ \upstream caching ->
        newMetadataClient
            ClientWiring
                { cwMetrics = metrics
                , cwUpstream = upstream
                , cwCaching = caching
                , cwFetch = rawFetch client
                , cwFetchVersion = rawFetchVersion client
                , cwSelectRaw = selectRaw
                , cwLogFailure = logFailure
                , cwLogInvalid = logInvalid
                , cwLogFetch = logFetch
                }
  where
    client = originClientOf origin

-- | The anonymous origin's handle, resolving through the shared cache under its 'Source' key.
publicMetadataClient :: MetadataCache -> Source -> MetadataReads Public -> MetadataClient
publicMetadataClient cache source (MetadataReads settle) = settle Metric.Public (Cached cache source)

-- | The per-caller origin's handle. It takes no cache, so the upstream re-authorises every caller.
privateMetadataClient :: MetadataReads Private -> MetadataClient
privateMetadataClient (MetadataReads settle) = settle Metric.Private Uncached

-- One origin's raw reads and observers, already settled by a caching policy and an upstream
-- label. Bundled so each read below takes it whole rather than nine positional parameters.
data ClientWiring = ClientWiring
    { cwMetrics :: MetricsPort
    , cwUpstream :: Metric.Upstream
    , cwCaching :: ManifestCaching
    , cwFetch :: PackageName -> IO (Either MetadataError Manifest)
    , cwFetchVersion :: PackageName -> Version -> IO (Either MetadataError VersionRead)
    , cwSelectRaw :: Version -> CachedDoc -> Maybe CachedDoc
    , cwLogFailure :: PackageName -> MetadataError -> IO ()
    , cwLogInvalid :: PackageName -> [InvalidEntry] -> IO ()
    , cwLogFetch :: PackageName -> IO ()
    }

newMetadataClient :: ClientWiring -> MetadataClient
newMetadataClient wiring =
    MetadataClient
        { fetchFullManifest = fmap (fmap entryToManifest) . resolveEntry wiring
        , fetchVersionMetadata = resolveVersionHybrid wiring
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
    pure (CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestBodyBytes manifest) (manifestDigest manifest))

resolveVersionHybrid :: ClientWiring -> PackageName -> Version -> IO (Either MetadataError VersionRead)
resolveVersionHybrid wiring name version = case cwCaching wiring of
    Uncached -> versionLeader wiring name version
    Cached cache source ->
        cachedVersion (cwMetrics wiring) cache source name version >>= \case
            Just versionRead -> Right versionRead <$ mpVersionCacheRequest (cwMetrics wiring) Metric.Hit
            Nothing ->
                cachedMetadata (cwMetrics wiring) cache source name >>= \case
                    Just entry -> Right (readOfEntry (cwSelectRaw wiring) version entry) <$ mpVersionCacheFullHit (cwMetrics wiring)
                    Nothing -> resolveVersion (cwMetrics wiring) cache source name version (versionLeader wiring name version)

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

-- | Find a version by its ecosystem-rendered key in a package snapshot.
selectVersion :: Version -> PackageInfo -> Maybe PackageDetails
selectVersion version info = Map.lookup (renderVersion version) (infoVersions info)

-- The typed view and raw version object must come from the same retained document.
readOfEntry :: (Version -> CachedDoc -> Maybe CachedDoc) -> Version -> CacheEntry -> VersionRead
readOfEntry selectRaw version entry =
    VersionRead
        { vrVersion = pairOf <$> selectVersion version (entryInfo entry)
        , vrBodyBytes = entryBodyBytes entry
        , vrUpstreamLatest = Map.lookup "latest" (infoDistTags (entryInfo entry))
        }
  where
    pairOf details = VersionDoc{vdDetails = details, vdRaw = selectRaw version (entryRaw entry)}

entryToManifest :: CacheEntry -> Manifest
entryToManifest entry =
    Manifest
        { manifestInfo = entryInfo entry
        , manifestRaw = entryRaw entry
        , manifestBodyBytes = entryBodyBytes entry
        , manifestDigest = entryDigest entry
        }

-- Only leaders record upstream work, so followers and retention hits do not inflate it.
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
