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
    readOfEntry,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageDetails, PackageInfo (infoDistTags, infoInvalidEntries, infoVersions), PackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (..),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionDoc (VersionDoc, vdDetails, vdRaw),
    VersionRead (VersionRead, vrUpstreamLatest, vrVersion),
 )
import Ecluse.Core.Registry.Origin (OriginClient, OriginFor, Private, Public, originClientOf)

import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryDigest, entryInfo, entryRaw),
    MetadataCache,
    Source,
    cachedMetadata,
    cachedVersion,
    resolveMetadata,
    resolveVersion,
 )
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
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
        newMetadataClient metrics upstream caching logFailure logInvalid logFetch (rawFetch client) (rawFetchVersion client) selectRaw
  where
    client = originClientOf origin

-- | The anonymous origin's handle, resolving through the shared cache under its 'Source' key.
publicMetadataClient :: MetadataCache -> Source -> MetadataReads Public -> MetadataClient
publicMetadataClient cache source (MetadataReads settle) = settle Metric.Public (Cached cache source)

-- | The per-caller origin's handle. It takes no cache, so the upstream re-authorises every caller.
privateMetadataClient :: MetadataReads Private -> MetadataClient
privateMetadataClient (MetadataReads settle) = settle Metric.Private Uncached

newMetadataClient ::
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    (PackageName -> IO (Either MetadataError Manifest)) ->
    (PackageName -> Version -> IO (Either MetadataError VersionRead)) ->
    (Version -> CachedDoc -> Maybe CachedDoc) ->
    MetadataClient
newMetadataClient metrics upstream caching logFailure logInvalid logFetch rawFetch rawFetchVersion selectRaw =
    MetadataClient
        { fetchFullManifest = fmap (fmap entryToManifest) . resolveEntry
        , fetchVersionMetadata = resolveVersionHybrid
        }
  where
    resolveEntry :: PackageName -> IO (Either MetadataError CacheEntry)
    resolveEntry name = case caching of
        Uncached -> manifestLeader name
        Cached cache source -> resolveMetadata metrics cache source name (manifestLeader name)

    manifestLeader :: PackageName -> IO (Either MetadataError CacheEntry)
    manifestLeader name = do
        logFetch name
        recordedFetch metrics upstream $
            rawFetch name >>= \case
                Right manifest -> do
                    let invalid = infoInvalidEntries (manifestInfo manifest)
                    unless (null invalid) (logInvalid name invalid)
                    pure (Right (CacheEntry (manifestInfo manifest) (manifestRaw manifest) (manifestDigest manifest)))
                Left err -> logFailure name err >> pure (Left err)

    -- The single-version hybrid: the small version cache, then the warm full cache
    -- read-only, then a cold selective fetch. Uncached, it is the raw selective fetch.
    resolveVersionHybrid :: PackageName -> Version -> IO (Either MetadataError VersionRead)
    resolveVersionHybrid name version = case caching of
        Uncached -> versionLeader name version
        Cached cache source -> do
            cached <- cachedVersion cache source name version
            case cached of
                Just versionRead -> pure (Right versionRead)
                Nothing -> do
                    warm <- cachedMetadata cache source name
                    case warm of
                        Just entry -> pure (Right (readOfEntry selectRaw version entry))
                        Nothing -> resolveVersion metrics cache source name version (versionLeader name version)

    versionLeader :: PackageName -> Version -> IO (Either MetadataError VersionRead)
    versionLeader name version = do
        logFetch name
        recordedFetch metrics upstream $
            rawFetchVersion name version >>= \case
                Right details -> pure (Right details)
                Left err -> logFailure name err >> pure (Left err)

-- | Find a version by its ecosystem-rendered key in a package snapshot.
selectVersion :: Version -> PackageInfo -> Maybe PackageDetails
selectVersion version info = Map.lookup (renderVersion version) (infoVersions info)

{- | Project a held entry onto one version's read, so a warm full-cache hit answers as a selective
read would: the pair's two sides and its digest all come from the one entry.
-}
readOfEntry :: (Version -> CachedDoc -> Maybe CachedDoc) -> Version -> CacheEntry -> VersionRead
readOfEntry selectRaw version entry =
    VersionRead
        { vrVersion = pairOf <$> selectVersion version (entryInfo entry)
        , vrUpstreamLatest = Map.lookup "latest" (infoDistTags (entryInfo entry))
        }
  where
    pairOf details = Snapshot (entryDigest entry) (VersionDoc{vdDetails = details, vdRaw = selectRaw version (entryRaw entry)})

entryToManifest :: CacheEntry -> Manifest
entryToManifest entry =
    Manifest
        { manifestInfo = entryInfo entry
        , manifestRaw = entryRaw entry
        , manifestDigest = entryDigest entry
        }

{- Record one upstream metadata fetch around a leader action: its latency on success, or the
bounded error cause otherwise. The leader runs only on a miss, so this never meters a cache hit. -}
recordedFetch :: MetricsPort -> Metric.Upstream -> IO (Either MetadataError a) -> IO (Either MetadataError a)
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds action
    case result of
        Right _ -> mpUpstreamFetch metrics upstream Metric.Status2xx seconds
        Left err -> mpUpstreamFetchError metrics upstream (metadataErrorCause err)
    pure result

{- Classify a leader-fetch failure into the bounded @ecluse.upstream.fetch.errors@ cause. It reads
the typed 'MetadataError', never error text, so the label set stays bounded by construction. -}
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
