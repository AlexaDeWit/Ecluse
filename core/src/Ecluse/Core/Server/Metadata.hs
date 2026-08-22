-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Wiring a per-request "Ecluse.Core.Registry.Metadata.MetadataClient" for the serve
path: the cross-cutting caching, metrics, and failure-logging policy wrapped around a
registry's raw fetch primitive.

The read boundary's /type/ lives in the registry layer (agnostic). A registry's raw
fetch primitive lives with that registry (npm's in
"Ecluse.Core.Registry.Npm.Metadata"). What lives __here__ is the serve-path policy
that is the same regardless of ecosystem. That policy covers whether an origin resolves
through the shared metadata cache, recording the upstream-fetch metrics, and logging a
failure once in the request's context. Keeping that policy in the serve layer is what lets the
registry layer stay free of the cache and telemetry.

The two operations differ in how they resolve. The full-manifest op resolves the whole
packument through the shared full-packument cache. The single-version op takes a
__hybrid__ path, so a cold tarball gate need not pay a whole-packument decode to consult
one version (see 'newMetadataClient'). It consults a small @(package, version)@ cache,
then the warm full-packument cache __read-only__. A packument @GET@ followed by its
tarball gate therefore still collapses to one upstream call. Only on a cold miss does it
lead its own __selective__ fetch into the @(package, version)@ cache. That fetch parses
just the requested version out of the full bytes, and never writes the whole packument
back to the shared cache.
-}
module Ecluse.Core.Server.Metadata (
    -- * Caching policy
    ManifestCaching (..),

    -- * Constructing a per-request read handle
    newMetadataClient,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageDetails, PackageInfo (infoInvalidEntries, infoVersions), PackageName)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient (..),
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable, MetadataUnreachable, MetadataUrlUnformable),
 )

import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryDigest, entryInfo, entryRaw),
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

{- | How a read handle resolves the full manifest for one origin.

The two origins of a packument merge differ exactly here. The private origin is the
per-client authority and must not be shared. The public origin is anonymous and shared
across every client.
-}
data ManifestCaching
    = {- | Resolve directly, uncached: the per-client private origin. It is re-fetched
      every request, so the upstream re-authorises each client's own forwarded credential.
      -}
      Uncached
    | {- | Resolve through the shared metadata cache under the origin's 'Source' key:
      the anonymous public origin. Concurrent and subsequent reads therefore collapse to
      one upstream call, and both operations of the resulting handle share this one entry.
      -}
      Cached MetadataCache Source

{- | Build a per-request read handle from a registry's raw fetch primitives. One fetches
and projects the __full manifest__, and one fetches and __selectively__ projects a
__single version__. The handle wires them with the caching policy, the upstream-fetch
metrics, and a request-context failure log.

The full-manifest op resolves the whole packument through the shared full-packument cache.
The single-version op takes the __hybrid__ path that delivers the cheap cold tarball gate
while preserving the warm install one-call property:

  1. consult the small @(package, version)@ cache. A hit returns at once, whether a
     positive snapshot or a cached /determined absence/.
  2. else consult the warm full-packument cache __read-only__. A hit selects the one
     version from the shared entry, so a packument @GET@ followed by its tarball gate is
     still one upstream call. It __does not__ populate the version cache.
  3. else (cold) lead the raw __single-version__ fetch through the @(package, version)@
     cache's single-flight. That fetch reads the full bytes but parses only the requested
     version. The resulting snapshot (or its determined absence) is cached there, and the
     whole packument is __never__ written back to the shared cache.

For the 'Uncached' policy (the per-client private origin) there is no shared cache to
consult. The single-version op is then the raw selective fetch, uncached, re-run each
request.

The failure log is invoked __once per real fetch__, inside the cache's single-flight
leader and in the caller's logging context. A coalesced follower therefore never re-logs
a failure the leader already reported. The dropped-entry log ('logInvalid') is invoked
the same way: once per real full-manifest fetch, and only when the projection dropped a
malformed entry. An operator therefore sees a degraded-but-served document without it
re-logging on every cache hit.
-}
newMetadataClient ::
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    (PackageName -> IO (Either MetadataError Manifest)) ->
    (PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))) ->
    MetadataClient
newMetadataClient metrics upstream caching logFailure logInvalid logFetch rawFetch rawFetchVersion =
    MetadataClient
        { fetchFullManifest = fmap (fmap entryToManifest) . resolveEntry
        , fetchVersionMetadata = resolveVersionHybrid
        }
  where
    resolveEntry :: PackageName -> IO (Either MetadataError CacheEntry)
    resolveEntry name = case caching of
        Uncached -> manifestLeader name
        Cached cache source -> resolveMetadata metrics cache source name (manifestLeader name)

    -- The full-manifest single-flight leader action: the real fetch, run only on a cache
    -- miss, metered. Any dropped malformed entries are logged on success. A fetch
    -- failure is logged once before its 'Left' reaches the cache, which stores nothing
    -- and delivers the same value to every coalesced follower.
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
    resolveVersionHybrid :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    resolveVersionHybrid name version = case caching of
        Uncached -> versionLeader name version
        Cached cache source -> do
            -- (1) The single-version cache: a positive snapshot or a cached determined
            -- absence both short-circuit.
            cached <- cachedVersion cache source name version
            case cached of
                Just details -> pure (Right details)
                Nothing -> do
                    -- (2) The warm full-packument cache, read-only: select the version
                    -- from the shared entry the packument @GET@ populated. Nothing is
                    -- written back to the version cache, the install one-call property.
                    warm <- cachedMetadata cache source name
                    case warm of
                        Just entry -> pure (Right (selectVersion version (entryInfo entry)))
                        -- (3) Cold: lead the selective fetch through the version cache.
                        Nothing -> resolveVersion metrics cache source name version (versionLeader name version)

    -- The single-version single-flight leader action: the real selective fetch, run only
    -- on a cold miss, metered. On failure it is logged once before its 'Left' reaches
    -- the cache, exactly as the full-manifest leader.
    versionLeader :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    versionLeader name version = do
        logFetch name
        recordedFetch metrics upstream $
            rawFetchVersion name version >>= \case
                Right details -> pure (Right details)
                Left err -> logFailure name err >> pure (Left err)

-- Select one version's details out of a parsed packument, by its rendered form.
selectVersion :: Version -> PackageInfo -> Maybe PackageDetails
selectVersion version info = Map.lookup (renderVersion version) (infoVersions info)

-- Widen a cached entry back to the read handle's 'Manifest'. The same three fields,
-- named for the boundary each type serves: the cache stores, the handle answers.
entryToManifest :: CacheEntry -> Manifest
entryToManifest entry =
    Manifest
        { manifestInfo = entryInfo entry
        , manifestRaw = entryRaw entry
        , manifestDigest = entryDigest entry
        }

{- Record one upstream metadata fetch around the leader action: its latency on a
successful resolve, or the bounded error cause otherwise. The leader runs only on a
cache miss, so the public path records real upstream calls, not cache hits. This is
value-agnostic in the payload, so it wraps either leg's leader: a full-manifest
'CacheEntry' or a single-version snapshot. The outcome passes through untouched, so the
caller's degrade is unchanged. -}
recordedFetch :: MetricsPort -> Metric.Upstream -> IO (Either MetadataError a) -> IO (Either MetadataError a)
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds action
    case result of
        Right _ -> mpUpstreamFetch metrics upstream Metric.Status2xx seconds
        Left err -> mpUpstreamFetchError metrics upstream (metadataErrorCause err)
    pure result

{- Classify a leader-fetch failure into the bounded @ecluse.upstream.fetch.errors@
cause. A decode or name failure is a decode fault, and an unreachable upstream is a
connection fault. A bound breach or a config fault is the catch-all other. The cause is
read off the typed 'MetadataError' rather than any stringly error text, so it stays
bounded by construction. -}
metadataErrorCause :: MetadataError -> Metric.Cause
metadataErrorCause = \case
    MetadataUndecodable -> Metric.Decode
    MetadataNameMismatch _ -> Metric.Decode
    MetadataBoundExceeded _ -> Metric.OtherCause
    MetadataUrlUnformable _ -> Metric.OtherCause
    MetadataUnreachable _ -> Metric.Connection
