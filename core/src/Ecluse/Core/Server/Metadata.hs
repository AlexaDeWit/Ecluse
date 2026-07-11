-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Wiring a per-request "Ecluse.Core.Registry.Metadata.MetadataClient" for the serve
path: the cross-cutting caching, metrics, and failure-logging policy wrapped around a
registry's raw fetch primitive.

The read boundary's /type/ lives in the registry layer (agnostic); a registry's raw
fetch primitive lives with that registry (npm's in
"Ecluse.Core.Registry.Npm.Metadata"). What lives __here__ is the serve-path policy
that is the same regardless of ecosystem: whether an origin is resolved through the
shared metadata cache, recording the upstream-fetch metrics, and logging a failure
once in the request's context. Keeping that policy in the serve layer is what lets the
registry layer stay free of the cache and telemetry.

The two operations differ in how they resolve. The full-manifest op resolves the whole
packument through the shared full-packument cache. The single-version op takes a
__hybrid__ path so a cold tarball gate need not pay a whole-packument decode to consult
one version (see 'newMetadataClient'): it consults a small @(package, version)@ cache, then
the warm full-packument cache __read-only__ (so a packument @GET@ followed by its tarball
gate still collapses to one upstream call), and only on a cold miss leads its own
__selective__ fetch -- parsing just the requested version out of the full bytes -- into the
@(package, version)@ cache, never writing the whole packument back to the shared cache.
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

The two origins of a packument merge differ exactly here: the private origin is the
per-client authority and must not be shared, while the public origin is anonymous and
shared across every client.
-}
data ManifestCaching
    = {- | Resolve directly, uncached -- the per-client private origin, re-fetched every
      request so the upstream re-authorises each client's own forwarded credential.
      -}
      Uncached
    | {- | Resolve through the shared metadata cache under the origin's 'Source' key --
      the anonymous public origin, so concurrent and subsequent reads collapse to one
      upstream call. Both operations of the resulting handle share this one entry.
      -}
      Cached MetadataCache Source

{- | Build a per-request read handle from a registry's raw fetch primitives -- one that
fetches and projects the __full manifest__, one that fetches and __selectively__ projects a
__single version__ -- wiring them with the caching policy, the upstream-fetch metrics, and a
request-context failure log.

The full-manifest op resolves the whole packument through the shared full-packument cache.
The single-version op takes the __hybrid__ path that delivers the cheap cold tarball gate
while preserving the warm install one-call property:

  1. consult the small @(package, version)@ cache -- a hit (a positive snapshot, or a cached
     /determined absence/) returns at once;
  2. else consult the warm full-packument cache __read-only__ -- a hit selects the one version
     from the shared entry (so a packument @GET@ followed by its tarball gate is still one
     upstream call), and __does not__ populate the version cache;
  3. else (cold) lead the raw __single-version__ fetch -- which fetches the full bytes but
     parses only the requested version -- through the @(package, version)@ cache's
     single-flight, caching the resulting snapshot (or its determined absence) there, and
     __never__ writing the whole packument back to the shared cache.

For the 'Uncached' policy (the per-client private origin) there is no shared cache to
consult, so the single-version op is the raw selective fetch, uncached, re-run each request.

The failure log is invoked __once per real fetch__ (inside the cache's single-flight
leader), in the caller's logging context, so a coalesced follower never re-logs a
failure the leader already reported. The dropped-entry log ('logInvalid') is invoked the
same way (once per real full-manifest fetch, only when the projection dropped a
malformed entry), so an operator sees a degraded-but-served document without it
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
    -- miss, metered, with any dropped malformed entries logged on success and a fetch
    -- failure logged once before its 'Left' is handed to the cache (which stores nothing
    -- and delivers the same value to every coalesced follower).
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
    -- read-only, then a cold selective fetch -- or, uncached, the raw selective fetch.
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
                    -- (2) The warm full-packument cache, read-only: select the version from
                    -- the shared entry the packument @GET@ populated, never writing back to
                    -- the version cache (the install one-call property).
                    warm <- cachedMetadata cache source name
                    case warm of
                        Just entry -> pure (Right (selectVersion version (entryInfo entry)))
                        -- (3) Cold: lead the selective fetch through the version cache.
                        Nothing -> resolveVersion metrics cache source name version (versionLeader name version)

    -- The single-version single-flight leader action: the real selective fetch, run only on
    -- a cold miss, metered and (on failure) logged once before its 'Left' is handed to the
    -- cache, exactly as the full-manifest leader.
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

-- Widen a cached entry back to the read handle's 'Manifest': the same three fields,
-- named for the boundary each type serves (the cache stores, the handle answers).
entryToManifest :: CacheEntry -> Manifest
entryToManifest entry =
    Manifest
        { manifestInfo = entryInfo entry
        , manifestRaw = entryRaw entry
        , manifestDigest = entryDigest entry
        }

{- Record one upstream metadata fetch around the leader action: its latency on a
successful resolve, or the bounded error cause otherwise. Wrapping the leader -- which
runs only on a cache miss -- means the public path records real upstream calls, not
cache hits. Value-agnostic in the payload, so it wraps either leg's leader (a
full-manifest 'CacheEntry' or a single-version snapshot); the outcome passes through
untouched, so the caller's degrade is unchanged. -}
recordedFetch :: MetricsPort -> Metric.Upstream -> IO (Either MetadataError a) -> IO (Either MetadataError a)
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds action
    case result of
        Right _ -> mpUpstreamFetch metrics upstream Metric.Status2xx seconds
        Left err -> mpUpstreamFetchError metrics upstream (metadataErrorCause err)
    pure result

{- Classify a leader-fetch failure into the bounded @ecluse.upstream.fetch.errors@
cause: a decode or name failure is a decode fault, an unreachable upstream a
connection fault, a bound breach or a config fault the catch-all other. Read off the
typed 'MetadataError' rather than any stringly error text, so the cause stays bounded
by construction. -}
metadataErrorCause :: MetadataError -> Metric.Cause
metadataErrorCause = \case
    MetadataUndecodable -> Metric.Decode
    MetadataNameMismatch _ -> Metric.Decode
    MetadataBoundExceeded _ -> Metric.OtherCause
    MetadataUrlUnformable _ -> Metric.OtherCause
    MetadataUnreachable _ -> Metric.Connection
