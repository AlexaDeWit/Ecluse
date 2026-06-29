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
__selective__ fetch — parsing just the requested version out of the full bytes — into the
@(package, version)@ cache, never writing the whole packument back to the shared cache.
-}
module Ecluse.Core.Server.Metadata (
    -- * Caching policy
    ManifestCaching (..),

    -- * Constructing a per-request read handle
    newMetadataClient,
    newNpmMetadataClient,
) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import UnliftIO.Exception (throwIO, try, tryAny)

import Ecluse.Core.Package (InvalidEntry, PackageDetails, PackageInfo (infoInvalidEntries, infoVersions), PackageName)
import Ecluse.Core.Registry.Metadata (
    MetadataClient (..),
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm (NpmClientConfig)
import Ecluse.Core.Registry.Npm.Metadata (fetchNpmManifest, fetchNpmVersion)

import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryInfo, entryRaw),
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
    = {- | Resolve directly, uncached — the per-client private origin, re-fetched every
      request so the upstream re-authorises each client's own forwarded credential.
      -}
      Uncached
    | {- | Resolve through the shared metadata cache under the origin's 'Source' key —
      the anonymous public origin, so concurrent and subsequent reads collapse to one
      upstream call. Both operations of the resulting handle share this one entry.
      -}
      Cached MetadataCache Source

{- | Build a per-request read handle from a registry's raw fetch primitives — one that
fetches and projects the __full manifest__, one that fetches and __selectively__ projects a
__single version__ — wiring them with the caching policy, the upstream-fetch metrics, and a
request-context failure log.

The full-manifest op resolves the whole packument through the shared full-packument cache.
The single-version op takes the __hybrid__ path that delivers the cheap cold tarball gate
while preserving the warm install one-call property:

  1. consult the small @(package, version)@ cache — a hit (a positive snapshot, or a cached
     /determined absence/) returns at once;
  2. else consult the warm full-packument cache __read-only__ — a hit selects the one version
     from the shared entry (so a packument @GET@ followed by its tarball gate is still one
     upstream call), and __does not__ populate the version cache;
  3. else (cold) lead the raw __single-version__ fetch — which fetches the full bytes but
     parses only the requested version — through the @(package, version)@ cache's
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
    (PackageName -> IO (Either MetadataError (PackageInfo, Value))) ->
    (PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))) ->
    MetadataClient
newMetadataClient metrics upstream caching logFailure logInvalid rawFetch rawFetchVersion =
    MetadataClient
        { fetchFullManifest = resolveFull
        , fetchVersionMetadata = resolveVersionHybrid
        }
  where
    resolveFull :: PackageName -> IO (Either MetadataError (PackageInfo, Value))
    resolveFull name = do
        -- A leader's parse\/policy failure is raised as the carrier so the cache stores
        -- nothing and re-raises to followers; here it is folded back to a 'Left'. A
        -- transport fault is a different type, so it is not caught and propagates to the
        -- serve path's bracket, exactly as before.
        outcome <- try (resolveEntry name)
        pure $ case outcome of
            Right entry -> Right (entryInfo entry, entryRaw entry)
            Left (ManifestFetchFailed err) -> Left err

    resolveEntry :: PackageName -> IO CacheEntry
    resolveEntry name = case caching of
        Uncached -> manifestLeader name
        Cached cache source -> resolveMetadata metrics cache source name (manifestLeader name)

    -- The full-manifest single-flight leader action: the real fetch, run only on a cache
    -- miss, metered, with any dropped malformed entries logged on success and a fetch
    -- failure logged once before the carrier is raised.
    manifestLeader :: PackageName -> IO CacheEntry
    manifestLeader name =
        recordedFetch metrics upstream $
            rawFetch name >>= \case
                Right (info, raw) -> do
                    let invalid = infoInvalidEntries info
                    unless (null invalid) (logInvalid name invalid)
                    pure (CacheEntry info raw)
                Left err -> logFailure name err >> throwIO (ManifestFetchFailed err)

    -- The single-version hybrid: the small version cache, then the warm full cache
    -- read-only, then a cold selective fetch — or, uncached, the raw selective fetch.
    resolveVersionHybrid :: PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
    resolveVersionHybrid name version = case caching of
        Uncached -> runVersion (versionLeader name version)
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
                        Nothing -> runVersion (resolveVersion metrics cache source name version (versionLeader name version))

    selectVersion :: Version -> PackageInfo -> Maybe PackageDetails
    selectVersion version info = Map.lookup (renderVersion version) (infoVersions info)

    -- Fold a single-version leader run's carrier back to a 'Left', mirroring 'resolveFull':
    -- a leader's parse\/policy failure is raised through the cache (which stores nothing and
    -- re-raises to followers) and recovered here; a transport fault is a different type and
    -- propagates to the serve path's bracket.
    runVersion :: IO (Maybe PackageDetails) -> IO (Either MetadataError (Maybe PackageDetails))
    runVersion action = do
        outcome <- try action
        pure $ case outcome of
            Right details -> Right details
            Left (VersionFetchFailed err) -> Left err

    -- The single-version single-flight leader action: the real selective fetch, run only on
    -- a cold miss, metered and (on failure) logged once before the carrier is raised.
    versionLeader :: PackageName -> Version -> IO (Maybe PackageDetails)
    versionLeader name version =
        recordedFetch metrics upstream $
            rawFetchVersion name version >>= \case
                Right details -> pure details
                Left err -> logFailure name err >> throwIO (VersionFetchFailed err)

{- | Build a per-request read handle for the npm protocol over one origin's fetch
configuration: the npm full-manifest and single-version fetches as the raw primitives, with
the serve-path caching, metrics, and the failure and dropped-entry logs wired by
'newMetadataClient'.
-}
newNpmMetadataClient ::
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    NpmClientConfig ->
    MetadataClient
newNpmMetadataClient metrics upstream caching logFailure logInvalid config =
    newMetadataClient metrics upstream caching logFailure logInvalid (fetchNpmManifest config) (fetchNpmVersion config)

{- The in-band failure carrier for a full-manifest leader fetch: a 'MetadataError' raised
so the shared metadata cache caches nothing on failure and re-raises it to coalesced
followers, then converted back to a 'Left' at the resolve boundary. Internal — the
serve path only ever sees the returned 'Either' (or a genuine transport throw). -}
newtype ManifestFetchFailed = ManifestFetchFailed MetadataError
    deriving stock (Show)

instance Exception ManifestFetchFailed

{- The single-version analogue of 'ManifestFetchFailed': the carrier a single-version
leader raises so the version cache stores nothing on failure and re-raises to coalesced
followers, recovered to a 'Left' by 'newMetadataClient'. Distinct from
'ManifestFetchFailed' only so each leg's carrier is unambiguous; both wrap a
'MetadataError'. -}
newtype VersionFetchFailed = VersionFetchFailed MetadataError
    deriving stock (Show)

instance Exception VersionFetchFailed

{- Record one upstream metadata fetch around the leader action: its latency on a
successful resolve, or the bounded error cause otherwise, before re-raising so the
caller's degrade is unchanged. Wrapping the leader — which runs only on a cache miss —
means the public path records real upstream calls, not cache hits. Value-agnostic, so it
wraps either leg's leader (a full-manifest 'CacheEntry' or a single-version snapshot). -}
recordedFetch :: MetricsPort -> Metric.Upstream -> IO a -> IO a
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds (tryAny action)
    case result of
        Right entry -> do
            mpUpstreamFetch metrics upstream Metric.Status2xx seconds
            pure entry
        Left err -> do
            mpUpstreamFetchError metrics upstream (fetchCause err)
            throwIO err

{- Classify a leader-fetch failure into the bounded @ecluse.upstream.fetch.errors@
cause: a decode or name failure is a decode fault, a transport error a connection
fault, a bound breach or anything else the catch-all other. Read off the typed
'MetadataError' the carrier holds rather than any stringly error text, so the cause
stays bounded by construction. -}
fetchCause :: SomeException -> Metric.Cause
fetchCause err
    | Just (ManifestFetchFailed me) <- fromException err = metadataErrorCause me
    | Just (VersionFetchFailed me) <- fromException err = metadataErrorCause me
    | Just (_ :: HTTP.HttpException) <- fromException err = Metric.Connection
    | otherwise = Metric.OtherCause

metadataErrorCause :: MetadataError -> Metric.Cause
metadataErrorCause = \case
    MetadataUndecodable -> Metric.Decode
    MetadataNameMismatch _ -> Metric.Decode
    MetadataBoundExceeded _ -> Metric.OtherCause
