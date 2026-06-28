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

Both operations of a handle resolve the full manifest the same way, so a handle's
single-version op shares the very cache entry its full-manifest op populates: a
packument @GET@ followed by its tarball gate collapses to one upstream call, and the
single-version op then selects the one version from the shared entry.
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

import Ecluse.Core.Package (PackageDetails, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Registry.Metadata (
    MetadataClient (..),
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Registry.Npm (NpmClientConfig)
import Ecluse.Core.Registry.Npm.Metadata (fetchNpmManifest)
import Ecluse.Core.Server.Cache (
    CacheEntry (CacheEntry, entryInfo, entryRaw),
    MetadataCache,
    Source,
    resolveMetadata,
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

{- | Build a per-request read handle from a raw, uncached fetch primitive, wiring it
with the caching policy, the upstream-fetch metrics, and a request-context failure log.

The single-version op shares the full-manifest op's cache resolution and then selects
the one version locally — current npm behaviour (full-decode-then-pick), preserved
behind the boundary so a selective parse can replace it later without a caller change.

The failure log is invoked __once per real fetch__ (inside the cache's single-flight
leader), in the caller's logging context, so a coalesced follower never re-logs a
failure the leader already reported.
-}
newMetadataClient ::
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> IO (Either MetadataError (PackageInfo, Value))) ->
    MetadataClient
newMetadataClient metrics upstream caching logFailure rawFetch =
    MetadataClient
        { fetchFullManifest = resolveFull
        , fetchVersionMetadata = \name version ->
            (fmap . fmap) (selectVersion version) (resolveFull name)
        }
  where
    selectVersion :: Version -> (PackageInfo, Value) -> Maybe PackageDetails
    selectVersion version (info, _raw) = Map.lookup (renderVersion version) (infoVersions info)

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
        Uncached -> leader name
        Cached cache source -> resolveMetadata metrics cache source name (leader name)

    -- The single-flight leader action: the real fetch, run only on a cache miss,
    -- metered and (on failure) logged once before the carrier is raised.
    leader :: PackageName -> IO CacheEntry
    leader name =
        recordedFetch metrics upstream $
            rawFetch name >>= \case
                Right (info, raw) -> pure (CacheEntry info raw)
                Left err -> logFailure name err >> throwIO (ManifestFetchFailed err)

{- | Build a per-request read handle for the npm protocol over one origin's fetch
configuration: the npm full-manifest fetch as the raw primitive, with the serve-path
caching, metrics, and failure log wired by 'newMetadataClient'.
-}
newNpmMetadataClient ::
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    NpmClientConfig ->
    MetadataClient
newNpmMetadataClient metrics upstream caching logFailure config =
    newMetadataClient metrics upstream caching logFailure (fetchNpmManifest config)

{- The in-band failure carrier for a leader fetch: a 'MetadataError' raised so the
shared metadata cache caches nothing on failure and re-raises it to coalesced
followers, then converted back to a 'Left' at the resolve boundary. Internal — the
serve path only ever sees the returned 'Either' (or a genuine transport throw). -}
newtype ManifestFetchFailed = ManifestFetchFailed MetadataError
    deriving stock (Show)

instance Exception ManifestFetchFailed

{- Record one upstream metadata fetch around the leader action: its latency on a
successful resolve, or the bounded error cause otherwise, before re-raising so the
caller's degrade is unchanged. Wrapping the leader — which runs only on a cache miss —
means the public path records real upstream calls, not cache hits. -}
recordedFetch :: MetricsPort -> Metric.Upstream -> IO CacheEntry -> IO CacheEntry
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
    | Just (_ :: HTTP.HttpException) <- fromException err = Metric.Connection
    | otherwise = Metric.OtherCause

metadataErrorCause :: MetadataError -> Metric.Cause
metadataErrorCause = \case
    MetadataUndecodable -> Metric.Decode
    MetadataNameMismatch _ -> Metric.Decode
    MetadataBoundExceeded _ -> Metric.OtherCause
