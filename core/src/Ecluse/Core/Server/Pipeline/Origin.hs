-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolving a packument's upstream origins: the per-origin fetch with its
credential posture, the read-handle construction over the mount's dependencies, and
the typed outcome the merge consumes.

The credential-authority invariant lives here (see
@docs\/architecture\/access-model.md@): the private (trusted) origin is fetched
__uncached__ with the client's own forwarded credential, so the upstream
re-authorises every client itself, while the public origin is fetched __anonymous__
(the client's credential is stripped before any public-upstream fetch) and resolved
through the shared metadata cache, one shared document serving every client. A fetch
that fails degrades to no contribution rather than an error; a self-reported
/different/ package name is kept distinct ('OriginNameMismatch') so the
no-valid-origin terminal can render a @502@ apart from a transient outage.

"Ecluse.Core.Server.Pipeline.Packument" gates, merges, and serves what resolves
here; "Ecluse.Core.Server.Pipeline.Tarball" shares the public read handle
('withPublicMetadataClient') so its single-version gate and the packument fetch
collapse onto one cache entry.
-}
module Ecluse.Core.Server.Pipeline.Origin (
    -- * A resolved contribution
    Contribution (..),
    fingerprintPiece,

    -- * The per-origin outcome
    OriginResult (..),
    originManifest,

    -- * Fetching the two origins
    fetchPrivateOrigin,
    fetchPublicOrigin,
    withPublicMetadataClient,
) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS), logFM, ls)
import Network.HTTP.Client (Manager)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Package.Merge (Provenance)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest,
    MetadataClient (fetchFullManifest),
    MetadataError (MetadataNameMismatch),
 )
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ServeRuntime (..),
 )
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached, Uncached))
import Ecluse.Core.Server.Pipeline.Diagnostics (logInvalidEntries, logMetadataFailure)
import Ecluse.Core.Telemetry.Metrics qualified as Metric

{- | A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve, and the
origin body's 'ContentDigest' for the derived validator. Pairing the views is
the decision-surface\/served-surface contract -- every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline.
-}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    , srcDigest :: ContentDigest
    }

{- | One source's slice of the derived validator: its provenance, its origin body's
digest, and the version keys that actually survived its gate -- together with the
mount base URL and package name, exactly the inputs the assembled document is a
deterministic function of (the plan itself derives from these).
-}
fingerprintPiece :: Contribution -> (Provenance, ContentDigest, [Text])
fingerprintPiece s = (srcProvenance s, srcDigest s, Map.keys (infoVersions (srcInfo s)))

{- | The outcome of resolving one upstream origin for a packument, beyond the
plain "resolved or not" the merge consumes: a name mismatch is kept distinct from a
plain non-resolution so the no-valid-origin terminal status can render a @502@ (a
responding upstream returned a packument for a different package) apart from a
transient outage or a genuine absence.
-}
data OriginResult
    = -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved Manifest
    | {- | The origin answered, but its packument self-reported a name for a /different/
      package -- dropped as untrusted for this request, and a @502@ signal when no
      origin is valid.
      -}
      OriginNameMismatch
    | {- | The origin did not yield a usable packument -- unreachable, undecodable, or a
      genuine absence -- the existing degrade (no contribution).
      -}
      OriginUnresolved
    | {- | The origin is not configured on this mount (a serve-only mount with no
      private upstream): structurally absent, kept distinct from 'OriginUnresolved'
      so an unconfigured leg never contributes the degraded-availability signal a
      __failed__ fetch rightly does.
      -}
      OriginAbsent

{- | The resolved manifest an origin contributed, if any. A name mismatch, a plain
non-resolution, and an absent origin alike contribute no document to the merge.
-}
originManifest :: OriginResult -> Maybe Manifest
originManifest = \case
    OriginResolved manifest -> Just manifest
    OriginNameMismatch -> Nothing
    OriginUnresolved -> Nothing
    OriginAbsent -> Nothing

{- Classify a per-origin full-manifest fetch into an 'OriginResult'. Every fetch outcome
-- an unreachable upstream included -- arrives typed in the 'MetadataError' channel and
degrades to no contribution; a 'MetadataNameMismatch' is kept distinct as
'OriginNameMismatch' so the no-valid-origin terminal status can render a @502@ (a
responding upstream answered for a different package) apart from a transient outage, an
undecodable body, or a bound breach. The 'tryAny' arm is the per-origin degrade boundary
for an __invariant break only__ (the fetch is total by type): a handle that escapes its
contract still costs one origin's contribution, never the whole merge. -}
originResultOf :: Either SomeException (Either MetadataError Manifest) -> OriginResult
originResultOf = \case
    Left _ -> OriginUnresolved
    Right (Left (MetadataNameMismatch _)) -> OriginNameMismatch
    Right (Left _) -> OriginUnresolved
    Right (Right manifest) -> OriginResolved manifest

{- | Resolve the private (trusted) upstream origin, __uncached__, forwarding the client's
own credential (the default @passthrough@ posture). Returns its coherent (parsed
packument, raw @Value@) pair -- or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error: the merge
serves the best-effort union of whatever resolved (partial-upstream availability).

Under @passthrough@ the private upstream is the per-client authority for who may read
what, so its metadata is __not__ shared across clients: it is fetched and parsed on
__every__ request with that client's own forwarded token, so the upstream re-authorises
each client itself. Caching it would key on the base URL alone (no credential
dimension), so within the TTL one client's cache hit would skip the fetch and serve
another client's private document -- bypassing the upstream's authorisation. The private
origin is therefore deliberately kept out of the metadata cache; only the anonymous
public origin is cached. (How a non-@passthrough@ strategy can instead share the private
origin safely is the serve-time authorisation it adds -- see
@docs\/architecture\/access-model.md@.)
-}
fetchPrivateOrigin :: PackumentDeps -> ServeRuntime -> Maybe Secret -> PackageName -> Handler OriginResult
fetchPrivateOrigin deps rt token name = case pdPrivateBaseUrl deps of
    -- No private upstream on this mount (a serve-only pure public gate): the leg is
    -- structurally absent, so no client is constructed and no fetch is attempted.
    Nothing -> pure OriginAbsent
    Just privateBase -> do
        logFM DebugS (ls ("fetching private origin for " <> renderPackageName name))
        resolved <-
            tryAny $
                withMetadataClient rt deps Metric.Private Uncached (pdLimits deps) (srPrivateManager rt) privateBase token $ \client ->
                    fetchFullManifest client name
        pure (originResultOf resolved)

{- | Resolve the public (gated, anonymous) upstream origin through the metadata cache,
keyed by the origin's base URL as its 'Source', returning its coherent (parsed
packument, raw @Value@) pair -- or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error.

The public origin is anonymous (no client credential), so a single cached entry serves
every client without crossing any trust boundary -- there is no per-client authority
to preserve, only one shared anonymous document. A hit returns the cached pair
(typed view and the exact bytes it was decoded from), so the served document and the
decision over it stay coherent across the TTL, and concurrent resolutions of a
popular package __collapse to one upstream call__ -- as does the tarball gate's
single-version read, which shares this very cache entry ('fetchVersionMetadata').
-}
fetchPublicOrigin :: PackumentDeps -> ServeRuntime -> PackageName -> Handler OriginResult
fetchPublicOrigin deps rt name = do
    logFM DebugS (ls ("fetching public origin for " <> renderPackageName name))
    resolved <-
        tryAny $
            withPublicMetadataClient rt deps (pdPublicBaseUrl deps) $ \client ->
                fetchFullManifest client name
    pure (originResultOf resolved)

{- Construct a per-request read handle for one origin and run an action over it, with the
ambient @katip@ context captured into the handle's failure log.

The handle's operations run the fetch in plain 'IO' (the public origin's cache leader runs
under @mask@); 'withRunInIO' discharges the 'Handler' logs to 'IO' while capturing the
request's trace-correlated context, so a breach\/decode\/name-mismatch warning, or the
dropped-entry warning a successful-but-degraded projection emits ('logInvalidEntries'),
still rides that context. The npm origin's credential posture, manager, base URL, and
response budget are the per-fetch 'NpmClientConfig'; the 'ManifestCaching' decides whether
the origin resolves through the shared metadata cache.

Every response bound (security.md invariant 4) is enforced inside the handle's fetch
against the mount's 'Limits' budget -- a body-size, nesting-depth, or version-count breach
becomes a 'MetadataBoundExceeded', logged once at a 'WarningS' (naming the package and the
ceiling crossed) before it degrades the contribution fail-closed, so an operator can tell a
hostile\/oversized upstream from an ordinary parse failure. -}
withMetadataClient ::
    ServeRuntime ->
    PackumentDeps ->
    Metric.Upstream ->
    ManifestCaching ->
    Limits ->
    Manager ->
    Text ->
    Maybe Secret ->
    (MetadataClient -> IO a) ->
    Handler a
withMetadataClient rt deps upstream caching limits manager baseUrl token k =
    withRunInIO $ \runInIO ->
        k $
            pdNewMetadataClient
                deps
                (srTracing rt)
                (srMetrics rt)
                upstream
                caching
                (\nm err -> runInIO (logMetadataFailure nm baseUrl err))
                (\nm entries -> runInIO (logInvalidEntries nm baseUrl entries))
                (\nm -> runInIO (logFM DebugS (ls ("fetching packument from origin for " <> renderPackageName nm))))
                limits
                manager
                baseUrl
                token

{- | The public origin's read handle: anonymous (no token), resolved through the shared
metadata cache under the base URL's 'Source'. Both the packument fetch ('fetchFullManifest')
and the tarball gate's single-version read ('fetchVersionMetadata') go through this handle,
so they share one cache entry.
-}
withPublicMetadataClient :: ServeRuntime -> PackumentDeps -> Text -> (MetadataClient -> IO a) -> Handler a
withPublicMetadataClient rt deps baseUrl =
    withMetadataClient rt deps Metric.Public (Cached (srMetadataCache rt) (Source baseUrl)) (pdLimits deps) (srPublicManager rt) baseUrl Nothing
