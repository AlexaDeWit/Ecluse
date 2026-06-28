{- | The serve paths behind the package routes: the packument merge behind
@GET \/{pkg}@ and the artifact relay behind @GET \/{pkg}\/-\/{file}.tgz@.

This is the data-plane handler module. It composes the
slices that decide /what/ to serve — the registry client
("Ecluse.Core.Registry.Npm"), the per-version rules ("Ecluse.Core.Rules"), the structural
filter ("Ecluse.Core.Registry.Npm.Filter"), the cross-upstream merge
("Ecluse.Core.Package.Merge"), the metadata cache ("Ecluse.Core.Server.Cache"), the
own-ETag conditional ("Ecluse.Core.Server.Conditional"), and the serve-outcome status
("Ecluse.Core.Server.Response") — into one action in the
'Ecluse.Core.Server.Context.Handler' reader, reading its mount's serve dependencies and
the request runtime 'Ecluse.Core.Server.Context.ServeRuntime' from the request's
'Ecluse.Core.Server.Context.RequestCtx'.

== Credential authority

This handler implements the default @passthrough@ credential posture (see
@docs\/architecture\/access-model.md@). The invariant that holds under __every__
strategy is the __public strip__: the client's credential is __stripped before any
public-upstream fetch__, which is always anonymous — sending an internal token to the
public registry would be a credential disclosure, so the public-upstream fetch is built
with no token at all. Under @passthrough@ the client's own credential is additionally
__forwarded verbatim to the private upstream__, which is the authority for who may
read what. The two origins are fetched concurrently, each with its own credential
posture; nothing shares a token across the trust split.

Because @passthrough@ makes the private upstream the __per-client authority__, its
metadata is __not cached across clients__ here: the private origin is fetched and parsed on
every request with that client's own credential, so the upstream re-authorises each
client itself, and only the anonymous public origin is cached (one shared document, no
per-client authority to preserve). Caching the private origin keyed by base URL alone
would let one client's cached entry serve another client's private document within the
TTL, bypassing the upstream's authorisation — a cross-client disclosure. (Other
strategies make the private origin shareable by authorising each serve differently; the
metadata cache itself stays credential-free regardless — see
@docs\/architecture\/access-model.md@ → "Caching".)

== Merge, not fallback

A packument is the /set of available versions/, spread across upstreams, so it is
__merged__ rather than short-circuited on a private hit (see
@docs\/architecture\/registry-model.md@ → "Packument merge across upstreams").
Private versions are trusted and enter unfiltered; public versions are gated
through the rules and the structural filter ('filterPlan' decides, 'applyFilterPlan'
replays) before they enter; the two are combined, private winning a collision and
an integrity divergence flagged. If one upstream
is unavailable while the other succeeds, the best-effort union of what resolved is
served — only when /nothing/ resolves does the request error.

== Decision surface vs served surface

The merge and filter reason over the /typed/ 'PackageInfo' but the document served
is the __raw upstream JSON__, edited in place, so every unmodeled wire key
survives (see @docs\/architecture\/registry-model.md@ → "Decision surface vs
served surface"). The 'MergePlan' names, for each surviving version, the source
that won it; the served body is assembled by taking each survivor's object from
the /raw @Value@/ of its winning source, carrying the reconciled @dist-tags@ and
@time@, and relaying every other top-level key from the precedence-winning
document. The typed model is never re-serialised. The two fields the merge /owns/ as
a decision — @dist-tags.latest@ and the @time@ instants — are re-rendered from that
decision (the times as normalised ISO-8601), so they may differ byte-for-byte from
any single upstream while denoting the same value; integrity-bearing fields
(@dist.integrity@, @dist.tarball@) are relayed raw and untouched. The served bytes
get our __own ETag__, since a merged\/filtered body matches no single upstream's.

== Ecosystem coupling

This is the __npm__ packument pipeline: it reaches for the npm registry
client, projection, and structural filter directly, so it is the one
serve-path module that depends on a concrete adapter. The coupling is
expedient, not intended — the agnostic handles that would let it dispatch through an
adapter (a per-adapter router, and an ecosystem-neutral filter\/projection) would
let a second ecosystem reuse this orchestration unchanged.

== Artifact path

The tarball handler ('serveTarball') is the demand-driven artifact relay. Its two legs
locate the tarball differently, by the trust of their origin.

The __private__ leg is a __conventional stable read__: it fetches the tarball at
@{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ ('artifactRequestByFile'), addressed by the
client's requested filename, __without a private-packument fetch__ — the stable,
cacheable shape an @npm ci@ install issues, so a worst-case lockfile fan-out pays one
artifact round-trip per tarball rather than a packument fetch+decode per tarball it
would only discard. The request __forwards the client's credential__ over the
__trusted__ manager, attached at the single bearer-attach point
('Ecluse.Core.Registry.Npm.withToken'), which pins @redirectCount = 0@: this
credential-bearing read __never follows a redirect__ (a private CDN @302@ is returned to
the serve path, not chased with the bearer). The constructed URL is on the private base
host, so the 'Ecluse.Core.Security.TrustedOrigin' tarball-host gate is satisfied
__same-host__, and the trusted origin is exempt from the internal-range block (a private
registry on an internal address still serves). A @2xx@ streams the artifact through with
__bounded memory__ (the @withResponse@\/@responseStream@ relay, never a buffering fetch)
and __answers the request__; a non-@2xx@ status or a connection failure is a __clean
miss__ that falls through to the public leg.

The private leg applies __no serve-time integrity floor__. An established version pinned
in a consumer's lockfile and served from an operator-__trusted__ private registry is
fast-tracked: its bytes are still verified __client-side by @npm@__ (against the
@dist.integrity@ it resolved over the packument route) and by the __mirror worker__ on
ingestion, so fast-tracking gives up only the proactive "refuse weak-integrity" stance,
not tamper-evidence. A consequence of the conventional read: a private upstream that
serves its tarball __off the conventional @\/-\/@ path__ (a separate files host, a signed
CDN URL the convention cannot rebuild) is not reached by this leg, so it is a private
miss that falls through to the public origin.

The __public__ leg honours the __authoritative upstream location__ — the
@Artifact.artUrl@ the projection preserved from the gated version's @dist.tarball@,
selected by the requested filename — rather than reconstructing the conventional path,
so the proxy can front a public registry that serves its artifacts from a separate host
or an off-convention path (a CDN\/files host, a signed URL). That location is gated, not
trusted: it is fetched only when the tarball-host policy
('Ecluse.Core.Security.tarballHostAllowed', per @PROXY_RESPECT_UPSTREAM_TARBALL_HOST@)
admits its host — the default refuses a cross-host @dist.tarball@ — and the untrusted
egress additionally carries the resolved-IP recheck. The public leg is anonymous: it
gates __that one version__ against the rules (the same machinery the packument path
gates the whole set with) and selects the artifact, and on an admit __streams the public
bytes from @artUrl@ and enqueues a 'Ecluse.Core.Queue.MirrorJob'__ (naming that
authoritative URL) for the worker to back-fill the mirror target; on a reject —
including a host the tarball-host policy refuses — it renders the serve error model
(@403@\/@503@\/@500@\/@404@) through the mount's renderer. The enqueue is
__serve-then-enqueue, best-effort and non-blocking__: the artifact reaches the client
first, and an enqueue failure is swallowed rather than failing or delaying the response.
Mirroring is __demand-driven__ — a job is enqueued only here, on a tarball-path admit,
never when a packument is filtered. The serve path does __not__ verify @dist.integrity@;
the client checks the artifact's own hash and the worker re-verifies before publishing.

An artifact is a __pass-through__ body — served byte-identical to upstream's — so its
conditional-GET handling __relays__ rather than computing an own ETag (see
@docs\/architecture\/web-layer.md@ → "Middleware and helper libraries", and contrast
the merged-packument own-ETag path): the client's @If-None-Match@\/@If-Modified-Since@
are forwarded onto the upstream artifact request on __both__ legs ('forwardValidators'),
and an upstream @304 Not Modified@ is relayed straight back to the client as a bodiless
@304@ ('isNotModified' via the relay's accept predicate) rather than re-downloading the
tarball — the cheap freshness check on the hot artifact path.
-}
module Ecluse.Core.Server.Pipeline (
    -- * The packument handler
    servePackument,
    headPackument,

    -- * The tarball handler
    serveTarball,
    headTarball,

    -- * The first-party publish handler
    servePublish,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)
import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, hAuthorization, hContentLength, hContentType, methodHead, mkStatus, status200, status401, status403, status405, status500, status501, status502, statusIsSuccessful)
import Network.Wai (Request, Response, ResponseReceived, consumeRequestBodyStrict, requestHeaders, responseHeaders, responseLBS, responseStatus)
import UnliftIO (concurrently, withRunInIO)
import UnliftIO.Exception (handle, throwIO, tryAny)

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Package (
    Artifact (artFilename, artHashes, artSize, artUrl),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoPublishedAt, infoVersions),
    PackageName,
    Scope,
    pkgNamespace,
    renderPackageName,
 )
import Ecluse.Core.Package.Filter (filterPlanFromDecisions, fpSurvivors)
import Ecluse.Core.Package.Integrity (
    MinTrustedIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyArtifacts,
 )
import Ecluse.Core.Package.Merge (
    MergePlan (mpDistTags, mpSurvivors, mpTime),
    Provenance (GatedSource, TrustedSource),
    SourceId,
    mergePackuments,
 )
import Ecluse.Core.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (MirrorJob, jobArtifact, jobArtifactUrl, jobMirrorTarget, jobPackage, jobTraceContext, jobVersion),
    enqueue,
 )
import Ecluse.Core.Registry (RegistryResponse (responseBody), UrlFormationError)
import Ecluse.Core.Registry.Npm (
    MetadataForm (Full),
    NpmClientConfig (..),
    PublishRelayResponse (PublishRelayResponse),
    ResponseBoundExceeded (ResponseBoundExceeded),
    artifactRequestByFile,
    artifactRequestByUrl,
    fetchMetadataForm,
    noValidators,
    relayPublishDocument,
 )
import Ecluse.Core.Registry.Npm.Filter (FilterResult (Filtered, NoSurvivors), applyFilterPlan, rewriteTarballUrls)
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue, projectName)
import Ecluse.Core.Rules (evalRules)
import Ecluse.Core.Rules.Types (Decision, EvalContext (EvalContext))
import Ecluse.Core.Security (
    LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions),
    Limits,
    LoweredHostSet,
    Origin (TrustedOrigin, UntrustedOrigin),
    checkNestingDepth,
    checkVersionCount,
    hostAddress,
    lowerCaseHosts,
    tarballHostAllowed,
 )
import Ecluse.Core.Server.Cache (CacheEntry (CacheEntry, entryInfo, entryRaw), Source (Source), resolveMetadata)
import Ecluse.Core.Server.Conditional (Conditional (Modified, NotModified), etagHeader, evaluateOwnETag, forwardValidators, isNotModified)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingPublishDeps, bindingRenderer),
    PackumentDeps (..),
    PublishDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Pipeline.Internal (
    PackumentNameMismatch (PackumentNameMismatch),
    PackumentUndecodable (PackumentUndecodable),
    admitByIntegrity,
    evalTier,
    fetchCause,
    logDecodeFailure,
    logNameMismatch,
    packumentServeDecision,
    recordDenials,
    recordEffectfulFailures,
    serveDecisionClass,
 )
import Ecluse.Core.Server.Response (
    ArtifactStatus (Forbidden, NotFound, Ok, ServerError, Unavailable'),
    MountRenderer,
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    RejectReason (BelowIntegrityFloor, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
    RenderedBody (RenderedBody),
    RetryAfter (RetryAfter),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    artifactStatus,
    artifactStatusCode,
    packumentStatus,
    packumentStatusCode,
    renderError,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Route (Filename (Filename))
import Ecluse.Core.Server.Stream (probeUpstreamWhen, streamUpstreamWhen)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (spanMirrorEnqueue, spanRuleEval)
import Ecluse.Core.Version (Version, renderVersion)

-- ── the handler ─────────────────────────────────────────────────────────────

{- | Serve a @GET \/{pkg}@ packument request end to end, over the request's
'RequestCtx'.

The mount's 'PackumentDeps' and error renderer are read from the matched
'MountBinding' in context, not threaded as arguments. When the mount has no
packument-serve dependencies wired, the route is recognised but not served — a
@501@ in the mount's surface — rather than fabricating a result.

With dependencies wired: the edge token, if configured, is validated before any
upstream is touched. Then the private and public upstreams are fetched
__concurrently__ — the client's credential forwarded to the private origin, the public
origin anonymous — each parse failure or unavailable upstream degrading to a missing
contribution rather than an error. Private versions are trusted as-is; public
versions are gated through the rules and the structural filter ('filterPlan' then
'applyFilterPlan'); the surviving sets are merged ('mergePackuments') and the
'MergePlan' replayed onto the raw upstream @Value@s to assemble the served body,
which is then answered against the client's conditional request with our own ETag.
When nothing survives, the status follows the most recoverable cause via
'packumentStatus'. An origin whose self-reported packument name disagrees with the
route is validated out — dropped as untrusted for this request and logged — so a
single misreporting upstream never denies a package another upstream serves; when
that leaves __no__ valid origin, the request is a @502@ (a responding upstream
returned an invalid response), distinct from a genuine absence. Every refusal — the
edge @401@ and the no-survivors @403@\/@503@\/@502@\/@500@ — is rendered through the
mount's 'MountRenderer'.
-}
servePackument ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePackument = packumentWith PackumentFull

{- | Serve a @HEAD \/{pkg}@ packument request: the __identical pipeline and gating__ as
'servePackument' — the same fetch, merge, filter, rule decision, and no-survivors
status — answered with the __identical status and headers__ as the @GET@ (the would-be
merged body's @Content-Length@ and the own @ETag@ the conditional-request machinery
computes), but with the body suppressed ('bodiless'), as HTTP semantics require of a
@HEAD@ reply.

A packument body is assembled __locally__ (a metadata fetch plus the cross-upstream
merge), so — unlike the tarball @HEAD@ ('headTarball') — answering it pumps __no
artifact body__ and carries no egress-amplification risk: this is the HTTP-correctness
half of the explicit-@HEAD@ handling, not the DoS lever the tarball path closes. The
merged body is still materialised, to size it and compute its @ETag@; only the bytes
are withheld from the reply.
-}
headPackument ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
headPackument name request respond =
    packumentWith PackumentHead name request (respond . bodiless)

{- The packument serve mode threaded through the handler: a full @GET@ that serves the
merged body, or a @HEAD@ that answers the identical status and headers with the body
suppressed. It changes exactly one thing in the pipeline — whether the @200@ success
path stamps the would-be body's @Content-Length@ (a @HEAD@ does, so a client sees the
framing a @GET@ would; a @GET@ leaves that to the serving layer, which frames the body
it actually writes). The body itself is withheld uniformly by the 'bodiless' wrapper
'headPackument' applies, and the gating is byte-for-byte identical between the two. -}
data PackumentServe
    = -- A @GET@: serve the merged packument body.
      PackumentFull
    | -- A @HEAD@: serve the identical status and headers (the would-be body's
      -- @Content-Length@ and the own @ETag@) with no body.
      PackumentHead

-- Dispatch shared by 'servePackument' and 'headPackument': resolve the mount's
-- dependencies (or the recognised-but-unserved @501@ stub) and serve in the given mode.
packumentWith ::
    PackumentServe ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
packumentWith mode name request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPackumentDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (recognisedButUnserved renderer))
        Just deps -> serveWithDeps mode renderer deps name request respond

-- Serve a packument once the mount's dependencies are known: fetch, gate, merge,
-- and answer — the credential-authority and merge logic the module header
-- describes. The request runtime is read from the request context. The
-- 'PackumentServe' mode is threaded to the success path so a @HEAD@ stamps the
-- would-be body's @Content-Length@ (the 'bodiless' wrapper withholds the bytes).
serveWithDeps ::
    PackumentServe ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveWithDeps mode renderer deps name request respond
    | not (edgeAuthorised deps request) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        rt <- asks ctxRuntime
        let metrics = srMetrics rt
        evalCtx <- liftIO (EvalContext <$> pdNow deps)
        let clientToken = forwardedToken request
        (privResult, pubResult) <-
            concurrently
                (fetchPrivateOrigin (pdLimits deps) rt (pdPrivateBaseUrl deps) clientToken name)
                (fetchPublicOrigin (pdLimits deps) rt (pdPublicBaseUrl deps) name)
        (public, publicExclusions) <- liftIO (gatePublic metrics deps evalCtx (originPackument pubResult))
        let (private, privateExclusions) = admitTrusted (pdMinTrustedIntegrity deps) (originPackument privResult)
            sources = catMaybes [private, public]
        case assemble deps sources of
            Just body -> do
                liftIO (mpServeDecision metrics Metric.Admit)
                liftIO (respond (servePackumentBody mode request body))
            Nothing -> do
                let decisions = collectDecisions privResult pubResult (privateExclusions <> publicExclusions)
                liftIO (mpServeDecision metrics (packumentServeDecision decisions))
                liftIO (recordDenials metrics decisions)
                liftIO (respond (noSurvivors renderer deps decisions))

-- A recognised-but-unserved packument route: a @501@ in the mount's surface, for a
-- mount whose packument-serve dependencies are not wired. The decision to serve or
-- stub is the handler's, so the routing layer need not re-derive it.
recognisedButUnserved :: MountRenderer -> Response
recognisedButUnserved renderer =
    renderedResponse status501 [] (renderError renderer Nothing "this route is recognised but not yet served by this proxy")

-- ── edge authentication ──────────────────────────────────────────────────────

{- Whether the request carries the configured inbound token. With no token
configured the edge is open; with one configured the request's bearer
@Authorization@ must match it exactly. Deny-by-default: a missing or mismatched
token is rejected. The token match is constant-time: 'Secret' equality compares
over the full UTF-8 bytes without a content-dependent early out, so this gate
does not leak the configured token's prefix length through timing. -}
edgeAuthorised :: PackumentDeps -> Request -> Bool
edgeAuthorised deps = edgeTokenAuthorised (pdInboundToken deps)

{- The shared edge gate against a configured inbound token: with none configured the
edge is open; with one configured the request's bearer must match it exactly
(deny-by-default, constant-time over the full 'Secret' bytes). The packument, tarball,
and publish paths all apply the same gate, so it is factored here rather than
duplicated per route. -}
edgeTokenAuthorised :: Maybe Secret -> Request -> Bool
edgeTokenAuthorised expected request = case expected of
    Nothing -> True
    Just want -> forwardedToken request == Just want

-- A @401@ for a request that failed edge authentication, before any upstream
-- fetch; the body is shaped by the mount's renderer.
edgeUnauthorised :: MountRenderer -> Response
edgeUnauthorised renderer =
    renderedResponse status401 [] (renderError renderer Nothing "authentication required")

{- The client's forwarded bearer credential, recovered from the request's
@Authorization: Bearer …@ header. 'Nothing' when no bearer credential is present;
the recovered 'Secret' is what is forwarded to the private upstream and compared
against the edge token. The scheme name is matched case-insensitively (npm sends
@Bearer@), the token taken verbatim after it. -}
forwardedToken :: Request -> Maybe Secret
forwardedToken request = do
    (_, raw) <- find ((== hAuthorization) . fst) (requestHeaders request)
    let value = decodeUtf8 raw
        (scheme, rest) = T.break (== ' ') value
    guard (T.toLower scheme == "bearer")
    let token = T.dropWhile (== ' ') rest
    guard (not (T.null token))
    pure (mkSecret token)

-- ── per-origin fetch ──────────────────────────────────────────────────────────

{- A successfully resolved upstream contribution: the parsed packument used to
decide, alongside the raw @Value@ that is edited in place to serve. Pairing them
is the decision-surface\/served-surface contract — every stage carries the raw
@Value@ next to the typed view so losslessness survives the pipeline. -}
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: Value
    }

{- The outcome of resolving one upstream origin for a packument, beyond the
plain "resolved or not" the merge consumes: a name mismatch is kept distinct from a
plain non-resolution so the no-valid-origin terminal status can render a @502@ (a
responding upstream returned a packument for a different package) apart from a
transient outage or a genuine absence. -}
data OriginResult
    = -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved (PackageInfo, Value)
    | {- | The origin answered, but its packument self-reported a name for a /different/
      package — dropped as untrusted for this request, and a @502@ signal when no
      origin is valid.
      -}
      OriginNameMismatch
    | {- | The origin did not yield a usable packument — unreachable, undecodable, or a
      genuine absence — the existing degrade (no contribution).
      -}
      OriginUnresolved

-- The resolved (packument, raw @Value@) pair an origin contributed, if any. A name
-- mismatch and a plain non-resolution alike contribute no document to the merge.
originPackument :: OriginResult -> Maybe (PackageInfo, Value)
originPackument = \case
    OriginResolved pair -> Just pair
    OriginNameMismatch -> Nothing
    OriginUnresolved -> Nothing

{- Classify a caught origin fetch into an 'OriginResult': a success is 'OriginResolved';
a 'PackumentNameMismatch' throw (the validated-name degrade) is kept distinct as
'OriginNameMismatch'; every other degrade (outage, bound breach, undecodable body) is a
plain 'OriginUnresolved'. -}
originResultFrom :: Either SomeException CacheEntry -> OriginResult
originResultFrom = \case
    Right entry -> OriginResolved (unpair entry)
    Left err -> case fromException err of
        Just PackumentNameMismatch -> OriginNameMismatch
        Nothing -> OriginUnresolved

{- Resolve the private (trusted) upstream origin, __uncached__, forwarding the client's
own credential (the default @passthrough@ posture). Returns its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error: the merge
serves the best-effort union of whatever resolved (partial-upstream availability).

Under @passthrough@ the private upstream is the per-client authority for who may read
what, so its metadata is __not__ shared across clients: it is fetched and parsed on
__every__ request with that client's own forwarded token, so the upstream re-authorises
each client itself. Caching it would key on the base URL alone (no credential
dimension), so within the TTL one client's cache hit would skip the fetch and serve
another client's private document — bypassing the upstream's authorisation. The private
origin is therefore deliberately kept out of the metadata cache; only the anonymous
public origin is cached. (How a non-@passthrough@ strategy can instead share the private
origin safely is the serve-time authorisation it adds — see
@docs\/architecture\/access-model.md@.) -}
fetchPrivateOrigin :: Limits -> ServeRuntime -> Text -> Maybe Secret -> PackageName -> Handler OriginResult
fetchPrivateOrigin limits rt baseUrl token name = do
    resolved <-
        tryAny
            ( recordedFetch
                (srMetrics rt)
                Metric.Private
                (fetchEntry limits (srPrivateManager rt) baseUrl token name)
            )
    pure (originResultFrom resolved)

{- Resolve the public (gated, anonymous) upstream origin through the metadata cache,
keyed by the origin's base URL as its 'Source', returning its coherent (parsed
packument, raw @Value@) pair — or 'Nothing' when the origin is unavailable or its body
does not parse. A failed fetch is a degraded contribution, not an error.

The public origin is anonymous (no client credential), so a single cached entry serves
every client without crossing any trust boundary — there is no per-client authority
to preserve, only one shared anonymous document. A hit returns the cached pair
(typed view and the exact bytes it was decoded from), so the served document and the
decision over it stay coherent across the TTL, and concurrent resolutions of a
popular package __collapse to one upstream call__. -}
fetchPublicOrigin :: Limits -> ServeRuntime -> Text -> PackageName -> Handler OriginResult
fetchPublicOrigin limits rt baseUrl name = do
    let metrics = srMetrics rt
    resolved <-
        tryAny $
            -- The cache runs the fetch action in plain 'IO' (its single-flight leader
            -- under @mask@); 'withRunInIO' discharges the 'Handler' fetch to 'IO' while
            -- capturing the ambient @katip@ context, so a breach\/decode warning the
            -- leader logs still rides the request's trace-correlated context.
            withRunInIO $ \runInIO ->
                resolveMetadata
                    metrics
                    (srMetadataCache rt)
                    (Source baseUrl)
                    name
                    (runInIO (recordedFetch metrics Metric.Public (fetchEntry limits (srPublicManager rt) baseUrl Nothing name)))
    pure (originResultFrom resolved)

{- Fetch one upstream's full packument (the @Full@ form, for the @time@ map a
publish-age rule needs) and decode it once into both the typed 'PackageInfo' used to
decide and the raw @Value@ edited in place to serve. The two come from the /same/
fetch, so the decision is taken over exactly the bytes served. A body that does not
decode into both throws, so the fetch degrades to a missing contribution rather than
failing the whole request. The injected token is the fetch's credential posture (the
client's for the private origin, 'Nothing' for the anonymous public origin).

The response bounds (security.md invariant 4) are enforced here against the mount's
'Limits' budget, every breach mapped onto the same fail-closed degraded path a parse
failure already takes (the throw that 'fetchPrivateOrigin'\/'fetchPublicOrigin' catch
to 'Nothing') — but each breach is __logged at a 'WarningS'__ first, naming the
package and the ceiling it crossed, so an operator can tell a hostile\/oversized
upstream (or a too-tight cap) from an ordinary parse failure:

\* __body size__ — 'fetchMetadataForm' reads the body through
  'Ecluse.Core.Security.boundedRead' against the budget's @maxBodyBytes@, so an oversized
  body raises a 'ResponseBoundExceeded' from the fetch before it is ever buffered
  whole; it is caught here, logged, and re-raised;
\* __nesting depth__ — 'Ecluse.Core.Security.checkNestingDepth' is applied on the decoded
  @Value@, before it is projected or deeply traversed, so a pathologically nested
  payload is refused before any deep walk. (The structure is already
  /bounded-by-body-size/ at the parser — the @maxBodyBytes@ cap above precedes the
  decode — so this guard bounds the /traversal/ cost of a within-size-but-deep
  document, not an unbounded one.)
\* __version count__ — 'Ecluse.Core.Security.checkVersionCount' is applied after projection,
  before the document threads into rule evaluation, so a version-flood packument is
  refused before per-version rules run.

A pathological document is therefore refused outright, never partially served. -}
fetchEntry :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> Handler CacheEntry
fetchEntry limits manager baseUrl token name = do
    -- The body-size breach is raised from the bounded read as a typed
    -- 'ResponseBoundExceeded'; log it (which ceiling, observed-vs-cap) before letting
    -- it propagate to the origin fetcher's @tryAny@, the same fail-closed degrade.
    response <-
        handle (\(ResponseBoundExceeded err) -> logBreach name err *> throwIO (ResponseBoundExceeded err)) $
            liftIO (fetchMetadataForm (clientConfig limits manager baseUrl token) Full noValidators name)
    -- Decode the body once into the raw @Value@, then project the typed view from
    -- that same parse: aeson decodes bytes to a 'Value' and runs the 'FromJSON'
    -- instance either way, so projecting from the @Value@ reuses the one parse
    -- rather than tokenising a multi-megabyte packument a second time.
    case Aeson.eitherDecodeStrict (responseBody response) of
        -- Bound the nesting depth on the decoded @Value@, before projecting or
        -- traversing, then the version count after projection, before the document
        -- reaches rule evaluation. A breach of either is logged and then degraded
        -- exactly like a parse failure.
        Right value -> case checkNestingDepth limits value of
            Right bounded -> case parsePackageInfoFromValue name bounded of
                Right (Projected info) -> case checkVersionCount limits info of
                    Right boundedInfo -> pure (CacheEntry{entryInfo = boundedInfo, entryRaw = bounded})
                    Left err -> boundBreach err
                Right (NameMismatch reported) -> nameMismatch reported
                Left _ -> decodeFailure
            Left err -> boundBreach err
        Left _ -> decodeFailure
  where
    -- A nesting/version breach: log which ceiling was crossed, then fail closed as a
    -- typed 'ResponseBoundExceeded' (caught by the origin fetcher's @tryAny@).
    boundBreach :: LimitError -> Handler CacheEntry
    boundBreach err = logBreach name err *> throwIO (ResponseBoundExceeded err)

    decodeFailure :: Handler CacheEntry
    decodeFailure = logDecodeFailure name *> throwIO PackumentUndecodable

    -- The origin answered with a packument self-reporting a different package's name:
    -- log it (both names + this origin) and degrade like a decode failure, but with a
    -- distinct typed throw so the no-valid-origin status can render a @502@.
    nameMismatch :: Text -> Handler CacheEntry
    nameMismatch reported = logNameMismatch name baseUrl reported *> throwIO PackumentNameMismatch

unpair :: CacheEntry -> (PackageInfo, Value)
unpair entry = (entryInfo entry, entryRaw entry)

{- Log a response-bound breach at 'WarningS' before the contribution is degraded
fail-closed, so an operator can distinguish a bound breach (a hostile\/oversized
upstream, or a too-tight cap) from an ordinary parse failure or upstream outage. The
structured payload names the package, which @bound@ was crossed, and the observed
value against its @cap@ — the high-cardinality identifiers that belong on the log
line, not a metric label. Emitted through the ambient @katip@ context (the request's,
so the line carries its trace-correlation @dd@), under the @ecluse@ namespace the rest
of the stream uses. -}
logBreach :: (KatipContext m) => PackageName -> LimitError -> m ()
logBreach name err =
    katipAddContext payload $
        logFM WarningS (ls message)
  where
    -- The package the refused document was for, plus the breach detail, as the
    -- structured @data@ object on the line.
    payload =
        sl "module" pipelineModule
            <> sl "package" (renderPackageName name)
            <> sl "bound" boundName
            <> sl "observed" observed
            <> sl "cap" cap

    -- A human-readable one-line summary; the structured fields carry the detail.
    message :: Text
    message = "refused an upstream metadata document: it exceeded the " <> boundName <> " response bound (observed " <> observed <> ", cap " <> cap <> ")"

    -- Which ceiling, the observed value, and the cap — pulled from the typed error so
    -- the three are always consistent with what was enforced.
    boundName :: Text
    observed :: Text
    cap :: Text
    (boundName, observed, cap) = case err of
        BodyTooLarge c -> ("body-size", "over " <> show c <> " bytes", show c <> " bytes")
        TooManyVersions seen c -> ("version-count", show seen, show c)
        TooDeeplyNested c -> ("nesting-depth", "over " <> show c <> " levels", show c <> " levels")

-- The @module@ tag this module's breach log carries — the operator-facing log filter
-- key, held stable as the current value rather than the source module path, so an
-- operator's saved filter keeps matching across the move into ecluse-core (the only
-- change to these lines is the trace-correlation @dd@ the ambient context adds). The
-- decode-failure log lives in "Ecluse.Core.Server.Pipeline.Internal", tagged likewise.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

{- The npm client config for one fetch: its response-bound budget, 'Manager', base
URL, and injected token (the client's credential for the private origin, 'Nothing'
for the anonymous public origin). The client never originates a token; the authority
model is decided here. The 'Manager' is passed explicitly per fetch — the trusted
'srPrivateManager' for the private upstream, the guarded 'srPublicManager' for the
public\/artifact fetches — so the resolved-IP SSRF recheck applies only to the
untrusted egress. The 'Limits' carries the mount's @maxBodyBytes@ to the bounded
metadata read in 'fetchMetadataForm'. -}
clientConfig :: Limits -> Manager -> Text -> Maybe Secret -> NpmClientConfig
clientConfig limits manager baseUrl token =
    NpmClientConfig
        { npmBaseUrl = baseUrl
        , npmManager = manager
        , npmToken = token
        , npmLimits = limits
        }

{- Apply the __trusted integrity floor__ to a private (trusted) contribution before it
enters the merge, returning the surviving 'Contribution' (if any version survived) and the
per-version exclusions for the dropped ones (for the no-survivors status). This is the
trusted-path mirror of 'gatePublic': a private version whose strongest digest is below the
trusted floor ('pdMinTrustedIntegrity') is dropped from the served listing, so by default
(floor = SHA-256) a SHA-1-only or hashless private version is not listed, while an operator
who loosens the trusted floor admits it again. Trusted versions stay __unfiltered by the
rules__ (the trust split is the caller's); only the integrity floor applies. The raw
@Value@ is kept whole — the merge replays only surviving keys onto it, so a dropped version
is never taken from it; tarball URLs are rewritten at assembly, uniformly across sources. -}
admitTrusted :: MinTrustedIntegrity -> Maybe (PackageInfo, Value) -> (Maybe Contribution, [ServeDecision])
admitTrusted minTrusted = \case
    Nothing -> (Nothing, [])
    Just (info, value) ->
        let (admissible, integrityRefusals) =
                admitByIntegrity minTrusted trustedIntegrityBelowFloor trustedIntegrityMissing info
         in if Map.null (infoVersions admissible)
                then (Nothing, integrityRefusals)
                else (Just (Contribution TrustedSource admissible value), integrityRefusals)

{- Gate a public-upstream contribution through the rules engine and the structural
filter, returning the surviving 'Contribution' (if any survived) and the per-version
exclusion outcomes (for the no-survivors status when nothing survives anywhere).

A public origin that did not resolve contributes nothing and no exclusions. A resolved
origin first has the __integrity-floor admission policy__ applied: any version whose
strongest digest does not meet the configured floor ('pdMinIntegrity') is dropped from
the gated set up front ('admitByIntegrity'), so a below-floor public version is never
listed (a client cannot fetch it — the artifact gate would refuse it anyway) and never
contributes its fingerprint to the merge. The remaining versions are decided by the
rules engine ('Ecluse.Core.Rules.evalRules' — the boot order walked to the first decisive
result), the resulting decisions handed to the agnostic
'filterPlanFromDecisions', and that plan replayed by 'applyFilterPlan' onto the raw
@Value@: 'Filtered' yields a gated 'Contribution' over the surviving versions;
'NoSurvivors' yields no contribution and the per-version 'ServeDecision's, each excluded
version's decision projected (a fail-closed 'Ecluse.Core.Rules.Types.Undecidable' carrying
its transient\/permanent cause, so the no-survivors status is a @503@\/@500@ rather than
a @403@). The dropped below-floor versions are projected as 'MissingIntegrity' (no digest
at all) or 'BelowIntegrityFloor' (a digest, but too weak) refusals and appended to those
exclusions, so a packument with /only/ inadmissible public versions is a @403@ rather
than an empty success. Evaluation is IO (an effectful rule may do IO), so this gate is
IO; with only pure rules it short-circuits without launching any IO.

The gated contribution's typed 'PackageInfo' is __restricted to the survivors__ to
match its filtered @Value@: 'mergePackuments' treats a 'GatedSource' as the
already-filtered set and never re-filters, so feeding it the unfiltered view would
let a denied version reach the merge plan (and skew the reconciled @latest@\/@time@).

This gate runs on the public path only; the trusted (private) contribution is admitted
separately by 'admitTrusted' against the trusted integrity floor (the rules never run on
it — the trust split is the caller's). -}
gatePublic :: MetricsPort -> PackumentDeps -> EvalContext -> Maybe (PackageInfo, Value) -> IO (Maybe Contribution, [ServeDecision])
gatePublic metrics deps ctx = \case
    Nothing -> pure (Nothing, [])
    Just (info, value) -> do
        let (admissible, integrityRefusals) = admitByIntegrity (pdMinIntegrity deps) integrityBelowFloor integrityMissing info
        (decisions, seconds) <- timedSeconds (decideVersions deps ctx admissible)
        mpRuleEvalDuration metrics (evalTier (pdRules deps)) seconds
        recordEffectfulFailures metrics (Map.elems decisions)
        let plan = filterPlanFromDecisions decisions admissible
        pure $ case applyFilterPlan plan value of
            Filtered filtered ->
                (Just (Contribution GatedSource (restrictToSurvivors (fpSurvivors plan) admissible) filtered), integrityRefusals)
            NoSurvivors leftover -> (Nothing, projectDecisions admissible leftover <> integrityRefusals)

{- Decide every version of a public packument against the rules engine, keyed by raw
version string (the map 'filterPlanFromDecisions' consumes). Each version is run
through 'Ecluse.Core.Rules.evalRules', so a fail-closed rule that cannot be computed
yields a 'Ecluse.Core.Rules.Types.Undecidable' decision. With only pure rules the
per-version call short-circuits without launching any IO. -}
decideVersions :: PackumentDeps -> EvalContext -> PackageInfo -> IO (Map Text Decision)
decideVersions deps ctx info =
    traverse (evalRules ctx (pdRules deps)) (infoVersions info)

{- Restrict a 'PackageInfo' to the version keys that survived filtering — the
'Ecluse.Core.Package.Filter.FilterPlan'\'s own 'fpSurvivors', which 'applyFilterPlan' kept
in the filtered @Value@'s @versions@, so the typed view handed to the merge matches
the filtered document. Taking the survivor set from the plan reuses the 'Set' the
filter already built rather than re-deriving it from the filtered @Value@'s keys (a
@Set.fromList@ of every survivor key, each 'Key.toText'-converted, over a packument of
up to the version cap). @dist-tags@ and @time@ are pruned to the surviving keys
likewise (the merge reconciles them over the union); @dist-tags@ targets absent from
the survivors are dropped. -}
restrictToSurvivors :: Set Text -> PackageInfo -> PackageInfo
restrictToSurvivors survivors info =
    info
        { infoVersions = Map.restrictKeys (infoVersions info) survivors
        , infoDistTags = Map.filter ((`Set.member` survivors) . renderVersion) (infoDistTags info)
        , infoPublishedAt = Map.restrictKeys (infoPublishedAt info) survivors
        }

{- Project each excluded version's 'Decision' to a 'ServeDecision' for the
no-survivors status. 'applyFilterPlan' carries the plan's decisions in
@versions@-key order ('Data.Map.elems'), so they zip back onto the same-ordered
'PackageDetails' to recover the package\/version each denial is about. -}
projectDecisions :: PackageInfo -> [Decision] -> [ServeDecision]
projectDecisions info =
    zipWith serveDecisionOf (Map.elems (infoVersions info))

-- ── decision-surface replay ───────────────────────────────────────────────────

-- The fully-edited served body: the raw @Value@ to encode and answer against the
-- conditional request.
newtype ServedBody = ServedBody {servedValue :: Value}

{- Assemble the served packument by merging the resolved sources and replaying the
'MergePlan' onto their raw @Value@s, or 'Nothing' when no version survives the
merge (no source resolved, or every public version was excluded and no private
versions exist).

The merge decides over the typed 'PackageInfo's; the served body is built from the
raw @Value@s so unmodeled keys survive. For each surviving @(version, SourceId)@
the version object is taken from that source's raw @Value@; @dist-tags@ and @time@
come from the plan (with @time@'s non-version bookkeeping keys retained from the
sources); every other top-level key is relayed from the precedence-winning
document. Tarball URLs are rewritten under the mount base so artifacts route back
through the gate. -}
assemble :: PackumentDeps -> [Contribution] -> Maybe ServedBody
assemble deps sources = do
    plan <- mergePackuments [(srcProvenance s, srcInfo s) | s <- sources]
    guard (not (Map.null (mpSurvivors plan)))
    let bySource :: Map SourceId Contribution
        bySource = Map.fromList (zip [0 ..] sources)
    pure (ServedBody (replayPlan deps bySource plan (baseDocument sources)))

{- The document whose unmodeled top-level keys are relayed into the served body:
the precedence-winning source's raw @Value@ — the first trusted source if any,
else the first source. (The merge takes its identity from the first input
likewise.) An empty source list never reaches here. -}
baseDocument :: [Contribution] -> Value
baseDocument sources =
    case find ((== TrustedSource) . srcProvenance) sources of
        Just s -> srcValue s
        Nothing -> case sources of
            s : _ -> srcValue s
            [] -> Object mempty

{- Replay a 'MergePlan' onto the raw source @Value@s: rebuild @versions@,
@dist-tags@, and @time@ from the plan, then rewrite the tarball URLs of the
result. Other top-level keys are inherited from the base document. -}
replayPlan :: PackumentDeps -> Map SourceId Contribution -> MergePlan -> Value -> Value
replayPlan deps bySource plan base =
    rewriteTarballUrls (pdMountBaseUrl deps) (Object rebuilt)
  where
    rebuilt :: KeyMap Value
    rebuilt =
        baseObject
            & KeyMap.insert "versions" (Object survivingVersions)
            & KeyMap.insert "dist-tags" (Object distTags)
            & KeyMap.insert "time" (Object reconciledTime)

    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    -- Each surviving version's object, taken from the raw @Value@ of the source
    -- that won the key (so the served bytes are the winning upstream's, unmodeled
    -- keys and all). A survivor whose source object is missing is dropped rather
    -- than fabricated — coherence with the plan is preserved by construction.
    survivingVersions :: KeyMap Value
    survivingVersions =
        KeyMap.fromList
            [ (Key.fromText version, object)
            | (version, sid) <- Map.toList (mpSurvivors plan)
            , Just object <- [versionObjectFrom sid version]
            ]

    -- Each source's raw @versions@ object, extracted once per source.
    -- 'versionObjectFrom' runs once per surviving version (up to the packument's
    -- version cap), so resolving the source's @versions@ object inside it would
    -- re-extract the same object on every version; hoisting it here leaves each
    -- survivor a single inner lookup. ('bySource' holds one entry per upstream.)
    versionsBySource :: Map SourceId (KeyMap Value)
    versionsBySource = Map.mapMaybe ((^? key "versions" . _Object) . srcValue) bySource

    versionObjectFrom :: SourceId -> Text -> Maybe Value
    versionObjectFrom sid version =
        Map.lookup sid versionsBySource >>= KeyMap.lookup (Key.fromText version)

    -- @dist-tags@ rebuilt from the plan's reconciled tags (each a rendered version
    -- string). The plan has already resolved @latest@ and dropped absent-target
    -- tags over the union.
    distTags :: KeyMap Value
    distTags =
        KeyMap.fromList
            [ (Key.fromText tag, String (renderVersion v))
            | (tag, v) <- Map.toList (mpDistTags plan)
            ]

    -- @time@ rebuilt from the plan's surviving-version times, with the sources'
    -- non-version bookkeeping keys (@created@\/@modified@) retained. The plan
    -- restricts @time@ to surviving versions; the bookkeeping keys are not versions
    -- and so are carried separately, from the base document's @time@.
    reconciledTime :: KeyMap Value
    reconciledTime =
        bookkeepingTime
            <> KeyMap.fromList
                [ (Key.fromText version, String (renderTime t))
                | (version, t) <- Map.toList (mpTime plan)
                ]

    -- The base @time@ map carries one entry per published version (up to the
    -- packument's version cap) plus the @created@\/@modified@ bookkeeping keys.
    -- Look those two keys up directly rather than filtering the whole map, so this
    -- is a pair of lookups, not a full traversal of every version's publish time.
    bookkeepingTime :: KeyMap Value
    bookkeepingTime =
        case base ^? key "time" . _Object of
            Nothing -> mempty
            Just timeObject ->
                KeyMap.fromList
                    [ (k, value)
                    | name <- timeBookkeepingKeys
                    , let k = Key.fromText name
                    , Just value <- [KeyMap.lookup k timeObject]
                    ]

-- The non-version keys an npm @time@ object carries that must be relayed unchanged.
timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

-- Render a publish time as the ISO-8601 instant npm serves in its @time@ map.
renderTime :: UTCTime -> Text
renderTime = toText . iso8601Show

-- ── status when nothing survives ──────────────────────────────────────────────

{- The per-version serve decisions weighed for the no-survivors status: the
public-set exclusions, plus the per-origin signals each upstream contributes.

A private upstream that did not resolve is a needed-but-unavailable transient signal
(it may resolve on retry), so a private outage with no public survivors is a @503@
rather than a @403@. An origin (private or public) that __answered with a packument
for a different package__ contributes an 'UpstreamInvalid' signal, so a request whose
only responding origins were invalid this way renders a @502@ — distinct from a
genuine absence. A public upstream that merely did not resolve degrades silently, as
before: its absence is not by itself a needed-upstream outage. -}
collectDecisions :: OriginResult -> OriginResult -> [ServeDecision] -> [ServeDecision]
collectDecisions privResult pubResult publicExclusions =
    privateDecision privResult <> publicMismatch pubResult <> publicExclusions
  where
    privateDecision :: OriginResult -> [ServeDecision]
    privateDecision = \case
        OriginResolved _ -> []
        OriginUnresolved -> [neededUpstreamUnavailable]
        OriginNameMismatch -> [upstreamInvalidDecision]

    publicMismatch :: OriginResult -> [ServeDecision]
    publicMismatch = \case
        OriginNameMismatch -> [upstreamInvalidDecision]
        OriginResolved _ -> []
        OriginUnresolved -> []

    neededUpstreamUnavailable :: ServeDecision
    neededUpstreamUnavailable = Reject (Rejection (Unavailable (WillResolve Nothing)) "a needed upstream was unavailable")

    upstreamInvalidDecision :: ServeDecision
    upstreamInvalidDecision = Reject (Rejection UpstreamInvalid "an upstream returned a packument for a different package")

-- ── response rendering ────────────────────────────────────────────────────────

{- Render the served packument body: @200@ with our own ETag over the served
bytes, or a @304@ when the client's conditional validator already matches. The
ETag is computed over exactly the bytes served, so it changes iff the served
document changes.

On a 'PackumentHead' the @200@ additionally carries the would-be body's
@Content-Length@ (the 'bodiless' wrapper then withholds the bytes), so the @HEAD@ reply
advertises the same framing a @GET@ would; a 'PackumentFull' leaves the @Content-Length@
to the serving layer, which frames the body it actually writes. The @304@ carries no
body either way, so it is identical between the two. -}
servePackumentBody :: PackumentServe -> Request -> ServedBody -> Response
servePackumentBody mode request body =
    case evaluateOwnETag (requestHeaders request) encoded of
        NotModified etag ->
            jsonResponse (mkStatus 304 "Not Modified") [etagHeader etag] ""
        Modified etag ->
            jsonResponse status200 (etagHeader etag : headContentLength mode encoded) encoded
  where
    encoded :: LByteString
    encoded = Aeson.encode (servedValue body)

{- The @Content-Length@ header a packument @200@ carries: on a 'PackumentHead', the
length of the would-be merged body, so the @HEAD@ reply advertises the framing a @GET@
would (the body itself is withheld by 'bodiless'); on a 'PackumentFull', none — the
serving layer frames the body it actually writes. -}
headContentLength :: PackumentServe -> LByteString -> ResponseHeaders
headContentLength = \case
    PackumentFull -> const []
    PackumentHead -> \bytes -> [(hContentLength, show (LBS.length bytes))]

{- Render the no-survivors outcome: the status 'packumentStatus' chose over the
exclusions, with a denial body collecting the reasons. Never a @404@ — the package
existed and its versions were withheld. -}
noSurvivors :: MountRenderer -> PackumentDeps -> [ServeDecision] -> Response
noSurvivors renderer deps decisions =
    renderedResponse (toStatus status) (retryAfterHeader status) (renderError renderer (pdHelp deps) message)
  where
    status :: PackumentStatus
    status = packumentStatus decisions

    toStatus :: PackumentStatus -> Status
    toStatus s = mkStatus (packumentStatusCode s) (statusReason s)

    statusReason :: PackumentStatus -> ByteString
    statusReason = \case
        PackumentOk -> "OK"
        PackumentForbidden -> "Forbidden"
        PackumentUnavailable{} -> "Service Unavailable"
        PackumentBadGateway -> "Bad Gateway"
        PackumentServerError -> "Internal Server Error"

    -- The collected denial reasons; an empty set (no versions at all) renders a
    -- deny-by-default message rather than an empty body.
    message :: Text
    message = case mapMaybe rejectionText decisions of
        [] -> "no versions are available for this package"
        reasons -> T.intercalate "; " reasons

    rejectionText :: ServeDecision -> Maybe Text
    rejectionText = \case
        Admit -> Nothing
        Reject rej -> Just (rejectionMessage rej)

-- The @Retry-After@ header for a transient no-survivors status, when a delay was
-- suggested; nothing for the other statuses.
retryAfterHeader :: PackumentStatus -> ResponseHeaders
retryAfterHeader = \case
    PackumentUnavailable (Just (RetryAfter secs)) -> [("Retry-After", show secs)]
    _ -> []

-- ── response helper ───────────────────────────────────────────────────────────

-- A JSON response with the given status, extra headers, and body. Used for the
-- served packument document itself, which is npm JSON.
jsonResponse :: Status -> ResponseHeaders -> LByteString -> Response
jsonResponse status extra =
    responseLBS status ((hContentType, "application/json") : extra)

-- A response built from a renderer's 'RenderedBody': its content type, then any
-- extra headers, then the rendered bytes.
renderedResponse :: Status -> ResponseHeaders -> RenderedBody -> Response
renderedResponse status extra (RenderedBody contentType body) =
    responseLBS status ((hContentType, contentType) : extra) body

-- ── the tarball handler ───────────────────────────────────────────────────────

{- | Serve a @GET \/{pkg}\/-\/{file}.tgz@ artifact request end to end, over the
request's 'RequestCtx'.

The mount's 'PackumentDeps' and error renderer are read from the matched
'MountBinding'; an unwired mount is the recognised-but-unserved @501@ stub (as for
'servePackument'). With dependencies wired and the edge token (if any) validated, the
two legs locate the tarball by the trust of their origin:

* the __private__ leg is a __conventional stable read__: it fetches
  @{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ by the requested filename
  ('artifactRequestByFile'), __forwarding the client's credential__ and __without a
  private-packument fetch__; a @2xx@ streams the bytes through with bounded memory and
  answers the request, any other status (or a connection failure) is a clean miss that
  falls through. It applies no serve-time integrity floor — the bytes are still verified
  client-side and by the mirror worker (see the module header → "Artifact path");
* on a private miss the __public__ leg fetches that one version's metadata anonymously
  and gates it against the rules; an admit honours the gated @dist.tarball@, streaming
  the public bytes __and enqueuing a 'MirrorJob'__ (serve-then-enqueue, the enqueue
  best-effort and non-blocking), a reject renders the serve error model
  (@403@\/@503@\/@500@\/@404@) through the mount's renderer.

The public-upstream fetch is always anonymous (the client credential is never sent to the
public upstream); the mirror job carries no credential. The serve path does not
verify @dist.integrity@ (see the module header → "Artifact path").
-}
serveTarball ::
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarball = tarballWith ServeFull

{- | Serve a @HEAD \/{pkg}\/-\/{file}.tgz@ artifact request end to end, over the
request's 'RequestCtx'.

A HEAD must __never__ run the full-@GET@ streaming pump: a bodiless HEAD would
otherwise open the upstream artifact connection and pump a whole artifact body that
the reply then discards — wasted upstream egress and a DoS-amplification lever (a
client forcing arbitrary full-artifact fetches with cheap HEADs). So this handler
gates the artifact through the __identical__ pipeline as 'serveTarball' — the same
edge auth, host-allowlist, internal-range, and tarball-host policy, and the same
upstream-request construction — but issues the upstream request as a HEAD and relays
its status and safe response headers ('relayArtifact') with __no body__
('Ecluse.Core.Server.Stream.probeUpstreamWhen'). On an admit no 'MirrorJob' is enqueued: a
HEAD serves no bytes, so there is nothing to back-fill (mirroring stays demand-driven
on the GET path). A refusal renders the same serve error model with an empty body.
-}
headTarball ::
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
headTarball name version filename request respond =
    -- A HEAD reply carries no body, by HTTP semantics: every branch — the bodiless
    -- upstream probe, an edge 401, a policy 403/404/503, an internal 500 — answers
    -- through 'bodiless', which keeps each branch's status and headers but strips the
    -- body. (The 'ServeHead' upstream probe is what keeps the artifact body from being
    -- fetched at all; this strips the body of the locally-rendered branches too.)
    tarballWith ServeHead name version filename request (respond . bodiless)

-- Strip a response's body while keeping its status and headers — the bodiless form a
-- HEAD reply takes on every branch (HTTP semantics: a HEAD carries no message body).
-- The headers a GET would carry (notably any relayed @Content-Length@) are preserved.
bodiless :: Response -> Response
bodiless response = responseLBS (responseStatus response) (responseHeaders response) ""

-- The artifact serve mode: a full GET that streams the body through, or a HEAD that
-- probes the upstream bodiless and relays only the headers. Threaded through the
-- artifact path so the gating and upstream-request construction are shared verbatim
-- between the two, differing only in the upstream method, whether a body is pumped,
-- and whether an admit enqueues a mirror job.
data ArtifactServe
    = -- A GET: stream the artifact body through, enqueuing a mirror job on a public
      -- admit (the demand-driven back-fill).
      ServeFull
    | -- A HEAD: probe the upstream as a HEAD and relay the headers with no body,
      -- enqueuing nothing (no bytes are served, so there is nothing to mirror).
      ServeHead

-- The dispatch shared by 'serveTarball' and 'headTarball': resolve the mount's
-- dependencies (or the recognised-but-unserved @501@ stub) and serve in the given mode.
tarballWith ::
    ArtifactServe ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
tarballWith mode name version filename request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPackumentDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (recognisedButUnserved renderer))
        Just deps -> serveTarballWithDeps mode renderer deps name version filename request respond

-- Serve a tarball once the mount's dependencies are known: edge auth, then the
-- private-hit / public-miss fetches the module header describes. The request runtime
-- is read from the request context. The 'ArtifactServe' mode is threaded into
-- both legs so a HEAD takes the identical gating as a GET, probing bodiless.
serveTarballWithDeps ::
    ArtifactServe ->
    MountRenderer ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarballWithDeps mode renderer deps name version (Filename file) request respond
    | not (edgeAuthorised deps request) = liftIO (respond (edgeUnauthorised renderer))
    | otherwise = do
        rt <- asks ctxRuntime
        let clientToken = forwardedToken request
            -- The client's conditional validators, relayed onto the upstream
            -- artifact request on both legs so upstream can answer a 304 for a
            -- pass-through body we serve unchanged (the conditional-GET contract).
            validators = forwardValidators (requestHeaders request)
        privateHit <- streamPrivateArtifact mode rt deps clientToken validators name file respond
        case privateHit of
            Just received -> do
                -- A private hit is an admit served from the trusted upstream (no rule
                -- gate runs); a private miss falls through to the gated public path,
                -- which records its own decision.
                liftIO (mpServeDecision (srMetrics rt) Metric.Admit)
                pure received
            Nothing -> servePublicArtifact mode rt renderer deps validators name version file respond

{- Stream the artifact from the __trusted__ private upstream as a __conventional stable
read__: build the tarball request at @{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ by the
client's requested filename ('artifactRequestByFile') and fetch it directly, __without
fetching the private packument first__. This is the stable, cacheable shape an @npm ci@
install issues; a worst-case lockfile fan-out therefore pays one artifact round-trip per
tarball rather than an uncached packument fetch+decode it would only discard.

The request __forwards the client's credential__ over the trusted manager, attached at
the single bearer-attach point ('Ecluse.Core.Registry.Npm.withToken'), which pins
@redirectCount = 0@: the credential-bearing read __never follows a redirect__ (a private
CDN @302@ is returned here, not chased with the bearer). The constructed URL is on the
private base host, so the 'Ecluse.Core.Security.TrustedOrigin' tarball-host gate is
satisfied __same-host__ — the host check is still applied, simply trivially met — and the
trusted origin is __exempt from the internal-range block__ ('srPrivateManager', the
serve-path mirror of that unguarded manager, security.md invariant 3): a private registry
on an internal address (e.g. @http:\/\/10.0.0.5\/@) still serves its same-host tarball.

A @2xx@ is streamed through with bounded memory and yields 'Just' (the request is
answered); a non-@2xx@ status, an unformable URL, or a failure opening the connection
yields 'Nothing' so the caller falls through to the public origin, the upstream artifact
body never read. The client's conditional @validators@ are relayed onto the request
('forwardValidators' filtered them upstream), and the relay accepts an upstream @304 Not
Modified@ ('acceptArtifact') as well as a @2xx@: a private tarball is a pass-through body,
so a @304@ is relayed straight back to the client (bodiless) rather than treated as a
private miss falling through to the public origin.

A failure that strikes __after__ a @2xx@ has begun streaming is unrecoverable — the
response is already on the wire — so 'streamUpstreamWhen' lets it propagate rather than
reporting a miss: the request fails internally (the connection is torn down) instead of
responding a second time over a half-sent artifact.

This leg applies __no serve-time integrity floor__: an established version pinned in a
consumer's lockfile and served from an operator-trusted private registry is fast-tracked,
its bytes still verified client-side by @npm@ and by the mirror worker on ingestion. A
private upstream that serves its tarball off the conventional @\/-\/@ path (a separate
files host, a signed CDN URL) is not reached by this leg and is a clean miss that falls
through to the public origin. -}
streamPrivateArtifact ::
    ArtifactServe ->
    ServeRuntime ->
    PackumentDeps ->
    Maybe Secret ->
    RequestHeaders ->
    PackageName ->
    Text ->
    (Response -> IO ResponseReceived) ->
    Handler (Maybe ResponseReceived)
streamPrivateArtifact mode rt deps token validators name file respond =
    case privateRequest of
        Just req -> liftIO (relayUpstreamWhen mode (srPrivateManager rt) req acceptArtifact relayArtifact respond)
        Nothing -> pure Nothing
  where
    -- Build the conventional-URL private tarball request {base}/{pkg}/-/{file} by the
    -- requested filename, when its (same-)host passes the tarball-host policy and the URL
    -- forms. 'Nothing' on either refusal — a private miss the caller falls through on. The
    -- constructed URL is on the private base host, so the host gate is trivially
    -- satisfied; it is kept applied rather than dropped. The request is marked with the
    -- serve mode's method (GET / HEAD) and carries the client's relayed conditional
    -- validators; 'artifactRequestByFile' attaches the forwarded credential with
    -- redirectCount = 0 (the credential-redirect invariant).
    privateRequest :: Maybe HTTP.Request
    privateRequest =
        if tarballHostHonoured TrustedOrigin deps (pdPrivateBaseUrl deps) (pdPrivateBaseUrl deps)
            then withValidators validators . withMethod mode <$> rightToMaybe (artifactRequestByFile (clientConfig (pdLimits deps) (srPrivateManager rt) (pdPrivateBaseUrl deps) token) name file)
            else Nothing

{- Serve the artifact from the public upstream after a private miss: gate the
single requested version against the rules, and on an admit stream the public bytes
(anonymously) and enqueue a mirror job; on a reject render the serve error model.
The public version metadata is fetched anonymously to decide. -}
servePublicArtifact ::
    ArtifactServe ->
    ServeRuntime ->
    MountRenderer ->
    PackumentDeps ->
    RequestHeaders ->
    PackageName ->
    Version ->
    Text ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublicArtifact mode rt renderer deps validators name version file respond = do
    let metrics = srMetrics rt
    gated <- gatePublicVersion rt deps name version file
    case gated of
        Admitted artifact -> do
            liftIO (mpServeDecision metrics Metric.Admit)
            liftIO (streamPublicArtifact mode rt renderer deps validators name version artifact respond)
        Refused decision -> liftIO $ do
            mpServeDecision metrics (serveDecisionClass decision)
            recordDenials metrics [decision]
            respond (artifactError renderer deps (artifactStatus decision) decision)

{- The outcome of gating a single requested artifact on the public path: either the
chosen 'Artifact' to fetch, or the serve decision the error model renders. The
admit carries the artifact so the stream step honours its 'artUrl' rather than
re-deciding or reconstructing the location. -}
data PublicArtifactGate
    = -- | The version was admitted; carries the artifact selected by filename.
      Admitted Artifact
    | -- | The version was refused (policy denial, upstream outage, or absence).
      Refused ServeDecision

{- Gate the single requested version against the rules engine and select its
artifact, returning the gate outcome. The public packument is fetched anonymously
and parsed; the requested version's 'PackageDetails' is evaluated through
'Ecluse.Core.Rules.evalRules' (the same engine the packument path gates with). On an admit the
artifact matching the requested filename is selected ('artifactFor'); a filename
absent from an otherwise-admitted version is a forwarded miss, the same @404@ as an
absent version.

The refusal causes the error model maps: a version (or file) absent from the public
metadata is a genuine miss (a @404@ forwarded absence, projected as 'Unavailable'
'WontResolve' only to carry a non-admit — the status is overridden to @404@ in
'artifactError'); a metadata fetch that fails is a transient upstream outage (@503@);
a present version is decided by the rules, where a needed effectful rule that cannot
be consulted fail-closes to an 'Unavailable' @503@\/@500@. -}
gatePublicVersion :: ServeRuntime -> PackumentDeps -> PackageName -> Version -> Text -> Handler PublicArtifactGate
gatePublicVersion rt deps name version file = do
    evalCtx <- liftIO (EvalContext <$> pdNow deps)
    resolved <- originPackument <$> fetchPublicOrigin (pdLimits deps) rt (pdPublicBaseUrl deps) name
    case resolved of
        Nothing -> pure (Refused upstreamUnavailable)
        Just (info, _value) -> case Map.lookup (renderVersion version) (infoVersions info) of
            Nothing -> pure (Refused versionAbsent)
            Just details ->
                -- The rule-eval domain span wraps the actual decision (only reached once
                -- the version exists), recording the verdict so a denial → 403 is
                -- explainable from the trace; the upstream-outage and version-absent
                -- branches above are not rule evaluations and carry no span.
                liftIO $
                    spanRuleEval (srTracing rt) name version $ do
                        (gate, seconds) <- timedSeconds (gateVersion evalCtx deps file details)
                        mpRuleEvalDuration (srMetrics rt) (evalTier (pdRules deps)) seconds
                        pure (gate, gateVerdict gate)

-- The serve verdict a public-artifact gate outcome carries, for the rule-eval span:
-- an admitted version admits; a refused one carries the decision the serve error
-- model renders.
gateVerdict :: PublicArtifactGate -> ServeDecision
gateVerdict = \case
    Admitted _ -> Admit
    Refused decision -> decision

{- Project a single version's rule decision to a gate outcome, selecting the
artifact by filename on an admit. A denied version is 'Refused' with its decision; an
admitted version whose requested filename matches no artifact is a forwarded miss
('versionAbsent', rendered @404@).

The __integrity-floor admission policy__ is enforced here, after the rules admit and
the artifact is selected: a public version whose selected artifact carries no digest
meeting the floor ('pdMinIntegrity') is inadmissible — 'integrityMissing' (no digest at
all) or 'integrityBelowFloor' (a digest, but too weak), both rendered @403@ — and
refused outright, never fetched. This is the public path; the trusted (private) artifact
serve is a conventional stable read in 'streamPrivateArtifact' that applies no serve-time
integrity floor, so it never reaches this gate. -}
gateVersion :: EvalContext -> PackumentDeps -> Text -> PackageDetails -> IO PublicArtifactGate
gateVersion ctx deps file details = do
    decision <- serveDecisionOf details <$> evalRules ctx (pdRules deps) details
    pure $ case decision of
        Admit -> maybe (Refused versionAbsent) admitWithIntegrity (artifactFor file details)
        Reject _ -> Refused decision
  where
    -- A rule-admitted artifact is served only if it carries a digest meeting the floor;
    -- a weaker-than-floor or hashless one is refused by the integrity-floor policy.
    admitWithIntegrity :: Artifact -> PublicArtifactGate
    admitWithIntegrity artifact = case classifyArtifacts (pdMinIntegrity deps) (artifact :| []) of
        MeetsFloor -> Admitted artifact
        BelowFloor -> Refused integrityBelowFloor
        NoIntegrity -> Refused integrityMissing

-- A transient public-upstream outage: a 'WillResolve' rejection (→ @503@).
upstreamUnavailable :: ServeDecision
upstreamUnavailable =
    Reject (Rejection (Unavailable (WillResolve Nothing)) "the upstream registry was unavailable")

{- A version not present in the public metadata: a non-admit carrying a
'WontResolve' cause, whose status 'artifactError' overrides to a @404@ forwarded
miss (the package may exist, this version does not). -}
versionAbsent :: ServeDecision
versionAbsent =
    Reject (Rejection (Unavailable WontResolve) "the requested version was not found upstream")

{- A __public__ version refused by the integrity-presence admission policy: its selected
artifact carries no integrity digest of any kind, so it cannot be tied to a
tamper-evident fingerprint. A deliberate deny-by-default policy refusal ('MissingIntegrity',
rendered @403@), not a rule denial and not a retryable outage. The trusted (private) path
uses 'trustedIntegrityMissing' instead, worded for its own context. -}
integrityMissing :: ServeDecision
integrityMissing =
    Reject (Rejection MissingIntegrity "this version carries no integrity digest and cannot be served from a public upstream")

{- A __public__ version refused by the integrity-floor admission policy: its selected
artifact carries an integrity digest, but the strongest one is weaker than the configured
minimum algorithm, so its bytes cannot be tied to a collision-resistant fingerprint. A
deliberate deny-by-default policy refusal ('BelowIntegrityFloor', rendered @403@),
distinct from 'integrityMissing' so the audit trail says which. The trusted (private) path
uses 'trustedIntegrityBelowFloor' instead. -}
integrityBelowFloor :: ServeDecision
integrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this version's integrity digest is weaker than the configured minimum and cannot be served from a public upstream")

{- A __trusted__ (private) version dropped by the trusted integrity floor for carrying no
integrity digest at all. The same 'MissingIntegrity' @403@ as the public refusal, but
worded for the private path; it surfaces only in the no-survivors body when no version
(private or public) is admissible. -}
trustedIntegrityMissing :: ServeDecision
trustedIntegrityMissing =
    Reject (Rejection MissingIntegrity "this private version carries no integrity digest and was not served")

{- A __trusted__ (private) version dropped by the trusted integrity floor: its strongest
digest is weaker than the configured trusted minimum (which an operator may loosen below
SHA-256). The same 'BelowIntegrityFloor' @403@ as the public refusal, worded for the
private path. -}
trustedIntegrityBelowFloor :: ServeDecision
trustedIntegrityBelowFloor =
    Reject (Rejection BelowIntegrityFloor "this private version's integrity digest is weaker than the configured trusted minimum and was not served")

{- Stream the artifact from the public upstream at its __authoritative location__,
__anonymously__ (the client credential is never sent to the public upstream), and —
__after__ the response is begun — enqueue a best-effort mirror job. The chosen
'Artifact''s 'artUrl' is honoured directly rather than reconstructed; the
tarball-host policy gates whether that location may be fetched (the public packument
host is the reference), and the resolved-IP recheck on 'srPublicManager' is the
defence-in-depth backstop. A host the policy refuses is the @403@ policy-denial path;
an unformable URL is the internal-error path.

The fetch keeps the open phase distinct from the committed stream, the same split the
private origin uses: opening the connection is the recoverable phase, so a transient
network failure or a connection-time 'Ecluse.Core.Security.BlockedTarget' (the resolved-IP
recheck refusing an allowlisted name that resolves internal) yields no committed
response and is rendered as the transient upstream-unavailable @503@ through the
mount's renderer — not left to escape as a bare @500@. Any upstream status is relayed
verbatim (the @accept@ predicate is total); only a failure __after__ the stream is
committed propagates, the connection torn down as it unwinds, so a half-sent artifact
is never followed by a second response. The mirror enqueue runs only on the committed
path, after the response is begun.

The client's conditional @validators@ are relayed onto the upstream artifact request
('forwardValidators' filtered them); the public artifact is a pass-through body, so an
upstream @304 Not Modified@ is relayed straight back to the client (bodiless, via
'streamUpstreamWhen'), the bytes never re-downloaded. The validators carry no
credential and the public fetch stays anonymous. -}
streamPublicArtifact ::
    ArtifactServe ->
    ServeRuntime ->
    MountRenderer ->
    PackumentDeps ->
    RequestHeaders ->
    PackageName ->
    Version ->
    Artifact ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
streamPublicArtifact mode rt renderer deps validators name version artifact respond =
    if tarballHostHonoured UntrustedOrigin deps (pdPublicBaseUrl deps) (artUrl artifact)
        then case withValidators validators . withMethod mode <$> artifactRequestByUrl (clientConfig (pdLimits deps) (srPublicManager rt) (pdPublicBaseUrl deps) Nothing) (artUrl artifact) of
            Right req ->
                relayUpstreamWhen mode (srPublicManager rt) req (const True) relayArtifact respond >>= \case
                    Just received -> do
                        -- Mirroring is demand-driven on the GET path only: a HEAD serves
                        -- no bytes, so there is nothing to back-fill.
                        enqueueOnFull mode (enqueueMirror rt deps name version artifact)
                        pure received
                    Nothing -> respond (artifactError renderer deps (artifactStatus upstreamUnavailable) upstreamUnavailable)
            Left _ -> respond internalArtifactError
        else respond crossHostRefused

-- ── the serve mode's three differences ─────────────────────────────────────────

{- Tag an upstream artifact request with the serve mode's method: a 'ServeFull' fetch
keeps the request's default @GET@, a 'ServeHead' probe is marked @HEAD@ so the upstream
sees a bodiless request and the proxy never pumps the body. -}
withMethod :: ArtifactServe -> HTTP.Request -> HTTP.Request
withMethod = \case
    ServeFull -> id
    ServeHead -> \req -> req{HTTP.method = methodHead}

{- Relay the client's conditional validators (the @If-None-Match@ \/ @If-Modified-Since@
'forwardValidators' filtered) onto an upstream artifact request, so upstream can answer
a @304 Not Modified@ for a pass-through body we serve unchanged. An empty validator set
(the client sent none) leaves the request unconditional. -}
withValidators :: RequestHeaders -> HTTP.Request -> HTTP.Request
withValidators validators req =
    req{HTTP.requestHeaders = validators <> HTTP.requestHeaders req}

{- The upstream artifact statuses the private relay accepts back to the client: a
@2xx@ success (the streamed artifact) or a @304 Not Modified@ (the pass-through
conditional-GET relay — the client's relayed validators matched upstream's, so the
unchanged artifact is answered as a bodiless @304@ by 'streamUpstreamWhen' rather than
re-downloaded). Any other status is a clean private miss the caller falls through on.
(The public relay accepts every status — it relays whatever the public origin returns
verbatim — so it needs no predicate of its own.) -}
acceptArtifact :: Status -> Bool
acceptArtifact s = statusIsSuccessful s || isNotModified s

{- Relay an upstream artifact response in the serve mode: 'ServeFull' streams the body
through with bounded memory ('streamUpstreamWhen'); 'ServeHead' probes bodiless,
relaying the status and headers with no body ('probeUpstreamWhen'). Both keep the same
recoverable-miss / committed split, so a HEAD falls through a private miss to the public
origin exactly as a GET does. -}
relayUpstreamWhen ::
    ArtifactServe ->
    Manager ->
    HTTP.Request ->
    (Status -> Bool) ->
    (Status -> ResponseHeaders -> (Status, ResponseHeaders)) ->
    (Response -> IO ResponseReceived) ->
    IO (Maybe ResponseReceived)
relayUpstreamWhen = \case
    ServeFull -> streamUpstreamWhen
    ServeHead -> probeUpstreamWhen

-- Run the demand-driven mirror enqueue only on the 'ServeFull' (GET) path; a
-- 'ServeHead' served no bytes, so it back-fills nothing.
enqueueOnFull :: ArtifactServe -> IO () -> IO ()
enqueueOnFull mode act = case mode of
    ServeFull -> act
    ServeHead -> pass

{- Enqueue a demand-driven mirror job for an admitted artifact, __best-effort__: it
runs after the client response is begun and any failure is swallowed, so a queue
outage never fails or delays the serve. The job names the artifact's authoritative
URL (the same location the public fetch targeted) and the mount's mirror target; it
carries no credential (the worker mints its own).

It also captures the __serve-time-admitted__ integrity digests, filename, and
declared size on the job, so the worker verifies the fetched bytes against exactly
what the rules cleared (immune to an upstream packument mutated in the
enqueue → process window) and can assemble the publish document without re-fetching.
The artifact reached this point through the integrity-presence admission policy, so
'artHashes' is non-empty; a hashless artifact (which that policy already refuses to
serve) is not enqueued, since there would be no digest to verify against. -}
enqueueMirror :: ServeRuntime -> PackumentDeps -> PackageName -> Version -> Artifact -> IO ()
enqueueMirror rt deps name version artifact =
    whenJust (nonEmpty (artHashes artifact)) $ \hashes ->
        void . spanMirrorEnqueue (srTracing rt) name version (artUrl artifact) enqueueErrorDetail $ \traceContext -> do
            enqueued <-
                tryAny . enqueue (srQueue rt) $
                    MirrorJob
                        { jobPackage = name
                        , jobVersion = version
                        , jobArtifactUrl = artUrl artifact
                        , jobMirrorTarget = pdMirrorTarget deps
                        , jobArtifact =
                            MirrorArtifact
                                { maFilename = artFilename artifact
                                , maHashes = hashes
                                , maSize = artSize artifact
                                }
                        , -- The enqueueing span's trace context, captured by the span
                          -- bracket, so the worker's per-job span links back across the hop.
                          jobTraceContext = traceContext
                        }
            -- Best-effort: the enqueue outcome is counted but never propagated, so a
            -- queue outage records a failure rather than failing or delaying the serve.
            either (const (mpMirrorEnqueueFailure (srMetrics rt))) (const (mpMirrorEnqueued (srMetrics rt))) enqueued
            -- Hand the outcome back so the span bracket can mark a swallowed failure
            -- errored on the producer span (the metric counts it; the span explains it).
            pure enqueued
  where
    -- Project the swallowed enqueue outcome onto the producer span's status: a failure
    -- records the cause (so a trace explains why the mirror was not enqueued), a success
    -- leaves the status unset.
    enqueueErrorDetail :: Either SomeException () -> Maybe Text
    enqueueErrorDetail = either (Just . enqueueFailureDetail) (const Nothing)

    enqueueFailureDetail :: SomeException -> Text
    enqueueFailureDetail e = "mirror enqueue failed: " <> toText (displayException e)

-- ── the egress gate at the serve boundary ─────────────────────────────────────────

{- Whether an artifact's authoritative @url@ may be fetched, given the origin's trust,
the mount's tarball-host policy, and the host that served the packument it came from.
Connects the pure 'tarballHostAllowed' at the serve boundary: the @url@'s host must be on
the upstream allowlist and — under the secure-default
'Ecluse.Core.Security.SameHostAsPackument' — equal to the packument host; the opt-in
'Ecluse.Core.Security.AnyAllowlistedHost' relaxes that last clause to any allowlisted host.
This is the policy half of the @dist.tarball@ defence; for an
'Ecluse.Core.Security.UntrustedOrigin' the resolved-IP recheck on the guarded manager is
its connection-time backstop (an allowlisted name that resolves to an internal address
is still refused there).

The internal-range block is __origin-aware__, mirroring the connection layer's trusted
vs guarded manager split: an 'Ecluse.Core.Security.UntrustedOrigin' (the public path) is
gated against it (subject to the empty opt-in here, the composition root's secure
default), while an 'Ecluse.Core.Security.TrustedOrigin' (the operator-configured private
upstream) is exempt — a private registry may legitimately live on an internal address,
just as its connections use the unguarded 'Ecluse.Core.Security.Egress.newTrustedTlsManager'
(security.md invariant 3). The allowlist and same-host clauses still gate the trusted
origin identically. -}
tarballHostHonoured :: Origin -> PackumentDeps -> Text -> Text -> Bool
tarballHostHonoured origin deps packumentBaseUrl artifactUrl =
    tarballHostAllowed
        origin
        (pdTarballHostPolicy deps)
        (upstreamAllowlist deps)
        (pdAllowedInternalHosts deps)
        (hostAddress packumentBaseUrl)
        (hostAddress artifactUrl)

{- The host allowlist on the serve path: the bare hosts of the mount's configured
upstreams — the public and private upstream base URLs and the mirror target. These
are exactly the hosts the proxy is configured to talk to, so an artifact @url@ on any
other host is off the allowlist regardless of policy (security.md invariant 2: a
@dist.tarball@ host off the configured upstreams is refused). -}
upstreamAllowlist :: PackumentDeps -> LoweredHostSet
upstreamAllowlist deps =
    lowerCaseHosts . fromList $
        map hostAddress [pdPublicBaseUrl deps, pdPrivateBaseUrl deps, pdMirrorTarget deps]

{- Select the artifact a request's filename names from a version's distribution
files. npm has exactly one artifact per version, so the match is the single file; a
many-per-version ecosystem (PyPI) would select the wheel\/sdist whose filename the
client requested. 'Nothing' when no artifact carries the requested filename — a
forwarded miss, never a fabricated location. -}
artifactFor :: Text -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== file) . artFilename) (pkgArtifacts details)

{- A @403@ for an artifact whose authoritative @url@ the tarball-host policy refuses:
a cross-host @dist.tarball@ under the secure-default 'Ecluse.Core.Security.SameHostAsPackument',
or a host off the upstream allowlist. A policy denial, not a serve outcome the rules
produced — the same @403@ surface a rule denial renders, with a fixed reason. -}
crossHostRefused :: Response
crossHostRefused =
    responseLBS (mkStatus 403 "Forbidden") [(hContentType, "application/json")] "{\"error\":\"the upstream artifact host is not permitted by the tarball-host policy\"}"

{- The relay for an artifact stream: forward the upstream status and headers,
dropping only the hop-by-hop framing headers (@Transfer-Encoding@, @Connection@)
whose values describe the upstream hop, not the artifact. The body is opaque binary
streamed verbatim, so the content headers (type, length, encoding) and the
upstream's @ETag@ pass through unchanged — the client verifies the artifact's own
@dist.integrity@ over exactly these bytes. -}
relayArtifact :: Status -> ResponseHeaders -> (Status, ResponseHeaders)
relayArtifact status headers =
    (status, filter (not . isHopByHop . fst) headers)
  where
    isHopByHop name = name == "Transfer-Encoding" || name == "Connection"

{- Render a non-admit artifact outcome as the serve error model: @403@ for a policy
denial, @503@ for a transient upstream unavailability, @404@ for a forwarded
upstream miss (the requested version is absent), @500@ otherwise. The body is shaped
by the mount's renderer; a transient status carries no suggested delay here (the
single-artifact path has none to offer). A @404@ is the version-absent miss, which
'gatePublicVersion' flags as a 'WontResolve' rejection — the only such cause on this
path — so it is mapped to @404@ rather than the @500@ a 'WontResolve' would
otherwise render. -}
artifactError :: MountRenderer -> PackumentDeps -> ArtifactStatus -> ServeDecision -> Response
artifactError renderer deps status decision =
    renderedResponse (toStatus actualStatus) [] (renderError renderer (pdHelp deps) message)
  where
    -- The version-absent miss is carried as a 'WontResolve' rejection but rendered
    -- as a forwarded @404@, not the @500@ a generic 'WontResolve' maps to.
    actualStatus :: ArtifactStatus
    actualStatus = if isVersionAbsent then NotFound else status

    isVersionAbsent :: Bool
    isVersionAbsent = case decision of
        Reject (Rejection (Unavailable WontResolve) _) -> True
        _ -> False

    toStatus :: ArtifactStatus -> Status
    toStatus s = mkStatus (artifactStatusCode s) (statusReason s)

    statusReason :: ArtifactStatus -> ByteString
    statusReason = \case
        Ok -> "OK"
        Forbidden -> "Forbidden"
        Unavailable'{} -> "Service Unavailable"
        ServerError -> "Internal Server Error"
        NotFound -> "Not Found"

    message :: Text
    message = case decision of
        Admit -> "the artifact is available"
        Reject rej -> rejectionMessage rej

{- A @500@ for an unformable upstream artifact URL — a configuration fault, not a
serve decision. The package segment and filename are already known-safe, so this is
reachable only on a misconfigured base URL; it is the internal-error tier, distinct
from the rule\/upstream outcomes 'artifactError' renders. -}
internalArtifactError :: Response
internalArtifactError =
    responseLBS (mkStatus 500 "Internal Server Error") [(hContentType, "application/json")] "{\"error\":\"could not form the upstream artifact URL\"}"

-- ── metric emits ───────────────────────────────────────────────────────────────

{- Record an upstream metadata fetch around the real fetch action: its latency on a
successful resolve (a 2xx body was read and decoded), or a bounded-cause error
otherwise, before re-raising so the caller's degrade is unchanged. Wrapping the fetch
action means the public path — fetched through the cache — records only on a miss (a
hit never runs the action), so the histogram counts real upstream calls, not cache
hits (those are the metadata-cache metric's concern). -}
recordedFetch :: MetricsPort -> Metric.Upstream -> Handler CacheEntry -> Handler CacheEntry
recordedFetch metrics upstream action = do
    (result, seconds) <- timedSeconds (tryAny action)
    case result of
        Right entry -> do
            liftIO (mpUpstreamFetch metrics upstream Metric.Status2xx seconds)
            pure entry
        Left err -> do
            liftIO (mpUpstreamFetchError metrics upstream (fetchCause err))
            throwIO err

-- ── the first-party publish handler ───────────────────────────────────────────

{- | Serve a @PUT \/{pkg}@ first-party publish request end to end, over the request's
'RequestCtx'.

The mount's 'PublishDeps' and error renderer are read from the matched 'MountBinding'
in context. The path is __opt-in__: a mount with no publish dependencies wired
('bindingPublishDeps' is 'Nothing') has no publication target, so a publish is
@405 Method Not Allowed@ — there is no implicit write path.

With a publication target configured, the order is load-bearing — every refusal happens
__before any upstream write is attempted__:

1. the edge token (if configured) is validated, exactly as the read paths gate
   ('edgeTokenAuthorised'); a missing\/mismatched token is a @401@;
2. the __anti-shadowing scope guard__ ('inPublishScope') is enforced — a name outside
   the configured @PUBLISH_SCOPES@ allow-list is a @403@ with a clear message, so a
   client cannot publish a name that shadows an existing public package
   (dependency confusion);
3. the body is read and its __declared identity is validated__
   ('bodyNameDisagreement'): the publish document carries its own @_id@, top-level
   @name@, and per-version @name@s, so any present declared name that disagrees with the
   URL-path name is a @403@ — holding the __guard-name ≡ write-name ≡ body-name__
   invariant, so a crafted body cannot write a name the scope guard never authorised;
4. only then is the body relayed to the publication target ('relayPublishDocument'),
   with the publisher's __own forwarded credential__ (passthrough; the static
   'pubStaticToken' is the fallback for a client that sends none) — never Écluse's own
   token, and never to the public upstream.

The publication target's own status and body are relayed back to the @npm@ client
verbatim, so the publisher sees exactly what the registry said. A target that cannot be
reached (a transport failure) is a @502@; an unformable target URL (misconfiguration) a
@500@. The package URL and the scope guard both key on the __route's__ 'PackageName', and
the body's declared name is validated to __agree__ with it, never substituted for it (see
@docs\/architecture\/registry-model.md@ → "Publishing first-party packages").
-}
servePublish ::
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublish name request respond = do
    renderer <- asks (bindingRenderer . ctxMount)
    asks (bindingPublishDeps . ctxMount) >>= \case
        Nothing -> liftIO (respond (publishDisabled renderer))
        Just deps -> publishWithDeps renderer deps name request respond

-- Serve a publish once the mount's publication target is known: the edge gate, the
-- anti-shadowing scope guard, then the body-name agreement check (all before any write),
-- then the relay to the publication target with the publisher's forwarded credential.
publishWithDeps ::
    MountRenderer ->
    PublishDeps ->
    PackageName ->
    Request ->
    (Response -> IO ResponseReceived) ->
    Handler ResponseReceived
publishWithDeps renderer deps name request respond
    | not (edgeTokenAuthorised (pubInboundToken deps) request) =
        liftIO (respond (edgeUnauthorised renderer))
    | not (inPublishScope (pubScopes deps) name) =
        liftIO (respond (outOfScope renderer deps name))
    | otherwise = do
        rt <- asks ctxRuntime
        -- The body is bounded by the client→proxy request-size cap (the size-limit
        -- middleware), read here only after the scope guard has admitted the name, so a
        -- refused publish never even buffers its (potentially large, base64-tarball)
        -- body.
        body <- liftIO (consumeRequestBodyStrict request)
        -- The body-name agreement leg of the anti-shadowing guard (issue #391): the scope
        -- guard authorised the URL-path name, but the publish document carries its own
        -- declared identity, so a crafted body could otherwise write a name the guard never
        -- saw. Refuse — before the relay — any present declared name that disagrees with the
        -- URL-path name, so the identity authorised is provably the identity written.
        case bodyNameDisagreement name body of
            Just declared -> liftIO (respond (bodyNameMismatch renderer deps name declared))
            -- @consumeRequestBodyStrict@ reads the whole body but returns it lazy; the
            -- publish builder ('relayPublishDocument') puts it on the wire as a strict
            -- @RequestBodyBS@, so materialise it strict here. The body is already bounded by
            -- the client→proxy request-size cap.
            Nothing -> do
                outcome <- tryAny (liftIO (relayPublishDocument (publishConfig rt deps request) name (LBS.toStrict body)))
                liftIO (respond (renderRelay renderer deps outcome))

{- The per-request npm client config for the publish relay: the publication target as
its base URL, the trusted private manager (the target is operator-configured, like the
private upstream, so it is reached over the trusted path without the untrusted-egress
resolved-IP recheck), the response-bound budget, and the __forwarded__ publisher token
— the client's own ('forwardedToken'), falling back to the static
'pubStaticToken' only when the client sends none (the passthrough credential model). -}
publishConfig :: ServeRuntime -> PublishDeps -> Request -> NpmClientConfig
publishConfig rt deps request =
    NpmClientConfig
        { npmBaseUrl = pubTargetUrl deps
        , npmManager = srPrivateManager rt
        , npmToken = forwardedToken request <|> pubStaticToken deps
        , npmLimits = pubLimits deps
        }

{- Whether a package name falls within the configured publish-scope allow-list — the
anti-shadowing guard. A __scoped__ name is admitted iff its scope is one of the
configured scopes; an __unscoped__ name is never in any scope, so it is refused (the
MVP allow-list is scope-based, e.g. @\@acme@). The scope equality is exact, so
@\@acme-evil@ does not match an @\@acme@ allow-list entry. -}
inPublishScope :: [Scope] -> PackageName -> Bool
inPublishScope scopes name = case pkgNamespace name of
    Just scope -> scope `elem` scopes
    Nothing -> False

{- Render the relay outcome: the publication target's own status and body forwarded to
the client on success (so the publisher sees the registry's real answer — a success
shape, a @409@, a @403@ the registry's own authorisation produced); a @502@ when the
target could not be reached; a @500@ when its URL is unformable (misconfiguration). -}
renderRelay ::
    MountRenderer ->
    PublishDeps ->
    Either SomeException (Either UrlFormationError PublishRelayResponse) ->
    Response
renderRelay renderer deps = \case
    Right (Right (PublishRelayResponse code relayed)) ->
        jsonResponse (mkStatus code "") [] relayed
    Right (Left _urlErr) ->
        renderedResponse status500 [] (renderError renderer (pubHelp deps) "the publication target URL is misconfigured")
    Left _exc ->
        renderedResponse status502 [] (renderError renderer (pubHelp deps) "the publication target could not be reached")

-- A @405@ for a publish on a mount with no publication target configured: the
-- opt-in path is off, so a @PUT \/{pkg}@ is not an allowed method here. The @Allow@
-- header advertises the read methods the package route does serve.
publishDisabled :: MountRenderer -> Response
publishDisabled renderer =
    renderedResponse status405 [("Allow", "GET, HEAD")] (renderError renderer Nothing "publishing is not enabled on this proxy (no publication target is configured)")

-- A @403@ for a publish whose name is outside the configured publish-scope
-- allow-list — the anti-shadowing guard, refused before any upstream write.
outOfScope :: MountRenderer -> PublishDeps -> PackageName -> Response
outOfScope renderer deps name =
    renderedResponse status403 [] (renderError renderer (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': its name is outside the configured publish-scope allow-list (the anti-shadowing guard against publishing a name that shadows a public package)"

-- A @403@ for a publish whose document body declares a package name — its @_id@,
-- top-level @name@, or a @versions[].name@ — that disagrees with the scope-guarded
-- URL-path name. The body-name agreement leg of the anti-shadowing guard (issue #391),
-- refused before any upstream write so the identity the guard authorises is the
-- identity written.
bodyNameMismatch :: MountRenderer -> PublishDeps -> PackageName -> Text -> Response
bodyNameMismatch renderer deps name declared =
    renderedResponse status403 [] (renderError renderer (pubHelp deps) message)
  where
    message :: Text
    message =
        "refusing to publish '"
            <> renderPackageName name
            <> "': the document body declares the name '"
            <> declared
            <> "', which disagrees with the URL-path package name the scope guard authorised (the anti-shadowing guard against publishing a name the allow-list never saw)"

{- The first declared body name that disagrees with the URL-path name, or 'Nothing'
when the body declares no disagreeing name. The publish document carries its own
identity — a top-level @_id@ and @name@, and a @name@ per entry in @versions@ — so a
relay that keyed the write off the body could otherwise write a name the scope guard
never authorised. Each __present__ declared name is canonicalised the same way the
route builds its 'PackageName' ('projectName') and compared by 'PackageName' equality
(ecosystem-aware, so an encoding variant of the same name cannot disagree silently); a
present name that does not equal the URL-path name is a disagreement. Only the names
are read — the base64 @_attachments@ are never decoded. An __absent__ name is not a
claim, so it is not a disagreement (a legitimate npm client always sends matching
names); a body that does not decode to a JSON object likewise declares no readable
name and raises none, leaving the relay to meet the target's own validation. -}
bodyNameDisagreement :: PackageName -> LByteString -> Maybe Text
bodyNameDisagreement name body =
    case Aeson.decode body of
        Nothing -> Nothing
        Just document -> find disagrees (declaredNames document)
  where
    disagrees :: Text -> Bool
    disagrees declared = case projectName declared of
        Right declaredName -> declaredName /= name
        Left _ -> True

-- Every package-name string a publish document declares as its own identity: the
-- top-level @_id@ and @name@, and each @versions.<v>.name@. Only string-valued name
-- slots are read (a non-string slot is no name claim); the base64 @_attachments@ are
-- never touched.
declaredNames :: Value -> [Text]
declaredNames document =
    [ declared
    | slot <-
        [document ^? key "_id", document ^? key "name"]
            <> [ versionDoc ^? key "name"
               | versions <- toList (document ^? key "versions" . _Object)
               , versionDoc <- KeyMap.elems versions
               ]
    , Just (String declared) <- [slot]
    ]
