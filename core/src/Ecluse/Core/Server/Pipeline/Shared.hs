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
('Ecluse.Core.Security.tarballHostAllowed', per @ECLUSE_RESPECT_UPSTREAM_TARBALL_HOST@)
admits its host (the default refuses a cross-host @dist.tarball@), and the untrusted
egress is https-only with certificate validation. The public leg is anonymous: it
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
module Ecluse.Core.Server.Pipeline.Shared (
    recognisedButUnserved,
    edgeAuthorised,
    edgeTokenAuthorised,
    edgeUnauthorised,
    forwardedToken,
    jsonResponse,
    renderedResponse,
    bodiless,
    integrityMissing,
    integrityBelowFloor,
    trustedIntegrityMissing,
    trustedIntegrityBelowFloor,
) where

import Data.Text qualified as T
import Network.HTTP.Types (ResponseHeaders, Status, hAuthorization, hContentType, status401, status501)
import Network.Wai (Request, Response, requestHeaders, responseHeaders, responseLBS, responseStatus)

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Server.Context (PackumentDeps (pdInboundToken))
import Ecluse.Core.Server.Response (
    MountRenderer,
    RejectReason (BelowIntegrityFloor, MissingIntegrity),
    Rejection (Rejection),
    RenderedBody (RenderedBody),
    ServeDecision (Reject),
    renderError,
 )

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

-- Strip a response's body while keeping its status and headers — the bodiless form a
-- HEAD reply takes on every branch (HTTP semantics: a HEAD carries no message body).
-- The headers a GET would carry (notably any relayed @Content-Length@) are preserved.
bodiless :: Response -> Response
bodiless response = responseLBS (responseStatus response) (responseHeaders response) ""

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
