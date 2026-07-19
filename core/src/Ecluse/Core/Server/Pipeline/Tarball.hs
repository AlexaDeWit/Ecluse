-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The serve paths behind the package routes: the artifact relay behind @GET \/{pkg}\/-\/{file}.tgz@.

This is the data-plane handler module for artifacts. It composes the
slices that decide /what/ to serve into one action in the
'Ecluse.Core.Server.Context.Handler' reader, reading its mount's serve dependencies and
the request runtime 'Ecluse.Core.Server.Context.ServeRuntime' from the request's
'Ecluse.Core.Server.Context.RequestCtx'.

== Artifact path

The tarball handler ('serveTarball') is the demand-driven artifact relay. Its two legs
locate the tarball differently, by the trust of their origin.

The __private__ leg is a __conventional stable read__: it fetches the tarball at
@{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ ('artifactRequestByFile'), addressed by the
client's requested filename, __without a private-packument fetch__ -- the stable,
cacheable shape an @npm ci@ install issues, so a worst-case lockfile fan-out pays one
artifact round-trip per tarball rather than a packument fetch+decode per tarball it
would only discard. The request __forwards the client's credential__ over the
__trusted__ manager, attached by npm's bearer scheme and finalised through the shared
redirect pin ('Ecluse.Core.Registry.Request.finaliseRequest', @redirectCount = 0@): this
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

The __public__ leg honours the __authoritative upstream location__ -- the
@Artifact.artUrl@ the projection preserved from the gated version's @dist.tarball@,
selected by the requested filename -- rather than reconstructing the conventional path,
so the proxy can front a public registry that serves its artifacts from a separate host
or an off-convention path (a CDN\/files host, a signed URL). That location is gated, not
trusted: it is fetched only when the tarball-host policy
('Ecluse.Core.Security.tarballHostAllowed', per @ECLUSE_RESPECT_UPSTREAM_TARBALL_HOST@)
admits its @host:port@ authority (the default refuses a cross-authority
@dist.tarball@, a different host or a different port alike), and the untrusted
egress is https-only with certificate validation. The public leg is anonymous: it
gates __that one version__ against the rules (the same machinery the packument path
gates the whole set with) and selects the artifact, and on an admit __streams the public
bytes from @artUrl@ and enqueues a 'Ecluse.Core.Queue.MirrorJob'__ (naming that
authoritative URL) for the worker to back-fill the mirror target; on a reject --
including a host the tarball-host policy refuses -- it selects the serve error model
(@403@\/@503@\/@500@\/@404@) through the route's injected reply factories. The enqueue is
__serve-then-enqueue, best-effort and non-blocking__: the artifact reaches the client
first, and an enqueue failure is swallowed rather than failing or delaying the response.
The public relay is additionally __judged__ at relay time ('RelayVerdict', status and
headers only, the body always verbatim): an anomalous relay -- a non-success passed
through, a success that is visibly not an artifact -- is logged and counted, and only
a clean artifact relay enqueues the mirror job.
Mirroring is __demand-driven__ -- a job is enqueued only here, on a tarball-path admit,
never when a packument is filtered. The two legs are not peers over time: the
back-fill retires each artifact from the public leg, so at steady state the private
conventional read serves the vast majority of tarball traffic and the public leg is
the transient onboarding\/fail-over ramp (see
@docs\/architecture\/registry-model.md@ → "Traffic shape over time"). The serve path does __not__ verify @dist.integrity@;
the client checks the artifact's own hash and the worker re-verifies before publishing.

An artifact is a __pass-through__ body -- served byte-identical to upstream's -- so its
conditional-GET handling __relays__ rather than computing an own ETag (see
@docs\/architecture\/web-layer.md@ → "Middleware and helper libraries", and contrast
the merged-packument own-ETag path): the client's @If-None-Match@\/@If-Modified-Since@
are forwarded onto the upstream artifact request on __both__ legs ('forwardValidators'),
and an upstream @304 Not Modified@ is relayed straight back to the client as a bodiless
@304@ ('Ecluse.Core.Server.Conditional.isNotModified' via the relay's accept predicate) rather than re-downloading the
tarball -- the cheap freshness check on the hot artifact path.
-}
module Ecluse.Core.Server.Pipeline.Tarball (
    TarballReplies (..),

    -- * The tarball handler
    serveTarball,
    headTarball,
) where

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, mkStatus)
import Network.Wai (Request, ResponseReceived, StreamingBody, requestHeaders)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    PackageDetails,
    PackageName,
 )
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionAdmit,
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
    admitArtifact,
 )
import Ecluse.Core.Queue (
    MirrorJob (MirrorJob, jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion),
    QueueFault,
    enqueue,
    qfDetail,
 )
import Ecluse.Core.Registry.Metadata (
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Rules.Types (EvalContext, mkEvalContext)
import Ecluse.Core.Security (
    Origin (TrustedOrigin, UntrustedOrigin),
    hostPortAddress,
    thgPrivateHostPort,
    thgPublicHostPort,
 )
import Ecluse.Core.Server.Admission (withServeAdmission)
import UnliftIO (withRunInIO)

import Ecluse.Core.Server.Conditional (forwardValidators)
import Ecluse.Core.Server.Context (
    Handler,
    MirrorServePlan (MirrorOnAdmit, NoMirrorWrite),
    MountBinding (bindingPackumentDeps),
    PackumentDeps (..),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.Path (Filename (Filename))
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    evalTier,
    logDenials,
    recordDenials,
    serveDecisionClass,
 )
import Ecluse.Core.Server.Pipeline.Origin (withPublicMetadataClient)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Relay (
    ArtifactServe (ServeFull, ServeHead),
    RelayVerdict (RelayedArtifact, RelayedNonSuccess, RelayedOddShape),
    acceptArtifact,
    observeRelayAnomaly,
    relayArtifact,
    relayUpstreamWhen,
    relayVerdict,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Response (
    ArtifactStatus (Forbidden, NotFound, Ok, ServerError, Unavailable'),
    RejectReason (Unavailable),
    Rejection (Rejection, rejectionMessage),
    RetryAfter (..),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    appendHelp,
    artifactStatus,
    artifactStatusCode,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (spanMirrorEnqueue, spanRuleEval)
import Ecluse.Core.Version (Version, renderVersion)

{- | The route-owned ways the tarball pipeline may answer. The response type is fixed by
the route's explicit pass-through contract; the pipeline never receives WAI's unrestricted
responder.
-}
data TarballReplies response = TarballReplies
    { tarballError :: Status -> ResponseHeaders -> Text -> response
    -- ^ An ecosystem-shaped local error.
    , tarballStream :: Status -> ResponseHeaders -> StreamingBody -> response
    -- ^ A transparent streamed upstream response.
    , tarballEmpty :: Status -> ResponseHeaders -> response
    -- ^ A transparent bodiless upstream response (@304@ or @HEAD@).
    }

{- | Serve a @GET \/{pkg}\/-\/{file}.tgz@ artifact request end to end, over the
request's 'RequestCtx'.

The mount's 'PackumentDeps' are read from the matched 'MountBinding'; an unwired mount is
the recognised-but-unserved @501@ stub (as for
'servePackument'). With dependencies wired and the edge token (if any) validated, the
two legs locate the tarball by the trust of their origin:

* the __private__ leg is a __conventional stable read__: it fetches
  @{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ by the requested filename
  ('artifactRequestByFile'), __forwarding the client's credential__ and __without a
  private-packument fetch__; a @2xx@ streams the bytes through with bounded memory and
  answers the request, any other status (or a connection failure) is a clean miss that
  falls through. It applies no serve-time integrity floor -- the bytes are still verified
  client-side and by the mirror worker (see the module header → "Artifact path");
* on a private miss the __public__ leg fetches that one version's metadata anonymously
  and gates it against the rules; an admit honours the gated @dist.tarball@, streaming
  the public bytes __and enqueuing a 'MirrorJob'__ (serve-then-enqueue, the enqueue
  best-effort and non-blocking), a reject selects the serve error model
  (@403@\/@503@\/@500@\/@404@) through the route's reply factories.

The public-upstream fetch is always anonymous (the client credential is never sent to the
public upstream); the mirror job carries no credential. The serve path does not
verify @dist.integrity@ (see the module header → "Artifact path").
-}
serveTarball ::
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarball = tarballWith ServeFull

{- | Serve a @HEAD \/{pkg}\/-\/{file}.tgz@ artifact request end to end, over the
request's 'RequestCtx'.

A HEAD must __never__ run the full-@GET@ streaming pump: a bodiless HEAD would
otherwise open the upstream artifact connection and pump a whole artifact body that
the reply then discards -- wasted upstream egress and a DoS-amplification lever (a
client forcing arbitrary full-artifact fetches with cheap HEADs). So this handler
gates the artifact through the __identical__ pipeline as 'serveTarball' -- the same
edge auth, host-allowlist, internal-range, and tarball-host policy, and the same
upstream-request construction -- but issues the upstream request as a HEAD and relays
its status and safe response headers ('relayArtifact') with __no body__
('Ecluse.Core.Server.Stream.probeUpstreamWhen'). On an admit no 'MirrorJob' is enqueued: a
HEAD serves no bytes, so there is nothing to back-fill (mirroring stays demand-driven
on the GET path). A refusal renders the same serve error model with an empty body.
-}
headTarball ::
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
headTarball = tarballWith ServeHead

-- The dispatch shared by 'serveTarball' and 'headTarball': read the mount's
-- dependencies and serve in the given mode.
tarballWith ::
    ArtifactServe ->
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
tarballWith mode replies name version filename request respond = do
    deps <- asks (bindingPackumentDeps . ctxMount)
    serveTarballWithDeps mode replies deps name version filename request respond

-- Serve a tarball once the mount's dependencies are known: edge auth, then the
-- private-hit / public-miss fetches the module header describes. The request runtime
-- is read from the request context. The 'ArtifactServe' mode is threaded into
-- both legs so a HEAD takes the identical gating as a GET, probing bodiless.
serveTarballWithDeps ::
    ArtifactServe ->
    TarballReplies response ->
    PackumentDeps ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarballWithDeps mode replies deps name version (Filename file) request respond
    | not (edgeTokenMatches (pdInboundToken deps) clientToken) =
        liftIO (respond (tarballError replies (mkStatus 401 "Unauthorized") [] "authentication required"))
    | otherwise = do
        rt <- asks ctxRuntime
        -- The client's conditional validators, relayed onto the upstream
        -- artifact request on both legs so upstream can answer a 304 for a
        -- pass-through body we serve unchanged (the conditional-GET contract).
        let validators = forwardValidators (requestHeaders request)
        privateHit <- streamPrivateArtifact mode replies rt deps clientToken validators name file respond
        case privateHit of
            Just received -> do
                -- A private hit is an admit served from the trusted upstream (no rule
                -- gate runs); a private miss falls through to the gated public path,
                -- which records its own decision.
                liftIO (mpServeDecision (srMetrics rt) Metric.Admit)
                pure received
            Nothing -> servePublicArtifact mode replies rt deps validators name version file respond
  where
    -- The client's bearer, scanned out of the headers once: the edge gate compares
    -- it and the private leg forwards it.
    clientToken = forwardedToken request

{- Stream the artifact from the __trusted__ private upstream as a __conventional stable
read__: build the tarball request at @{pdPrivateBaseUrl}\/{pkg}\/-\/{file}@ by the
client's requested filename ('artifactRequestByFile') and fetch it directly, __without
fetching the private packument first__. This is the stable, cacheable shape an @npm ci@
install issues; a worst-case lockfile fan-out therefore pays one artifact round-trip per
tarball rather than an uncached packument fetch+decode it would only discard.

The request __forwards the client's credential__ over the trusted manager, attached by
npm's bearer scheme and finalised through the shared redirect pin
('Ecluse.Core.Registry.Request.finaliseRequest', @redirectCount = 0@): the
credential-bearing read __never follows a redirect__ (a private
CDN @302@ is returned here, not chased with the bearer). The constructed URL is on the
private base host, so the 'Ecluse.Core.Security.TrustedOrigin' tarball-host gate is
satisfied __same-host__ (the host check is still applied, simply trivially met), and the
trusted origin is __exempt from the literal internal-range block__ (security.md
invariant 3): a private registry on an internal address (e.g. @https:\/\/registry.internal\/@,
served with a certificate the operator's image trusts) still serves its same-host tarball.

A @2xx@ is streamed through with bounded memory and yields 'Just' (the request is
answered); a non-@2xx@ status, an unformable URL, or a failure opening the connection
yields 'Nothing' so the caller falls through to the public origin, the upstream artifact
body never read. The client's conditional @validators@ are relayed onto the request
('forwardValidators' filtered them upstream), and the relay accepts an upstream @304 Not
Modified@ ('acceptArtifact') as well as a @2xx@: a private tarball is a pass-through body,
so a @304@ is relayed straight back to the client (bodiless) rather than treated as a
private miss falling through to the public origin.

A failure that strikes __after__ a @2xx@ has begun streaming is unrecoverable -- the
response is already on the wire -- so 'streamUpstreamWhen' lets it propagate rather than
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
    TarballReplies response ->
    ServeRuntime ->
    PackumentDeps ->
    Maybe Secret ->
    RequestHeaders ->
    PackageName ->
    Text ->
    (response -> IO ResponseReceived) ->
    Handler (Maybe ResponseReceived)
streamPrivateArtifact mode replies rt deps token validators name file respond =
    case privateRequest of
        Just req ->
            liftIO
                ( relayUpstreamWhen
                    mode
                    (srPrivateManager rt)
                    req
                    acceptArtifact
                    (\status headers -> pure (relayArtifact status headers))
                    (relayResponder replies respond)
                )
        Nothing -> pure Nothing
  where
    -- Build the conventional-URL private tarball request {base}/{pkg}/-/{file} by the
    -- requested filename, when the mount has a private upstream at all, its (same-)host
    -- passes the tarball-host policy, and the URL forms. 'Nothing' on any refusal -- a
    -- private miss the caller falls through on (an absent private leg, the serve-only
    -- pure gate, is the same clean miss). The constructed URL is on the private base
    -- host, so the host gate is trivially satisfied; it is kept applied rather than
    -- dropped. The request is marked with the serve mode's method (GET / HEAD) and
    -- carries the client's relayed conditional validators; 'artifactRequestByFile'
    -- attaches the forwarded credential with redirectCount = 0 (the credential-redirect
    -- invariant).
    privateRequest :: Maybe HTTP.Request
    privateRequest = case pdPrivateBaseUrl deps of
        Nothing -> Nothing
        Just privateBase
            | tarballHostHonoured TrustedOrigin deps privateHostPort privateHostPort ->
                withValidators validators . withMethod mode <$> rightToMaybe (pdBuildArtifactRequestByFile deps (pdLimits deps) (srPrivateManager rt) privateBase token name file)
            | otherwise -> Nothing
      where
        -- The precomputed private authority: the constructed URL is on the private
        -- base, so both the packument and the tarball sides of the trusted gate are it
        -- (the check stays applied, trivially satisfied, without re-parsing the URL).
        privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)

{- Serve the artifact from the public upstream after a private miss: gate the
single requested version against the rules, and on an admit stream the public bytes
(anonymously) and enqueue a mirror job; on a reject render the serve error model.
The public version metadata is fetched anonymously to decide. -}
servePublicArtifact ::
    ArtifactServe ->
    TarballReplies response ->
    ServeRuntime ->
    PackumentDeps ->
    RequestHeaders ->
    PackageName ->
    Version ->
    Text ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
servePublicArtifact mode replies rt deps validators name version file respond = do
    let metrics = srMetrics rt
    -- The advisory database active for this request, resolved once and used both for
    -- the version's evaluation and for a denial's audit line.
    advisoryEtag <- liftIO (pdAdvisoryEtag deps)
    withServeAdmission metrics (srAdmission rt) (gatePublicVersion rt deps name version file advisoryEtag) >>= \case
        Just (Admitted artifact) -> do
            liftIO (mpServeDecision metrics Metric.Admit)
            withRunInIO $ \runInIO ->
                streamPublicArtifact mode replies rt deps validators name version artifact (runInIO . observeRelayAnomaly metrics name version) respond
        Just (Refused decision) -> do
            liftIO (mpServeDecision metrics (serveDecisionClass decision))
            logDenials name advisoryEtag [VersionVerdict (renderVersion version) decision]
            liftIO (recordDenials metrics [decision])
            liftIO (respond (artifactError replies deps (artifactStatus decision) decision))
        Nothing -> liftIO $ do
            mpServeDecision metrics Metric.Unavailable
            respond (tarballError replies shedStatus [shedRetryAfter] "server is busy; retry later")

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
artifact, returning the gate outcome. The single-version metadata is fetched through the
public origin's read handle ('fetchVersionMetadata'), which resolves the full packument
__through the shared metadata cache__ -- so a packument @GET@ and the tarball gate that
follows still collapse to one upstream call -- and selects the requested version's
'PackageDetails'. That version is evaluated through 'Ecluse.Core.Rules.evalRules' (the same
engine the packument path gates with). On an admit the artifact matching the requested
filename is selected ('artifactFor'); a filename absent from an otherwise-admitted version
is a forwarded miss, the same @404@ as an absent version.

The refusal causes the error model maps: a version (or file) absent from the public
metadata is a genuine miss (a @404@ forwarded absence, projected as 'Unavailable'
'WontResolve' only to carry a non-admit -- the status is overridden to @404@ in
'artifactError'); a metadata fetch that fails -- a transport outage or any 'MetadataError',
a misreporting origin included -- is a transient upstream outage (@503@), the single-version
path collapsing every unobtainable-metadata cause to the same retryable outage; a present
version is decided by the rules, where a needed effectful rule that cannot be consulted
fail-closes to an 'Unavailable' @503@\/@500@. -}
gatePublicVersion :: ServeRuntime -> PackumentDeps -> PackageName -> Version -> Text -> Maybe DbEtag -> Handler PublicArtifactGate
gatePublicVersion rt deps name version file advisoryEtag = do
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pure advisoryEtag))
    eval <-
        withPublicMetadataClient rt deps (pdPublicBaseUrl deps) $ \client ->
            liftIO (fetchVersionDetails client name version)
    case eval of
        VersionMetadataUnavailable -> pure (Refused upstreamUnavailable)
        VersionMissing -> pure (Refused versionAbsent)
        VersionPresent details ->
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

{- Gate one requested artifact of one public version through the shared admission
gate ('Ecluse.Core.Package.Admission.admitArtifact', which the worker's ingest
re-evaluation also runs), projecting the shared 'ArtifactAdmission' onto the serve
surface: a denied or undecidable version carries its rule decision through
'serveDecisionOf' (a @403@, or a fail-closed @503@\/@500@ by transience); an
admitted version whose requested filename matches no artifact is a forwarded miss
('versionAbsent', rendered @404@); an artifact refused by the integrity-floor policy
('pdMinIntegrity') is 'integrityMissing' (no digest at all) or 'integrityBelowFloor'
(a digest, but too weak), both rendered @403@ and never fetched. This is the public
path; the trusted (private) artifact serve is a conventional stable read in
'streamPrivateArtifact' that applies no serve-time integrity floor, so it never
reaches this gate. -}
gateVersion :: EvalContext -> PackumentDeps -> Text -> PackageDetails -> IO PublicArtifactGate
gateVersion ctx deps file details = do
    admission <- admitArtifact ctx (pdRules deps) (pdMinIntegrity deps) file details
    pure $ case admission of
        -- The carried floor-checked digest set is the worker's ingest concern
        -- (its tamper gate and publish descriptor); the serve path streams
        -- without rehashing, so it has no consumer for it here.
        AdmissionAdmit artifact _ -> Admitted artifact
        AdmissionDenied decision -> Refused (serveDecisionOf details decision)
        AdmissionUndecidable decision -> Refused (serveDecisionOf details decision)
        AdmissionFileAbsent -> Refused versionAbsent
        AdmissionBelowFloor -> Refused integrityBelowFloor
        AdmissionIntegrityMissing -> Refused integrityMissing

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

{- Stream the artifact from the public upstream at its __authoritative location__,
__anonymously__ (the client credential is never sent to the public upstream), and --
__after__ the response is begun -- enqueue a best-effort mirror job. The chosen
'Artifact''s 'artUrl' is honoured directly rather than reconstructed (it is an
https-only URL, normalised at projection); the tarball-host policy gates whether that
location may be fetched (the public packument host is the reference), and certificate
validation on 'srPublicManager' authenticates the host. A host the policy refuses is the
@403@ policy-denial path; an unformable URL is the internal-error path.

The fetch keeps the open phase distinct from the committed stream, the same split the
private origin uses: opening the connection is the recoverable phase, so a transient
network failure or a TLS handshake failure (a host that cannot present a CA-trusted
certificate for the requested name) yields no committed response and is rendered as the
transient upstream-unavailable @503@ through the route's reply factories, not left to escape as
a bare @500@. Any upstream status is relayed
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
    TarballReplies response ->
    ServeRuntime ->
    PackumentDeps ->
    RequestHeaders ->
    PackageName ->
    Version ->
    Artifact ->
    -- | Observe the relay verdict (the anomaly log line and metric).
    (RelayVerdict -> IO ()) ->
    (response -> IO ResponseReceived) ->
    IO ResponseReceived
streamPublicArtifact mode replies rt deps validators name version artifact observeVerdict respond
    | not hostHonoured = respond (crossHostRefused replies)
    | otherwise = case publicRequest of
        Left _ -> respond (internalArtifactError replies)
        Right req -> do
            -- The verdict slot: written by the observing relay at relay time
            -- (status and headers only, before any body moves), read after the
            -- committed relay to gate the mirror enqueue and observe an anomaly.
            verdictRef <- newIORef Nothing
            let verdictingRelay status headers = do
                    atomicWriteIORef verdictRef (Just (relayVerdict status headers))
                    pure (relayArtifact status headers)
            relayUpstreamWhen mode (srPublicManager rt) req (const True) verdictingRelay (relayResponder replies respond) >>= \case
                Just received -> do
                    -- The committed relay always ran the verdicting relay exactly
                    -- once; an unwritten slot is an invariant break, folded into
                    -- the fail-closed odd shape rather than trusted as clean.
                    verdict <- fromMaybe (RelayedOddShape "the relay committed without classifying (invariant)") <$> readIORef verdictRef
                    observeVerdict verdict
                    -- Mirroring is demand-driven on the GET path only, and only a
                    -- clean artifact relay back-fills: a relayed miss would
                    -- enqueue a doomed job, an oddly-shaped 2xx a misleading one.
                    -- A serve-only mount ('NoMirrorWrite') never enqueues, so no
                    -- producer span opens and no enqueue metric fires for work
                    -- that cannot happen.
                    case (verdict, pdMirror deps) of
                        (RelayedArtifact, MirrorOnAdmit _) -> enqueueOnFull mode (enqueueMirror rt deps name version artifact)
                        (RelayedArtifact, NoMirrorWrite) -> pass
                        (RelayedOddShape _, _) -> pass
                        (RelayedNonSuccess _, _) -> pass
                    pure received
                Nothing -> respond (artifactError replies deps (artifactStatus upstreamUnavailable) upstreamUnavailable)
  where
    hostHonoured = tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps)) (hostPortAddress (artUrl artifact))

    publicRequest = withValidators validators . withMethod mode <$> pdBuildArtifactRequestByUrl deps (pdLimits deps) (srPublicManager rt) (pdPublicBaseUrl deps) Nothing (artUrl artifact)

-- Adapt the route's typed response constructors to the streaming helper's callback.
-- The upstream connection remains open until the selected response has completed.
relayResponder :: TarballReplies response -> (response -> IO received) -> RelayResponder received
relayResponder replies respond =
    RelayResponder
        (\status headers body -> respond (tarballStream replies status headers body))
        (\status headers -> respond (tarballEmpty replies status headers))

-- Run the demand-driven mirror enqueue only on the 'ServeFull' (GET) path; a
-- 'ServeHead' served no bytes, so it back-fills nothing.
enqueueOnFull :: ArtifactServe -> IO () -> IO ()
enqueueOnFull mode act = case mode of
    ServeFull -> act
    ServeHead -> pass

{- Enqueue a demand-driven mirror job for an admitted artifact, __best-effort__: it
runs after the client response is begun and any failure is swallowed, so a queue
outage never fails or delays the serve. The 'enqueue' it calls is the composition
root's buffered hand-off ('Ecluse.Core.Queue.newEnqueueBuffer'), so even a slow
backend's own producer latency (the SQS round trip) stays off the request path
rather than holding the served connection's turn. The job names the artifact's
authoritative URL (the same location the public fetch targeted); it carries no
credential and no mirror target (the worker mints its own token and publishes
through the mount-resolved target).

It also captures the __serve-time-admitted__ filename on the job: the selection
key the worker's ingest re-evaluation gates under current policy. Nothing else of
the artifact travels -- no digest and no size -- because the queue payload is a
trust boundary the worker grants no authority: the tamper gate and the publish
document both use the descriptor the worker derives from the re-admitted artifact.
The artifact URL travels as the validated egress witness ('pdEgressUrl'): the projection
already normalised it to https, so a witness that will not form is unreachable in
production and fails the best-effort enqueue closed (counted, never served-blocking). -}
enqueueMirror :: ServeRuntime -> PackumentDeps -> PackageName -> Version -> Artifact -> IO ()
enqueueMirror rt deps name version artifact =
    case pdEgressUrl deps (artUrl artifact) of
        Left _ -> mpMirrorEnqueueFailure (srMetrics rt)
        Right egressUrl ->
            void . spanMirrorEnqueue (srTracing rt) name version (artUrl artifact) enqueueErrorDetail $
                enqueueJob egressUrl
  where
    enqueueJob egressUrl traceContext = do
        enqueued <- enqueue (srQueue rt) (mirrorJob egressUrl traceContext)
        -- Best-effort: the typed hand-off outcome is counted but never propagated,
        -- so a refused hand-off records a failure rather than failing or delaying
        -- the serve. (Drops and backend delivery failures behind the buffered
        -- hand-off are counted by the composition root's buffer callbacks.)
        either (const (mpMirrorEnqueueFailure (srMetrics rt))) (const (mpMirrorEnqueued (srMetrics rt))) enqueued
        -- Hand the outcome back so the span bracket can mark a swallowed failure
        -- errored on the producer span (the metric counts it; the span explains it).
        pure enqueued

    mirrorJob egressUrl traceContext =
        MirrorJob
            { jobPackage = name
            , jobVersion = version
            , jobArtifactUrl = egressUrl
            , jobArtifactFilename = artFilename artifact
            , -- The enqueueing span's trace context, captured by the span
              -- bracket, so the worker's per-job span links back across the hop.
              jobTraceContext = traceContext
            }

    -- Project the swallowed enqueue outcome onto the producer span's status: a failure
    -- records the cause (so a trace explains why the mirror was not enqueued), a success
    -- leaves the status unset.
    enqueueErrorDetail :: Either QueueFault () -> Maybe Text
    enqueueErrorDetail = either (Just . enqueueFailureDetail) (const Nothing)

    enqueueFailureDetail :: QueueFault -> Text
    enqueueFailureDetail fault = "mirror enqueue failed: " <> qfDetail fault

{- A @403@ for an artifact whose authoritative @url@ the tarball-host gate refuses:
a @dist.tarball@ on a different host or port than the packument origin (the
ecosystem's own declared artifact hosts excepted), or an authority off the
upstream allowlist. A gate denial, not a serve outcome the rules produced -- the
same @403@ surface a rule denial renders, with a fixed reason. -}
crossHostRefused :: TarballReplies response -> response
crossHostRefused replies =
    tarballError replies (mkStatus 403 "Forbidden") [] "the upstream artifact host is not permitted by the tarball-host policy"

{- Render a non-admit artifact outcome as the serve error model: @403@ for a policy
denial, @503@ for a transient upstream unavailability, @404@ for a forwarded
upstream miss (the requested version is absent), @500@ otherwise. The body is shaped
by the mount's renderer; a transient status carries no suggested delay here (the
single-artifact path has none to offer). A @404@ is the version-absent miss, which
'gatePublicVersion' flags as a 'WontResolve' rejection -- the only such cause on this
path -- so it is mapped to @404@ rather than the @500@ a 'WontResolve' would
otherwise render. -}
artifactError :: TarballReplies response -> PackumentDeps -> ArtifactStatus -> ServeDecision -> response
artifactError replies deps status decision =
    tarballError replies (toStatus actualStatus) retryHeaders (appendHelp (pdHelp deps) message)
  where
    retryHeaders :: ResponseHeaders
    retryHeaders = case actualStatus of
        Unavailable' (Just (RetryAfter secs)) -> [(hRetryAfter, show secs)]
        _ -> []
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

{- A @500@ for an unformable upstream artifact URL -- a configuration fault, not a
serve decision. The package segment and filename are already known-safe, so this is
reachable only on a misconfigured base URL; it is the internal-error tier, distinct
from the rule\/upstream outcomes 'artifactError' renders. -}
internalArtifactError :: TarballReplies response -> response
internalArtifactError replies =
    tarballError replies (mkStatus 500 "Internal Server Error") [] "could not form the upstream artifact URL"
