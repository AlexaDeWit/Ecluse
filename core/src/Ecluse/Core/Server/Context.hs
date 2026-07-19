-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | The per-request context the serve path reads through, and the handler monad
over it.

Mount dispatch matches a request to one 'MountBinding' -- a mount's __complete__
ecosystem wiring -- then runs the route's handler in 'Handler', a reader over a
'RequestCtx' pairing that binding with the request runtime 'ServeRuntime'. A handler
reads its per-mount dependencies (the classifier, the packument-serve dependencies,
the path prefix) and the shared runtime from that one context,
rather than taking them as explicit arguments threaded down the pipeline.

'ServeRuntime' is the __runtime interface__ the serve path is closed over: the two
data-plane HTTP managers, the metadata cache, the mirror queue, and the abstract
metric- and tracing-recording ports. It holds precisely what the pipeline needs to
serve a request and nothing more; the application's composition root constructs it
(wiring the concrete OpenTelemetry-backed ports), and a test constructs it over
doubles. Logging is __not__ a field: a handler logs through the ambient @katip@
context, which the dispatch boundary establishes (with the structured-log scribes and
the trace-correlation @dd@ object) when it runs the handler.

'RequestCtx' is a concrete record with plain accessors ('ctxRuntime', 'ctxMount'). The
handler monad layers over @katip@'s logging context, so a structured log call composes
uniformly across the serve path.
-}
module Ecluse.Core.Server.Context (
    -- * Request runtime
    ServeRuntime (..),

    -- * Packument-serve dependencies
    PackumentDeps (..),
    MirrorServePlan (..),
    tarballHostHonoured,

    -- * Publish-serve dependencies
    PublishDeps (..),

    -- * The serve action, and the router an adapter supplies
    RouteAction (..),
    ResponseAction (..),
    MountRouter,

    -- * Mount binding
    MountBinding (..),

    -- * Per-request context
    RequestCtx (..),

    -- * The handler monad
    Handler,
    runHandler,
) where

import Data.IP (IPRange)
import Data.Time (UTCTime)
import Katip (Katip, KatipContext, LogEnv, SimpleLogPayload)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.HTTP.Client (Manager, Request)
import Network.HTTP.Types (Method)
import Network.Wai (ResponseReceived)
import Network.Wai qualified as Wai
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Package (InvalidEntry, PackageName, Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Package.Merge (DivergencePolicy, MergePlan, SourceId)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry (PublishRelayFault, PublishRelayResponse, UrlFormationError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (MetadataClient, MetadataError)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (HostPort, Limits, Origin, TarballHostGate, tarballHostAllowed, thgAllowlist, thgEcosystemHosts)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Admission (ServeAdmission)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission)
import Ecluse.Core.Server.Cache (MetadataCache)
import Ecluse.Core.Server.Contract (ResponseContract)
import Ecluse.Core.Server.Metadata (ManifestCaching)
import Ecluse.Core.Server.Response (HelpMessage)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

{- | The runtime backends the serve path is closed over: exactly the effectful
capabilities a request needs to fetch, gate, serve, and record. A record of concrete
handles and abstract ports (the Handle pattern), assembled by the composition root and
read by every handler through the 'RequestCtx'.

The two HTTP managers carry the per-origin split: the public manager serves the
untrusted public-upstream and artifact egress, the private manager the trusted
private-upstream path. Both are the validating TLS manager (registry egress is
https-only by construction; certificate validation authenticates the host), so the
split is in credential handling and the @dist.tarball@ host gate's trust, not the
manager. The metadata cache and mirror queue are the shared data-plane handles. The
metric and tracing ports are the abstract recording interfaces
("Ecluse.Core.Telemetry.Record", "Ecluse.Core.Telemetry.Span"); the application supplies
their OpenTelemetry-backed implementations, so the serve path records without naming a
telemetry backend. There is no log field: handlers log through the ambient @katip@
context.
-}
data ServeRuntime = ServeRuntime
    { srAdmission :: ServeAdmission
    {- ^ The process-wide brief-wait bound around metadata materialisation
    ("Ecluse.Core.Server.Admission"). A private tarball hit and the artifact
    streaming pump stay outside it; packument work and a tarball miss's public
    metadata gate acquire a slot, waiting briefly for one under load.
    -}
    , srPublicManager :: Manager
    {- ^ The validating-TLS data-plane manager for the __untrusted__ public-upstream
    metadata fetch and every artifact stream.
    -}
    , srPrivateManager :: Manager
    {- ^ The manager for the __trusted__ private upstream. The same validating TLS
    manager; the private origin differs in credential handling, not in the manager.
    -}
    , srMetadataCache :: MetadataCache
    {- ^ The short-TTL, size-bounded metadata cache shared by the serve paths
    (see "Ecluse.Core.Server.Cache").
    -}
    , srQueue :: MirrorQueue
    {- ^ The mirror-queue handle: the durable, best-effort hand-off from the serve
    path to the mirror worker.
    -}
    , srMetrics :: MetricsPort
    -- ^ The metric-recording port the serve path emits the @ecluse.*@ catalogue through.
    , srTracing :: TracingPort
    -- ^ The tracing port the serve path opens its hand-added domain spans through.
    }

{- | Whether an admitted public artifact is enqueued for the demand-driven mirror, and
where that write lands. The discriminant is an absent capability, not a no-op handle:
a serve-only mount opens no mirror producer span and emits no enqueue metric, so the
telemetry never claims work that cannot happen.
-}
data MirrorServePlan
    = {- | Enqueue admitted public artifacts for publication to this mirror-target
      endpoint (the mount's declared destination; the worker resolves its publish
      capability from the same configuration).
      -}
      MirrorOnAdmit Text
    | {- | Serve-only: admitted public artifacts stream to the client and are never
      mirrored anywhere. Every artifact stays on the gated public leg.
      -}
      NoMirrorWrite
    deriving stock (Eq, Show)

{- | The per-mount inputs the serve handlers need beyond the request runtime
'ServeRuntime': the upstream endpoints, the mount's externally-visible base URL,
the mirror serve plan, its resolved rule policy, the edge auth token, the
wall-clock source, and the operator help message.

These are a mount-level concern, resolved at the composition root (a separate
concern) and carried on the mount's 'MountBinding'; a handler reads exactly what it
needs to decide and serve from the 'RequestCtx' it runs in. Both the packument and
the tarball paths share these deps -- the tarball path additionally gates one
version and, under 'MirrorOnAdmit', enqueues a mirror job -- so the name is retained
for continuity rather than narrowed to one route.
-}
data PackumentDeps = PackumentDeps
    { pdPrivateBaseUrl :: Maybe Text
    {- ^ The private upstream base URL; under @passthrough@, reads forward the
    client's credential. 'Nothing' when the mount has no private upstream (a
    serve-only pure public gate): the private leg is structurally absent, never
    fetched, and a tarball request is a clean private miss straight to the public leg.
    -}
    , pdPublicBaseUrl :: Text
    -- ^ The public upstream base URL; reads are anonymous (no client credential).
    , pdMountBaseUrl :: Text
    {- ^ The mount's externally-visible base URL, under which served @dist.tarball@
    URLs are rewritten so artifacts are fetched back through the gate.
    -}
    , pdMirror :: MirrorServePlan
    {- ^ Whether an admitted public artifact is enqueued for the demand-driven
    mirror, and the declared destination when it is ('MirrorOnAdmit'); a
    serve-only mount carries 'NoMirrorWrite' and never enqueues.
    -}
    , pdRules :: [PreparedRule]
    {- ^ The mount's resolved rule set as the engine's prepared runtime rules
    ("Ecluse.Core.Rules.PreparedRule"), evaluated against every public version. The
    built-in rules run directly; an effectful rule carries a resilience policy. The
    composition root 'prepare's it (and logs its boot order) once.
    -}
    , pdAdditionalBlockedRanges :: [IPRange]
    {- ^ The operator-configured ranges (@ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES@) extending the
    fixed literal internal-range block when gating an honoured artifact location
    ('Ecluse.Core.Security.tarballHostAllowed'), the cheap pure defence-in-depth that
    complements the host allowlist. Empty by default.
    -}
    , pdTarballHostGate :: TarballHostGate
    {- ^ The mount-constant inputs to the per-request tarball-host gate
    ('Ecluse.Core.Security.TarballHostGate'): the canonicalised @host:port@ allowlist
    and the private and public upstream authorities, extracted __once__ at the
    composition root from the base URLs above. The hot artifact path reads these fields
    rather than rebuilding the allowlist set and re-parsing the base URLs on every
    request (only the dynamic public @dist.tarball@ authority is parsed per request).

    __Invariant__: this is a cached projection of 'pdPrivateBaseUrl', 'pdPublicBaseUrl',
    and 'pdMirror'; whoever changes one of those after construction must re-derive
    it via 'Ecluse.Core.Security.tarballHostGate' or the gate goes stale. The composition
    root builds the deps once, so production never does; a test that record-updates a URL
    field must rebuild the gate (the serve-path test harness does this centrally).
    -}
    , pdLimits :: Limits
    {- ^ The response-bound budget enforced on every upstream metadata fetch and
    decode (@ECLUSE_LIMITS__MAX_RESPONSE_BYTES@\/@ECLUSE_LIMITS__MAX_VERSION_COUNT@\/@ECLUSE_LIMITS__MAX_NESTING_DEPTH@):
    the body-size, version-count, and JSON-nesting ceilings of
    'Ecluse.Core.Security.Limits'. The data plane reads the metadata body through
    'Ecluse.Core.Security.boundedRead' against @maxBodyBytes@, checks
    'Ecluse.Core.Security.checkNestingDepth' at the JSON-decode boundary, and
    'Ecluse.Core.Security.checkVersionCount' after projection; a breach degrades the
    contribution to nothing (the fail-closed parse-failure path), so a pathological
    upstream document is refused, never partially served (security.md invariant 4).
    -}
    , pdInboundToken :: Maybe Secret
    {- ^ The optional inbound token a client must present (@ECLUSE_SERVER__AUTH_TOKEN@);
    'Nothing' leaves the edge open (the network layer guards it).
    -}
    , pdNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'Ecluse.Core.Rules.Types.EvalContext'.
    Injected so the time-sensitive age gate is deterministic under test.
    -}
    , pdAdvisoryEtag :: IO (Maybe DbEtag)
    {- ^ A non-pinning read of the active advisory database's 'DbEtag' for the
    per-request 'Ecluse.Core.Rules.Types.EvalContext' (the same value
    'Ecluse.Core.Rules.rdCurrentAdvisoryEtag' provides, bridged onto these deps
    because the serve gate is where the context is built). 'Nothing' when no
    database is loaded.
    -}
    , pdHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to every denial body, if configured.
    , pdMinIntegrity :: MinIntegrity
    {- ^ The minimum integrity algorithm a __public__ (untrusted) version's digest must
    meet to be admitted (the global @ECLUSE_INTEGRITY__MIN_PUBLIC@ floor, default SHA-256).
    The public gate refuses a version whose strongest digest is below this; it is
    __hard-floored at SHA-256__ and never lowerable (see "Ecluse.Core.Package.Integrity").
    The trusted private path consults 'pdMinTrustedIntegrity' instead.
    -}
    , pdMinTrustedIntegrity :: MinTrustedIntegrity
    -- ^ The minimum integrity hash required for a trusted upstream dependency.
    , pdDivergencePolicy :: DivergencePolicy
    {- ^ What to do with a served version a cross-upstream integrity divergence was
    detected on (@ECLUSE_INTEGRITY__DIVERGENCE_POLICY@, default 'Ecluse.Core.Package.Merge.Warn').
    The signal (the @WARNING@ log and the @ecluse.registry.merge.divergence@ counter)
    fires regardless; this only decides whether the contested version is additionally
    withheld from the served listing ('Ecluse.Core.Package.Merge.FailClosed').
    -}
    , pdNewMetadataClient ::
        TracingPort ->
        MetricsPort ->
        Metric.Upstream ->
        ManifestCaching ->
        (PackageName -> MetadataError -> IO ()) ->
        (PackageName -> [InvalidEntry] -> IO ()) ->
        (PackageName -> IO ()) ->
        Limits ->
        Manager ->
        Text ->
        Maybe Secret ->
        MetadataClient
    {- ^ Build a per-request metadata client for one origin, given the per-fetch
    parameters. The composition root closes over the ecosystem's raw fetch
    primitives; the pipeline supplies only the per-request runtime parameters.
    -}
    , pdBuildArtifactRequestByFile :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> Text -> Either UrlFormationError Request
    {- ^ Build an artifact request by conventional filename path for the private
    (trusted) leg.
    -}
    , pdBuildArtifactRequestByUrl :: Limits -> Manager -> Text -> Maybe Secret -> Text -> Either UrlFormationError Request
    -- ^ Build an artifact request by authoritative URL for the public leg.
    , pdAssemble :: Text -> Map SourceId CachedDoc -> MergePlan -> Maybe CachedDoc -> CachedDoc
    {- ^ Assemble the served document ('CachedDoc') from a merge plan and the raw source
    documents: rebuild the plan-owned keys onto the precedence-winning base document
    ('Nothing' when there is none) from the winning sources, rewriting each surviving
    version's artifact URL under the given mount base in the same pass. The adapter reads
    the documents; the pipeline threads them opaquely.
    -}
    , pdSerialise :: CachedDoc -> LByteString
    {- ^ Encode an assembled served document ('CachedDoc') to its wire bytes, through the
    adapter's own representation, so the serve tail materialises the served body without
    reading the document.
    -}
    , pdEgressUrl :: Text -> Either Text RegistryUrl
    {- ^ Form the validated egress witness for an artifact URL about to leave the
    process on a 'Ecluse.Core.Queue.MirrorJob'. The composition root wires the
    https-only 'Ecluse.Core.Security.Egress.mkRegistryUrl'; the loopback test
    harness substitutes its flag-gated dev former. On the production path every
    public artifact URL was already normalised to https at projection, so a 'Left'
    here is unreachable -- it fails the best-effort enqueue closed rather than
    letting an unwitnessed URL travel to the worker.
    -}
    }

{- | Whether an artifact's @dist.tarball@ authority may be fetched, given the
origin's trust and the authority that served the packument it came from. Connects
the pure 'Ecluse.Core.Security.tarballHostAllowed' to a mount's precomputed gate:
the tarball's @host:port@ pair must be on the upstream allowlist and equal to the
packument origin's pair, the ecosystem's own declared artifact hosts being the one
same-host equivalence.

The literal internal-range block is __origin-aware__: an
'Ecluse.Core.Security.UntrustedOrigin' (the public path) is gated against the fixed
range set plus the operator-configured @additionalBlockedRanges@, while an
'Ecluse.Core.Security.TrustedOrigin' (the operator-configured private upstream) is
exempt, since a private registry may legitimately live on an internal address
(security.md invariant 3). The allowlist and same-authority clauses still gate the
trusted origin identically.

This is the __one__ composition of the host gate over a mount's inputs: the serve
pipeline applies it before its public artifact fetch, and the composition root
closes it (against the public upstream authority) into the mirror worker's
re-evaluation bundle, so the ingest-time host check can never drift from the
serve-time one. Both authorities are __already extracted__ ('Nothing' meaning no
dialable authority, which the gate refuses): the mount-constant ones live in the
precomputed 'pdTarballHostGate', so the hot path parses no base URL and rebuilds
no allowlist per request; only the dynamic artifact authority is parsed at the
call site.
-}
tarballHostHonoured :: Origin -> PackumentDeps -> Maybe HostPort -> Maybe HostPort -> Bool
tarballHostHonoured origin deps =
    tarballHostAllowed
        (thgEcosystemHosts (pdTarballHostGate deps))
        origin
        (thgAllowlist (pdTarballHostGate deps))
        (pdAdditionalBlockedRanges deps)

{- | The per-mount inputs the first-party publish handler needs: the publication
target endpoint, the publish-scope allow-list (the anti-shadowing guard), the
optional static fallback credential, the edge token, the response-bound budget, and
the operator help message.

The mere __presence__ of these deps is the publish path's opt-in: a mount carries a
'PublishDeps' only when a publication target is configured, so the binding's
@bindingPublishDeps@ being 'Nothing' is exactly the "no publication target ⇒ a
@PUT \/{pkg}@ is @405 Method Not Allowed@" rule, modelled in the type rather than
re-derived at the handler (see
@docs\/architecture\/registry-model.md@ → "Publishing first-party packages").

The credential posture is __passthrough__, symmetric with the private-upstream read
under @passthrough@: the publisher's own forwarded token is what reaches the
publication target, the static 'pubStaticToken' only a fallback for a client that
sends none. Écluse mints no token of its own here -- unlike the mirror target -- so this
record carries no 'Ecluse.Core.Credential.CredentialProvider' (see
@docs\/architecture\/access-model.md@ → "Publishing: the publication target").
-}
data PublishDeps = PublishDeps
    { pubTargetUrl :: Text
    {- ^ The publication target endpoint (@ECLUSE_PUBLICATION_TARGET@) a client
    @npm publish@ is relayed to. The package path is appended to it.
    -}
    , pubScopes :: [Scope]
    {- ^ The configured publish allow-list (@ECLUSE_MOUNTS__{ECOSYSTEM}__PUBLISH_ALLOW@;
    for npm, a list of scopes) -- the
    anti-shadowing guard. A publish whose package name is not within one of these
    scopes is refused __before any upstream write__, so a client cannot publish a name
    that shadows an existing public package (a dependency-confusion vector). Never
    empty when a publication target is configured (config validation rejects that).
    -}
    , pubStaticToken :: Maybe Secret
    {- ^ The static fallback credential (@ECLUSE_PUBLICATION_TARGET_TOKEN@) forwarded to the
    publication target __only when the client sends no token of its own__. The default
    model is passthrough -- the publisher's own token -- so this is 'Nothing' on the
    common path.
    -}
    , pubInboundToken :: Maybe Secret
    {- ^ The optional inbound edge token a client must present (@ECLUSE_SERVER__AUTH_TOKEN@),
    the same gate the read paths apply; 'Nothing' leaves the edge open.
    -}
    , pubLimits :: Limits
    {- ^ The response-bound budget enforced on the publication target's response,
    carried for symmetry with the read paths.
    -}
    , pubBodyBudget :: ByteAdmission
    {- ^ The process-wide aggregate byte-admission buffered publish bodies are
    reserved against __before__ the body is read, shared by every mount; sized
    from the memory plan's publish tenant. Exhaustion sheds (503), exactly as the
    unit-slot admission does on the read path.
    -}
    , pubMaxRequestBytes :: Int
    {- ^ The per-request publish-body cap, in bytes, enforced by the publish route at
    the read site: a declared Content-Length over it fails closed before any byte is
    read, and a chunked body is bounded by a counted read against it. It is also the
    pessimistic weight a chunked body (no declared length) reserves against the
    aggregate body-byte budget.
    -}
    , pubHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to a publish denial, if configured.
    , pubRelayPublish :: Limits -> Manager -> Text -> Maybe Secret -> PackageName -> ByteString -> IO (Either PublishRelayFault PublishRelayResponse)
    -- ^ Relay a publish document to the publication target, returning its response.
    , pubCanonicaliseName :: Text -> Maybe PackageName
    {- ^ Canonicalise a raw package-name string to a 'PackageName', or 'Nothing' if
    it cannot be parsed. Used by the body-name agreement guard.
    -}
    , pubDeclaredNames :: LByteString -> [Text]
    {- ^ Extract every package name a publish body declares as its own identity, the
    ecosystem's own reading of its publish-document schema
    ('Ecluse.Core.Registry.Adapter.Types.publishDeclaredNames'). The body-name
    agreement guard canonicalises each and refuses any that disagrees with the URL-path
    name, so the pipeline compares names without knowing the wire schema. A body that
    declares no readable name yields @[]@.
    -}
    }

{- | How one matched request is served: by the proxy itself, or through the data plane.

This is the __whole__ of what the web layer knows about a request. A route is an
ecosystem's own concern (npm's @\/{pkg}\/-\/{file}.tgz@ and RubyGems' @\/versions@ have
nothing in common but the fact that something must be done about them), so the mapping
from a request to an action is declared by the ecosystem's adapter, as its 'MountRouter'.
What is shared is only the __kind__ of thing an action can be:

* An 'AnswerLocally' action is a pure value admitted by the route's response contract, so
  the dispatcher simply responds with it: no upstream round-trip, no effects.

* A 'RunPipeline' action is a data-plane handler awaiting the request and its typed respond
  continuation, so the dispatcher discharges it to 'IO' under the request perimeter (the
  guard that answers an escaped fault with the route's declared neutral @500@). Those handlers
  ("Ecluse.Core.Server.Pipeline") are themselves __ecosystem-neutral__: a registry's
  client, projection, and document assembly reach them as injected capabilities on
  'PackumentDeps', never as imports. So two ecosystems whose URL grammars share nothing
  can still route onto the same handler, and one with a route the other lacks simply
  names a different action.

Being a closed sum of exactly these two is what lets the front door serve a request
without knowing what the request /is/.

It lives here, beside 'MountBinding' and 'Handler', because the three are one
mutually-recursive knot: a mount carries a router, the router names an action, and an
action is a handler that reads the mount.
-}
data ResponseAction response
    = -- | A pure value admitted by the route's response contract.
      AnswerLocally response
    | {- | A data-plane handler and its pre-commit perimeter fallback. The handler receives
      only the responder for this @response@ type, so it cannot send an unrestricted WAI
      'Response'. The fallback is a value of the same type and is therefore documented by
      the same contract.
      -}
      RunPipeline response (Wai.Request -> (response -> IO ResponseReceived) -> Handler ResponseReceived)

{- | A matched route's response contract existentially paired with an action that can
produce only that contract's response type.

The existential is the application boundary's proof: dispatch can render the action
without knowing an ecosystem's response sum, while the action cannot be paired with a
contract for another sum.
-}
data RouteAction = forall response. RouteAction (ResponseContract response) (ResponseAction response)

{- | An ecosystem's __whole routing decision__: what to do with a mount-relative request.

The adapter supplies one ('Ecluse.Core.Registry.Adapter.Types.serveRouter'), derived from
its own declarative route table, so the ecosystem owns both halves of the decision: which
of its paths a request names, and what action that names. A path the ecosystem does not
recognise yields its deny-by-default @404@
('Ecluse.Core.Server.Pipeline.Shared.notFoundInMount').

The 'Method' is part of the mapping because the same path names different actions by
method (npm's @GET \/{pkg}@ reads, @PUT \/{pkg}@ publishes), and because a @HEAD@ is a
__bodiless variation__ of its @GET@ rather than a distinct action, which the router
resolves by selecting the head-mode handler. Segments arrive already mount-stripped and
percent-decoded.
-}
type MountRouter = Method -> [Text] -> RouteAction

{- | A mount: a path prefix bound to a registry, carrying that registry's
__complete__ ecosystem wiring. Dispatch matches a request's leading path segments
to 'bindingPrefix', strips them, and routes the remainder through the rest of the
binding.

The prefix is a 'NonEmpty' list of segments (@"npm" :| []@ for a @\/npm@ mount):
every registry is path-mounted, so a root mount -- which would force a URL change
on every consumer the day a second ecosystem is added -- is __unrepresentable__
rather than merely discouraged. Bundling the classifier and serve dependencies into one
record means a mount cannot be half-wired: there is no default to fall back to.
-}
data MountBinding = MountBinding
    { bindingPrefix :: NonEmpty Text
    -- ^ The leading path segments this mount is served under; never empty.
    , bindingRouter :: MountRouter
    {- ^ The ecosystem's whole routing decision: what this mount's native path names,
    and what serving it amounts to (an 'Ecluse.Core.Server.Dispatch.RouteAction'). The
    adapter derives it from its own route table, so the web layer holds no ecosystem's
    path grammar of its own.
    -}
    , bindingPackumentDeps :: PackumentDeps
    {- ^ The packument-serve dependencies. Not optional: a mount exists only for an
    ecosystem with a registered adapter, and the composition root builds these from that
    adapter, so a bound mount always serves packuments and artifacts. Only /publish/ is a
    genuine opt-in ('bindingPublishDeps').
    -}
    , bindingPublishDeps :: Maybe PublishDeps
    {- ^ The first-party publish dependencies, when a publication target is
    configured; 'Nothing' is the opt-out -- a @PUT \/{pkg}@ is then @405@ (no implicit
    write path).
    -}
    }

{- | The context one request is served through: the request runtime 'ServeRuntime'
paired with the 'MountBinding' the request matched. A concrete record with plain
accessors -- 'ctxRuntime' and 'ctxMount' -- so a handler reads the shared runtime and its
per-mount wiring from one place rather than as explicit arguments.

Dispatch builds it once per request; the handler reads it through the 'Handler' reader.
-}
data RequestCtx = RequestCtx
    { ctxRuntime :: ServeRuntime
    -- ^ The request runtime -- the data-plane managers, the caches and queue, the recording ports.
    , ctxMount :: MountBinding
    -- ^ The mount the request matched, carrying its complete ecosystem wiring.
    }

{- | The request hot path's monad: a reader over the per-request 'RequestCtx'
layered on @katip@'s logging context.

A @newtype@ over @'ReaderT' 'RequestCtx' ('KatipContextT' 'IO')@ so its instances
are this module's to control and call sites name one concrete monad. The derived
instances give reader access to the context ('MonadReader' 'RequestCtx'), arbitrary
effects ('MonadIO'), the unlift capability ('MonadUnliftIO') the serve path's
@concurrently@\/@bracket@ need, and the @katip@ classes ('Katip', 'KatipContext')
so a structured log call composes through the ambient context the dispatch boundary
establishes.

The @katip@ base is a reader, never a 'StateT', so logging context behaves
correctly across the serve path's concurrent fetches (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").
-}
newtype Handler a = Handler
    { unHandler :: ReaderT RequestCtx (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader RequestCtx
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

{- | Run a 'Handler' against the 'RequestCtx' dispatch built for the request and the
@katip@ logging environment and initial context the dispatch boundary supplies,
yielding the underlying 'IO' action the server's continuation runs in. This is the
boundary where the serve path's 'Handler' code is discharged to 'IO'.

The 'LogEnv' (the structured-log scribes) and the initial context payload are passed
in rather than read from the runtime, so the application owns the log stream and the
trace-correlation @dd@ enrichment: it resolves the @dd@ object for the request and
hands it here as the initial context, so every line a handler emits carries @dd@ for
trace-to-log correlation. A handler narrows the namespace or adds package\/version\/rule
context with @katip@'s combinators on top as it logs.
-}
runHandler :: LogEnv -> SimpleLogPayload -> RequestCtx -> Handler a -> IO a
runHandler logEnv initialContext ctx action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unHandler action) ctx)
