{- | The per-request context the serve path reads through, and the handler monad
over it.

Mount dispatch matches a request to one 'MountBinding' — a mount's __complete__
ecosystem wiring — then runs the route's handler in 'Handler', a reader over a
'RequestCtx' pairing that binding with the request runtime 'ServeRuntime'. A handler
reads its per-mount dependencies (the classifier, the packument-serve dependencies,
the error renderer, the path prefix) and the shared runtime from that one context,
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

    -- * Publish-serve dependencies
    PublishDeps (..),

    -- * Mount binding
    MountBinding (..),

    -- * Per-request context
    RequestCtx (..),

    -- * The handler monad
    Handler,
    runHandler,
) where

import Data.Time (UTCTime)
import Katip (Katip, KatipContext, LogEnv, SimpleLogPayload)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (Limits, LoweredHostSet, TarballHostPolicy)
import Ecluse.Core.Server.Cache (MetadataCache)
import Ecluse.Core.Server.Response (HelpMessage, MountRenderer)
import Ecluse.Core.Server.Route (Classifier)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

-- ── request runtime ───────────────────────────────────────────────────────────

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
    { srPublicManager :: Manager
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

-- ── packument-serve dependencies ──────────────────────────────────────────────

{- | The per-mount inputs the serve handlers need beyond the request runtime
'ServeRuntime': the two upstream endpoints, the mount's externally-visible base URL,
the mirror-target endpoint, its resolved rule policy, the edge auth token, the
wall-clock source, and the operator help message.

These are a mount-level concern, resolved at the composition root (a separate
concern) and carried on the mount's 'MountBinding'; a handler reads exactly what it
needs to decide and serve from the 'RequestCtx' it runs in. Both the packument and
the tarball paths share these deps — the tarball path additionally gates one
version and enqueues a mirror job to 'pdMirrorTarget' — so the name is retained for
continuity rather than narrowed to one route.
-}
data PackumentDeps = PackumentDeps
    { pdPrivateBaseUrl :: Text
    -- ^ The private upstream base URL; under @passthrough@, reads forward the client's credential.
    , pdPublicBaseUrl :: Text
    -- ^ The public upstream base URL; reads are anonymous (no client credential).
    , pdMountBaseUrl :: Text
    {- ^ The mount's externally-visible base URL, under which served @dist.tarball@
    URLs are rewritten so artifacts are fetched back through the gate.
    -}
    , pdMirrorTarget :: Text
    {- ^ The mount's mirror-target endpoint — where the demand-driven mirror worker
    publishes an approved artifact. Carried on the enqueued
    'Ecluse.Core.Queue.MirrorJob' as its publish destination; the serve path never reads
    or writes it itself.
    -}
    , pdRules :: [PreparedRule]
    {- ^ The mount's resolved rule set as the engine's prepared runtime rules
    ("Ecluse.Core.Rules.PreparedRule"), evaluated against every public version. The
    built-in rules run directly; an effectful rule carries a resilience policy. The
    composition root 'prepare's it (and logs its boot order) once.
    -}
    , pdTarballHostPolicy :: TarballHostPolicy
    {- ^ Whether a tarball may be fetched from a @dist.tarball@ host that differs
    from the upstream that served the packument
    ('Ecluse.Core.Security.SameHostAsPackument' by default, the secure reading of the
    host allowlist; relaxed to 'Ecluse.Core.Security.AnyAllowlistedHost' by
    @PROXY_RESPECT_UPSTREAM_TARBALL_HOST@).
    -}
    , pdAllowedInternalHosts :: LoweredHostSet
    {- ^ The hosts deliberately opted in to the literal internal-range block when gating
    an honoured artifact location ('Ecluse.Core.Security.tarballHostAllowed'), the cheap
    pure defence-in-depth that complements the host allowlist. Empty by default, the
    secure reading.
    -}
    , pdLimits :: Limits
    {- ^ The response-bound budget enforced on every upstream metadata fetch and
    decode (@PROXY_MAX_RESPONSE_BYTES@\/@PROXY_MAX_VERSION_COUNT@\/@PROXY_MAX_NESTING_DEPTH@):
    the body-size, version-count, and JSON-nesting ceilings of
    'Ecluse.Core.Security.Limits'. The data plane reads the metadata body through
    'Ecluse.Core.Security.boundedRead' against @maxBodyBytes@, checks
    'Ecluse.Core.Security.checkNestingDepth' at the JSON-decode boundary, and
    'Ecluse.Core.Security.checkVersionCount' after projection; a breach degrades the
    contribution to nothing (the fail-closed parse-failure path), so a pathological
    upstream document is refused, never partially served (security.md invariant 4).
    -}
    , pdInboundToken :: Maybe Secret
    {- ^ The optional inbound token a client must present (@PROXY_AUTH_TOKEN@);
    'Nothing' leaves the edge open (the network layer guards it).
    -}
    , pdNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'Ecluse.Core.Rules.Types.EvalContext'.
    Injected so the time-sensitive age gate is deterministic under test.
    -}
    , pdHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to every denial body, if configured.
    , pdMinIntegrity :: MinIntegrity
    {- ^ The minimum integrity algorithm a __public__ (untrusted) version's digest must
    meet to be admitted (the global @PROXY_MIN_PUBLIC_INTEGRITY@ floor, default SHA-256).
    The public gate refuses a version whose strongest digest is below this; it is
    __hard-floored at SHA-256__ and never lowerable (see "Ecluse.Core.Package.Integrity").
    The trusted private path consults 'pdMinTrustedIntegrity' instead.
    -}
    , pdMinTrustedIntegrity :: MinTrustedIntegrity
    {- ^ The minimum integrity algorithm a __trusted__ (private) version's digest must
    meet to be served (the global @PROXY_MIN_TRUSTED_INTEGRITY@ floor, default SHA-256).
    The trusted gate drops a private version whose strongest digest is below this from the
    served listing and falls a below-floor private artifact through to the public origin.
    Unlike 'pdMinIntegrity' it is __operator-loosenable below SHA-256__ (down to SHA-1 /
    MD5) for a legacy private mirror, where trust substitutes for cryptographic strength.
    -}
    }

-- ── publish-serve dependencies ────────────────────────────────────────────────

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
sends none. Écluse mints no token of its own here — unlike the mirror target — so this
record carries no 'Ecluse.Core.Credential.CredentialProvider' (see
@docs\/architecture\/access-model.md@ → "Publishing: the publication target").
-}
data PublishDeps = PublishDeps
    { pubTargetUrl :: Text
    {- ^ The publication target endpoint (@PUBLICATION_TARGET_URL@) a client
    @npm publish@ is relayed to. The package path is appended to it.
    -}
    , pubScopes :: [Scope]
    {- ^ The configured publish-scope allow-list (@PUBLISH_SCOPES@) — the
    anti-shadowing guard. A publish whose package name is not within one of these
    scopes is refused __before any upstream write__, so a client cannot publish a name
    that shadows an existing public package (a dependency-confusion vector). Never
    empty when a publication target is configured (config validation rejects that).
    -}
    , pubStaticToken :: Maybe Secret
    {- ^ The static fallback credential (@PUBLICATION_TARGET_TOKEN@) forwarded to the
    publication target __only when the client sends no token of its own__. The default
    model is passthrough — the publisher's own token — so this is 'Nothing' on the
    common path.
    -}
    , pubInboundToken :: Maybe Secret
    {- ^ The optional inbound edge token a client must present (@PROXY_AUTH_TOKEN@),
    the same gate the read paths apply; 'Nothing' leaves the edge open.
    -}
    , pubLimits :: Limits
    {- ^ The response-bound budget enforced on the publication target's response,
    carried for symmetry with the read paths.
    -}
    , pubHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to a publish denial, if configured.
    }

-- ── mount binding ─────────────────────────────────────────────────────────────

{- | A mount: a path prefix bound to a registry, carrying that registry's
__complete__ ecosystem wiring. Dispatch matches a request's leading path segments
to 'bindingPrefix', strips them, and routes the remainder through the rest of the
binding.

The prefix is a 'NonEmpty' list of segments (@"npm" :| []@ for a @\/npm@ mount):
every registry is path-mounted, so a root mount — which would force a URL change
on every consumer the day a second ecosystem is added — is __unrepresentable__
rather than merely discouraged. Bundling the classifier, serve dependencies, and
renderer into one record means a mount cannot be half-wired: there is no default
to fall back to.
-}
data MountBinding = MountBinding
    { bindingPrefix :: NonEmpty Text
    -- ^ The leading path segments this mount is served under; never empty.
    , bindingClassifier :: Classifier
    -- ^ The ecosystem path grammar mapping this mount's native path to an 'Ecluse.Core.Server.Route.Route'.
    , bindingPackumentDeps :: Maybe PackumentDeps
    {- ^ The packument-serve dependencies, when wired; 'Nothing' leaves the
    packument route recognised-but-unserved (the @501@ stub).
    -}
    , bindingPublishDeps :: Maybe PublishDeps
    {- ^ The first-party publish dependencies, when a publication target is
    configured; 'Nothing' is the opt-out — a @PUT \/{pkg}@ is then @405@ (no implicit
    write path).
    -}
    , bindingRenderer :: MountRenderer
    {- ^ This mount's renderer for error\/denial bodies — the ecosystem surface an
    in-mount @403@\/@404@\/@501@ is shaped into.
    -}
    }

-- ── per-request context ───────────────────────────────────────────────────────

{- | The context one request is served through: the request runtime 'ServeRuntime'
paired with the 'MountBinding' the request matched. A concrete record with plain
accessors — 'ctxRuntime' and 'ctxMount' — so a handler reads the shared runtime and its
per-mount wiring from one place rather than as explicit arguments.

Dispatch builds it once per request; the handler reads it through the 'Handler' reader.
-}
data RequestCtx = RequestCtx
    { ctxRuntime :: ServeRuntime
    -- ^ The request runtime — the data-plane managers, the caches and queue, the recording ports.
    , ctxMount :: MountBinding
    -- ^ The mount the request matched, carrying its complete ecosystem wiring.
    }

-- ── the handler monad ─────────────────────────────────────────────────────────

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
