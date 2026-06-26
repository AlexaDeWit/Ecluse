{- | The composition root: the single record from which every effectful
component is reached.

'Env' is the one place backend choice is resolved. It holds the proxy's __handles__
— the registry-protocol client, the mirror queue, and the outbound-credential
provider — each an opaque record of functions (the Handle pattern) whose closures
already capture their backend's private state. Nothing downstream inspects which
backend a handle is; it only applies the field. Alongside the handles it carries the
shared @http-client@ 'Manager' that the data plane (metadata fetch, artifact
streaming) reuses across every request, so connection pooling and TLS setup are
established once.

Two invariants make this hold together:

* __No backend SDK appears here.__ 'Env' imports only the handle /records/, never a
  cloud SDK (no @amazonka@, no GCP client). Each handle's effectful fields return
  'IO' (not 'Ecluse.App.App'), so an adapter never imports back into this module —
  there is no import cycle and no recursive @Env@-holds-a-handle-whose-methods-need-@Env@
  knot (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").

* __It is the sole composition root.__ The server and worker are each a
  self-contained entry function over this shared record
  (@runServer :: Env -> IO ()@, @runWorker :: Env -> IO ()@ in "Ecluse"), so the
  single-process program and any future split into separate binaries both wire up
  through here and nowhere else (see
  @docs\/architecture\/cloud-backends.md@ → "Process model").

Request handlers read this 'Env' through a per-request
'Ecluse.Server.Context.RequestCtx' that pairs it with the matched mount; the
worker\/service layer reads it through "Ecluse.App"'s @App@ monad.
-}
module Ecluse.Env (
    -- * Composition root
    Env (..),
    newEnv,
    withEnv,

    -- * Worker heartbeat
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
) where

import Data.Time (UTCTime)
import Katip (LogEnv)
import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO, bracket)

import Ecluse.Credential (CredentialProvider)
import Ecluse.Log (DdContext)
import Ecluse.Queue (MirrorQueue)
import Ecluse.Registry (RegistryClient)
import Ecluse.Server.Cache (MetadataCache)
import Ecluse.Telemetry (Telemetry)
import Ecluse.Telemetry.Correlation (ddIdentityFromEnvironment)
import Ecluse.Telemetry.Instruments (Metrics, newMetrics)

{- | The composition-root record: the handles plus the shared HTTP manager and the
metadata cache, from which the whole effectful shell is reached. See the module
header for the no-SDK and sole-composition-root invariants it upholds.
-}
data Env = Env
    { envRegistry :: RegistryClient
    {- ^ The registry-protocol handle the mirror __worker__ publishes approved
    packages through. The request serve path does __not__ read it: each upstream
    of a packument merge builds its own client over 'envManager' (two upstreams, with
    per-origin credentials), so this slot is the __publish side__. One npm client serves
    every cloud, since protocol and auth are orthogonal axes; until a backend is
    configured behind it the slot is a refusing placeholder (see
    'Ecluse.unconfiguredRegistry').
    -}
    , envQueue :: MirrorQueue
    {- ^ The mirror-queue handle: the durable hand-off from the request path to the
    mirror worker.
    -}
    , envCredentials :: CredentialProvider
    {- ^ The outbound-credential handle: mints the bearer token used to write
    approved packages to the mirror target.
    -}
    , envManager :: Manager
    {- ^ The shared @http-client@ 'Manager' for the __untrusted__ data plane — the
    public-upstream metadata fetch and every artifact stream — so connection pooling
    and TLS are established once and reused across requests. This manager carries the
    resolved-IP SSRF recheck (see "Ecluse.Security.Egress"): a public fetch whose host
    resolves to an internal address is refused at connect time, since a public
    @dist.tarball@ is upstream-chosen and could otherwise steer the proxy at an
    internal target.
    -}
    , envPrivateManager :: Manager
    {- ^ The @http-client@ 'Manager' for the __trusted__ private upstream. The
    private base URL is operator-configured and deliberately trusted — it may
    legitimately resolve to an internal address (a registry on the private network) —
    so this manager does __not__ carry the resolved-IP recheck that 'envManager' does.
    The trust split is by origin: only the untrusted public\/artifact fetches are guarded
    (see @docs\/architecture\/security.md@).
    -}
    , envMetadataCache :: MetadataCache
    {- ^ The short-TTL, size-bounded metadata cache (see "Ecluse.Server.Cache")
    shared by the serve paths: one parsed packument is reused across the packument
    and tarball-gating fetches, and concurrent resolutions of a hot package
    collapse to a single upstream call.
    -}
    , envLogEnv :: LogEnv
    {- ^ The @katip@ logging environment (see "Ecluse.Log"): the structured-log
    stream every layer attaches context to, with its stdout scribe and format
    chosen at startup.
    -}
    , envTelemetry :: Telemetry
    {- ^ The OpenTelemetry handle (see "Ecluse.Telemetry"): the tracer and meter
    providers spans and metrics are emitted through, or — by default, with
    @PROXY_TELEMETRY@ unset — the inert no-op that emits nothing. Its provider
    lifecycle is bracketed by the composition root that supplies it.
    -}
    , envMetrics :: Metrics
    {- ^ The @ecluse.*@ metric instruments (see "Ecluse.Telemetry.Instruments"),
    built once from 'envTelemetry' so every layer records through the same
    instruments. Inert when telemetry is off (the instruments are created on the
    SDK's no-op meter), so a layer records unconditionally.
    -}
    , envDdContext :: DdContext
    {- ^ The resolved @dd@ log identity (@service@\/@env@\/@version@, see
    "Ecluse.Telemetry.Correlation"), installed as the initial @katip@ context at the
    request and worker entry points so every line carries the @dd@ object; the active
    span's trace\/span ids are filled per line on top of it.
    -}
    , envWorkerHeartbeat :: WorkerHeartbeat
    {- ^ The mirror worker's consume-loop heartbeat: the time of its
    last-successful-poll. Distinct from the server's HTTP readiness — it is the
    worker's own liveness surface — and read by the liveness probe so a stalled
    worker is visible in single-process health (see "Ecluse.Worker").
    -}
    }

{- | The mirror worker's consume-loop heartbeat: the wall-clock time of the
worker's __last successful poll__ of the queue.

It is the worker's own liveness signal, kept apart from the server's HTTP
readiness so single-process health reflects a stalled worker today and a future
standalone worker binary keeps the same probe. The worker 'recordPoll's after each
successful @receive@ (whether or not the batch was empty — an empty long-poll is a
healthy idle, not a stall); a liveness probe reads 'lastPoll' and compares it
against the wall clock to decide whether the loop has gone quiet for too long.
-}
newtype WorkerHeartbeat = WorkerHeartbeat (TVar (Maybe UTCTime))

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = WorkerHeartbeat <$> newTVarIO Nothing

{- | Record the time of a successful queue poll, advancing the heartbeat. Called
by the worker after each @receive@ returns (the loop is alive even on an empty
batch).
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll (WorkerHeartbeat var) now = atomically (writeTVar var (Just now))

{- | The time of the worker's last successful poll, or 'Nothing' before its first.
A liveness probe reads this and compares it against the wall clock.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll (WorkerHeartbeat var) = readTVarIO var

{- | Assemble an 'Env' from its constructed handles and the two data-plane HTTP
'Manager's — the guarded one for the untrusted public\/artifact fetches and the
trusted one for the private upstream.

The 'Manager's, 'MetadataCache', 'LogEnv', and 'Telemetry' handle are taken as
arguments rather than built here: a 'Manager' owns a connection pool whose lifetime
should be bracketed by the caller that also owns teardown (see 'withEnv'), and
injecting them keeps 'Env' assembly pure of network, logging, and telemetry setup —
so it can be exercised in tests against in-memory handle doubles with no sockets
opened, no scribe attached to stdout, and no exporter initialised. Backend
selection happens in the handle smart constructors that produce the arguments;
this only gathers them.
-}
newEnv :: RegistryClient -> MirrorQueue -> CredentialProvider -> Manager -> Manager -> MetadataCache -> LogEnv -> Telemetry -> WorkerHeartbeat -> IO Env
newEnv registry queue credentials manager privateManager metadataCache logEnv telemetry heartbeat = do
    -- The metric instruments are built once here from the telemetry handle: created on
    -- its meter provider when enabled, on the SDK's no-op meter when off (so they are
    -- inert without an SDK). Building them in 'newEnv' keeps the construction the single
    -- source of telemetry-derived state, so no caller threads a separate handle.
    metrics <- newMetrics telemetry
    -- The dd log identity is resolved from the (already-normalised) OTEL_* environment,
    -- the same precedence table the exporter uses, so logs and traces share one identity.
    ddContext <- ddIdentityFromEnvironment
    pure
        Env
            { envRegistry = registry
            , envQueue = queue
            , envCredentials = credentials
            , envManager = manager
            , envPrivateManager = privateManager
            , envMetadataCache = metadataCache
            , envLogEnv = logEnv
            , envTelemetry = telemetry
            , envMetrics = metrics
            , envDdContext = ddContext
            , envWorkerHeartbeat = heartbeat
            }

{- | Build an 'Env', run an action against it, and tear it down — even on
exception or asynchronous cancellation. The teardown is bracketed via @unliftio@,
so the composition root's resources are released along every exit path; this is
the scope within which the server and worker run.
-}
withEnv ::
    (MonadUnliftIO m) =>
    RegistryClient ->
    MirrorQueue ->
    CredentialProvider ->
    Manager ->
    Manager ->
    MetadataCache ->
    LogEnv ->
    Telemetry ->
    WorkerHeartbeat ->
    (Env -> m a) ->
    m a
withEnv registry queue credentials manager privateManager metadataCache logEnv telemetry heartbeat =
    bracket
        (liftIO (newEnv registry queue credentials manager privateManager metadataCache logEnv telemetry heartbeat))
        teardown
  where
    -- The connection pool behind the 'Manager' and the telemetry providers behind
    -- the 'Telemetry' handle are each owned and released by whoever provided them
    -- (the manager's caller; 'Ecluse.Telemetry.withTelemetry' for the providers),
    -- and the handles hold no resource this root acquired — so the composition
    -- root has nothing of its own to release.
    teardown :: (MonadUnliftIO m) => Env -> m ()
    teardown _ = pure ()
