-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root: the single record from which every effectful
component is reached.

'Env' is the one place backend choice is resolved. It holds the proxy's __handles__
-- the mirror queue foremost -- each an opaque record of functions (the Handle
pattern) whose closures
already capture their backend's private state. Nothing downstream inspects which
backend a handle is; it only applies the field. Alongside the handles it carries the
shared @http-client@ 'Manager' that the data plane (metadata fetch, artifact
streaming) reuses across every request, so connection pooling and TLS setup are
established once.

Two invariants make this hold together:

* __No backend SDK appears here.__ 'Env' imports only the handle /records/, never a
  cloud SDK (no @amazonka@, no GCP client). Each handle's effectful fields return
  'IO' (not an application monad), so an adapter never imports back into this module --
  there is no import cycle and no recursive @Env@-holds-a-handle-whose-methods-need-@Env@
  knot (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").

* __It is the sole composition root.__ The server and worker are each a
  self-contained entry function over this shared record
  (@runServer :: Env -> IO ()@, @runWorker :: Env -> IO ()@ in @Ecluse@), so the
  single-process program and any future split into separate binaries both wire up
  through here and nowhere else (see
  @docs\/architecture\/cloud-backends.md@ → "Process model").

Request handlers read this 'Env' through a per-request
'Ecluse.Core.Server.Context.RequestCtx' -- the request runtime projected by
'serveRuntimeOf', paired with the matched mount; the mirror worker reads it through the
'Ecluse.Core.Worker.WorkerRuntime' projected by 'workerRuntimeOf'.
-}
module Ecluse.Runtime.Env (
    -- * Composition root
    Env (..),
    newEnvWithAdmission,
    withEnvWithAdmission,

    -- * Runtime projections
    serveRuntimeOf,
    workerRuntimeOf,

    -- * Worker heartbeat (re-exported from "Ecluse.Core.Worker")
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
) where

import Katip (LogEnv, katipAddContext)
import Network.HTTP.Client (Manager)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Server.Admission (ServeAdmission)
import Ecluse.Core.Server.Cache (MetadataCache)
import Ecluse.Core.Server.Context (ServeRuntime (..))
import Ecluse.Core.Worker (WorkerHeartbeat, WorkerPolicies, WorkerRuntime (..), lastPoll, newWorkerHeartbeat, recordPoll)
import Ecluse.Runtime.Log (DdContext)
import Ecluse.Runtime.Telemetry (Telemetry)
import Ecluse.Runtime.Telemetry.Correlation (ddIdentityFromEnvironment, ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, metricsPortOf, newMetrics, workerMetricsPortOf)
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf, workerTracingPortOf)

{- | The composition-root record: the handles plus the shared HTTP manager and the
metadata cache, from which the whole effectful shell is reached. See the module
header for the no-SDK and sole-composition-root invariants it upholds.
-}
data Env = Env
    { envServeAdmission :: ServeAdmission
    {- ^ The process-wide brief-wait bound for metadata-bearing serve work
    ("Ecluse.Core.Server.Admission"). It is projected into every request runtime,
    so all mounts share one aggregate cap and one waiting room.
    -}
    , envQueue :: MirrorQueue
    {- ^ The mirror-queue handle: the durable hand-off from the request path to the
    mirror worker.
    -}
    , envManager :: Manager
    {- ^ The shared @http-client@ 'Manager' for the __untrusted__ data plane (the
    public-upstream metadata fetch and every artifact stream), so connection pooling
    and TLS are established once and reused across requests. The standard validating TLS
    manager: registry egress is https-only by construction and certificate validation
    authenticates the dialled host (see "Ecluse.Core.Security.Egress"), so a public
    @dist.tarball@ cannot steer the proxy at an internal or rebound address, which has no
    CA-trusted certificate for the requested name.
    -}
    , envPrivateManager :: Manager
    {- ^ The @http-client@ 'Manager' for the __trusted__ private upstream. The private
    base URL is operator-configured and is held to the same https-only requirement; this
    manager is the same validating TLS manager as 'envManager'. The split is kept because
    the two origins differ in credential handling and in the @dist.tarball@ host gate's
    trust, not in the manager itself (see @docs\/architecture\/security.md@).
    -}
    , envMetadataCache :: MetadataCache
    {- ^ The short-TTL, size-bounded metadata cache (see "Ecluse.Core.Server.Cache")
    shared by the serve paths: one parsed packument is reused across the packument
    and tarball-gating fetches, and concurrent resolutions of a hot package
    collapse to a single upstream call.
    -}
    , envLogEnv :: LogEnv
    {- ^ The @katip@ logging environment (see "Ecluse.Runtime.Log"): the structured-log
    stream every layer attaches context to, with its stdout scribe and format
    chosen at startup.
    -}
    , envTelemetry :: Telemetry
    {- ^ The OpenTelemetry handle (see "Ecluse.Runtime.Telemetry"): the tracer and meter
    providers spans and metrics are emitted through, or -- by default, with
    @ECLUSE_TELEMETRY@ unset -- the inert no-op that emits nothing. Its provider
    lifecycle is bracketed by the composition root that supplies it.
    -}
    , envMetrics :: Metrics
    {- ^ The @ecluse.*@ metric instruments (see "Ecluse.Runtime.Telemetry.Instruments"),
    built once from 'envTelemetry' so every layer records through the same
    instruments. Inert when telemetry is off (the instruments are created on the
    SDK's no-op meter), so a layer records unconditionally.
    -}
    , envDdContext :: DdContext
    {- ^ The resolved @dd@ log identity (@service@\/@env@\/@version@, see
    "Ecluse.Runtime.Telemetry.Correlation"), installed as the initial @katip@ context at the
    request and worker entry points so every line carries the @dd@ object; the active
    span's trace\/span ids are filled per line on top of it.
    -}
    , envWorkerHeartbeat :: WorkerHeartbeat
    {- ^ The mirror worker's consume-loop heartbeat: the time of its
    last-successful-poll. Distinct from the server's HTTP readiness -- it is the
    worker's own liveness surface -- and read by the liveness probe so a stalled
    worker is visible in single-process health (see "Ecluse.Core.Worker").
    -}
    }

{- | Assemble an 'Env' from its constructed handles, an explicit process-wide serve
admission handle, and the two data-plane HTTP 'Manager's (one per origin: the untrusted
public\/artifact fetches and the trusted private upstream, both the validating TLS
manager). The executable passes the admission handle sized from its configured bound.

The 'Manager's, 'MetadataCache', 'LogEnv', and 'Telemetry' handle are taken as
arguments rather than built here: a 'Manager' owns a connection pool whose lifetime
should be bracketed by the caller that also owns teardown (see 'withEnvWithAdmission'),
and injecting them keeps 'Env' assembly pure of network, logging, and telemetry setup,
so it can be exercised in tests against in-memory handle doubles with no sockets
opened, no scribe attached to stdout, and no exporter initialised. Backend
selection happens in the handle smart constructors that produce the arguments;
this only gathers them.
-}
newEnvWithAdmission :: ServeAdmission -> MirrorQueue -> Manager -> Manager -> MetadataCache -> LogEnv -> Telemetry -> WorkerHeartbeat -> IO Env
newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat = do
    -- The metric instruments are built once here from the telemetry handle: created on
    -- its meter provider when enabled, on the SDK's no-op meter when off (so they are
    -- inert without an SDK). Building them in 'newEnvWithAdmission' keeps the construction
    -- the single source of telemetry-derived state, so no caller threads a separate handle.
    metrics <- newMetrics telemetry
    -- The dd log identity is resolved from the (already-normalised) OTEL_* environment,
    -- the same precedence table the exporter uses, so logs and traces share one identity.
    ddContext <- ddIdentityFromEnvironment
    pure
        Env
            { envServeAdmission = admission
            , envQueue = queue
            , envManager = manager
            , envPrivateManager = privateManager
            , envMetadataCache = metadataCache
            , envLogEnv = logEnv
            , envTelemetry = telemetry
            , envMetrics = metrics
            , envDdContext = ddContext
            , envWorkerHeartbeat = heartbeat
            }

{- | Assemble an 'Env' carrying an explicit serve admission handle and run an action
within its scope: the scope the server and worker run in. The composition root
__borrows__ every resource it holds (the 'Manager's, the 'Telemetry' providers); each
is owned and torn down by the caller that supplied it, so this root has nothing of its
own to release and needs no teardown bracket.
-}
withEnvWithAdmission ::
    (MonadIO m) =>
    ServeAdmission ->
    MirrorQueue ->
    Manager ->
    Manager ->
    MetadataCache ->
    LogEnv ->
    Telemetry ->
    WorkerHeartbeat ->
    (Env -> m a) ->
    m a
withEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat action = do
    -- The connection pool behind each 'Manager' and the telemetry providers behind
    -- the 'Telemetry' handle are owned and released by whoever provided them (the
    -- manager's caller; 'Ecluse.Runtime.Telemetry.withTelemetry' for the providers), and the
    -- handles hold no resource this root acquired -- so assembly needs no teardown
    -- bracket; the 'Env' simply scopes the action.
    env <- liftIO (newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat)
    action env

{- | Project the request runtime ("Ecluse.Core.Server.Context.ServeRuntime") the serve
path is closed over from the composition root: the two data-plane managers, the
metadata cache and mirror queue, and the OpenTelemetry-backed metric and tracing ports
('Ecluse.Runtime.Telemetry.Instruments.metricsPortOf', 'Ecluse.Runtime.Telemetry.Tracing.tracingPortOf').
Built at dispatch per request -- it gathers existing handles and wraps the instrument and
telemetry handles in their ports -- so the core pipeline reads its backends through the
core interface without depending on this application 'Env'.
-}
serveRuntimeOf :: Env -> ServeRuntime
serveRuntimeOf env =
    ServeRuntime
        { srAdmission = envServeAdmission env
        , srPublicManager = envManager env
        , srPrivateManager = envPrivateManager env
        , srMetadataCache = envMetadataCache env
        , srQueue = envQueue env
        , srMetrics = metricsPortOf (envMetrics env)
        , srTracing = tracingPortOf (envTelemetry env)
        }

{- | Project the worker runtime ("Ecluse.Core.Worker.WorkerRuntime") the mirror worker
is closed over from the composition root: the mirror queue, the untrusted
data-plane manager, the consume-loop heartbeat, and the
OpenTelemetry-backed worker metric and tracing ports
('Ecluse.Runtime.Telemetry.Instruments.workerMetricsPortOf',
'Ecluse.Runtime.Telemetry.Tracing.workerTracingPortOf'). Built at the worker entry point -- it
gathers existing handles and wraps the instrument and telemetry handles in their worker
ports -- so the core loop reads its backends through the core interface without depending
on this application 'Env' (the analogue of 'serveRuntimeOf' for the serve path).

The per-ecosystem bundles are passed in rather than read from the 'Env':
they are derived from the served mounts and the resolved publish targets (the same
prepared rules, artifact request formation, and public origin the serve path gates
with, plus each mount's married mirror-write capability), which the composition root
resolves alongside the handles ('Ecluse.Composition.Worker.workerPoliciesFor'), so
the worker re-runs current policy against a job before mirroring it through one
codepath with the serve gate, and publishes through the job ecosystem's own bundle.
-}
workerRuntimeOf :: WorkerPolicies -> Env -> WorkerRuntime
workerRuntimeOf policies env =
    WorkerRuntime
        { wrQueue = envQueue env
        , wrManager = envManager env
        , wrHeartbeat = envWorkerHeartbeat env
        , wrMetrics = workerMetricsPortOf (envMetrics env)
        , wrTracing = workerTracingPortOf (envTelemetry env)
        , wrInjectTraceContext = \action -> do
            dd <- liftIO $ ddPayloadNow (envDdContext env)
            katipAddContext dd action
        , wrPolicies = policies
        }
