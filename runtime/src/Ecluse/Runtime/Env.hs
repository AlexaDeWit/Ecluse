-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root: the single record from which every effectful component is reached,
and the one place backend choice is resolved. Each handle it holds is an opaque record of
functions whose closures already capture their backend's private state, so nothing downstream
inspects which backend it got. It also carries the @http-client@ 'Manager' the data plane
shares, so pooling and TLS setup happen once. Consumers read it through a projection:
'serveRuntimeOf' per request, 'workerRuntimeOf' for the mirror worker.

== Invariants

* __No backend SDK appears here.__ 'Env' imports the handle /records/ only, never a cloud
  SDK, and their effectful fields return 'IO'. That is what keeps an adapter from importing
  back into this module (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").

* __It is the sole composition root.__ The single-process proxy and the split deployment
  (@ecluse proxy --no-worker@ beside an @ecluse mirror@ fleet) both wire up through here and
  nowhere else (see @docs\/architecture\/cloud-backends.md@ → "Process model").
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

{- | The composition-root record from which the whole effectful shell is reached. The module
header states the no-SDK and sole-composition-root invariants it upholds.
-}
data Env = Env
    { envServeAdmission :: ServeAdmission
    {- ^ The process-wide brief-wait bound for metadata-bearing serve work
    ("Ecluse.Core.Server.Admission"). Every mount shares this one aggregate cap and waiting room.
    -}
    , envQueue :: MirrorQueue
    {- ^ The mirror-queue handle: the durable hand-off from the request path to the
    mirror worker.
    -}
    , envManager :: Manager
    {- ^ The shared validating-TLS 'Manager' for the __untrusted__ data plane: public
    metadata fetches and artifact streams. Egress is https-only, so certificate validation
    authenticates the dialled host. A public @dist.tarball@ cannot steer the proxy at an
    internal or rebound address (see "Ecluse.Core.Security.Egress").
    -}
    , envPrivateManager :: Manager
    {- ^ The 'Manager' for the __trusted__ private upstream, the same validating TLS manager as
    'envManager' and held to the same https-only requirement. The split stays because the two
    origins differ in credential handling and in the @dist.tarball@ host gate's trust.
    -}
    , envMetadataCache :: MetadataCache
    {- ^ The metadata cache ("Ecluse.Core.Server.Cache"). One parsed packument serves the
    packument and tarball-gating fetches. Hot-package resolutions collapse to one upstream call.
    -}
    , envLogEnv :: LogEnv
    {- ^ The @katip@ logging environment (see "Ecluse.Runtime.Log"): the structured-log stream
    every layer attaches context to. Its stdout scribe and format are chosen at startup.
    -}
    , envTelemetry :: Telemetry
    {- ^ The OpenTelemetry handle ("Ecluse.Runtime.Telemetry") that emits spans and metrics.
    It is an inert no-op unless @ECLUSE_OBSERVABILITY__TELEMETRY@ is set.
    -}
    , envMetrics :: Metrics
    {- ^ The @ecluse.*@ metric instruments ("Ecluse.Runtime.Telemetry.Instruments"), built once
    from 'envTelemetry'. They are inert when telemetry is off, so a layer records unconditionally.
    -}
    , envDdContext :: DdContext
    {- ^ The resolved @dd@ log identity (@service@\/@env@\/@version@, see
    "Ecluse.Runtime.Telemetry.Correlation"). Each log line adds the active span's trace\/span ids.
    -}
    , envWorkerHeartbeat :: WorkerHeartbeat
    {- ^ The time of the mirror worker's last successful poll ("Ecluse.Core.Worker"). The liveness
    probe reads it, so a stalled worker shows up in health, separately from HTTP readiness.
    -}
    }

{- | Assemble an 'Env' from its built handles and the two data-plane 'Manager's, one per origin.
The caller supplies each handle and owns its lifetime, so assembly opens no socket itself.
-}
newEnvWithAdmission :: ServeAdmission -> MirrorQueue -> Manager -> Manager -> MetadataCache -> LogEnv -> Telemetry -> WorkerHeartbeat -> IO Env
newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat = do
    metrics <- newMetrics telemetry
    -- The dd log identity comes from the (already-normalised) OTEL_* environment, the
    -- same precedence table the exporter uses, so logs and traces share one identity.
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

{- | Assemble an 'Env' and run an action in its scope, the scope the server and worker run in.
The root borrows every resource it holds, so it has nothing to release and needs no bracket.
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
    env <- liftIO (newEnvWithAdmission admission queue manager privateManager metadataCache logEnv telemetry heartbeat)
    action env

{- | Project the 'ServeRuntime' the serve path closes over, built per request at dispatch.
The core pipeline reads its backends through it without depending on this application 'Env'.
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

{- | Project the 'WorkerRuntime' the mirror worker closes over, the analogue of 'serveRuntimeOf'.
'WorkerPolicies' is an argument because it derives from the served mounts. The worker re-runs
it against a job before mirroring, through one codepath with the serve gate.
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
