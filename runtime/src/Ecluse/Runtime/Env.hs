-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Process resources shared by serving and worker runtimes. The application owns their lifetime.
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

import Katip (LogEnv, Severity (InfoS, WarningS), katipAddContext, sl)
import Network.HTTP.Client (Manager)

import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry.Backoff (CooldownEvent (..))
import Ecluse.Core.Registry.Public (PublicControl, newPublicControl)
import Ecluse.Core.Security.Authority (HostPort (..))
import Ecluse.Core.Server.Admission (ServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter)
import Ecluse.Core.Server.Cache (MetadataCache)
import Ecluse.Core.Server.Context (ServeRuntime (..))
import Ecluse.Core.Telemetry.Record (MetricsPort (mpUpstreamRequest))
import Ecluse.Core.Worker (WorkerHeartbeat, WorkerPolicies, WorkerRuntime (..), lastPoll, newWorkerHeartbeat, recordPoll)
import Ecluse.Runtime.Log (DdContext, logLine)
import Ecluse.Runtime.Telemetry (Telemetry)
import Ecluse.Runtime.Telemetry.Correlation (ddIdentityFromEnvironment, ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, metricsPortOf, newMetrics, workerMetricsPortOf)
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf, workerTracingPortOf)

-- | The composition-root record from which the whole effectful shell is reached.
data Env = Env
    { envServeAdmission :: ServeAdmission
    -- ^ Share the metadata admission limit and waiting room across all mounts.
    , envMemoryMeter :: MemoryMeter
    -- ^ The process-wide memory budget every serving mount pays into.
    , envQueue :: MirrorQueue
    -- ^ Queue mirror work from serving requests.
    , envManager :: Manager
    -- ^ Validate public TLS peers for metadata and artifact requests.
    , envPublicControl :: PublicControl
    -- ^ Shared cooldown and request observation for every public serve and worker read.
    , envPrivateManager :: Manager
    -- ^ Validate private TLS peers while retaining their credential handling.
    , envMetadataCache :: MetadataCache
    -- ^ Selected-version and assembled retention with transient full-read coalescing.
    , envLogEnv :: LogEnv
    -- ^ Use the configured output format and minimum severity for structured logs.
    , envTelemetry :: Telemetry
    -- ^ Emit spans and metrics when telemetry is enabled, otherwise do nothing.
    , envMetrics :: Metrics
    -- ^ Process-wide metric instruments are inert when telemetry is disabled.
    , envDdContext :: DdContext
    -- ^ Retain the resolved Datadog identity for per-line trace correlation.
    , envWorkerHeartbeat :: WorkerHeartbeat
    -- ^ Track completed worker progress for liveness, separately from HTTP readiness.
    }

-- | Assemble process handles without opening a socket. Callers own the supplied resources.
newEnvWithAdmission :: ServeAdmission -> MemoryMeter -> MirrorQueue -> Manager -> Manager -> MetadataCache -> LogEnv -> Telemetry -> WorkerHeartbeat -> IO Env
newEnvWithAdmission admission meter queue manager privateManager metadataCache logEnv telemetry heartbeat = do
    metrics <- newMetrics telemetry
    publicControl <- newPublicControl (mpUpstreamRequest (metricsPortOf metrics)) (reportCooldown logEnv)
    -- The dd log identity comes from the (already-normalised) OTEL_* environment, the
    -- same precedence table the exporter uses, so logs and traces share one identity.
    ddContext <- ddIdentityFromEnvironment
    pure
        Env
            { envServeAdmission = admission
            , envMemoryMeter = meter
            , envQueue = queue
            , envManager = manager
            , envPublicControl = publicControl
            , envPrivateManager = privateManager
            , envMetadataCache = metadataCache
            , envLogEnv = logEnv
            , envTelemetry = telemetry
            , envMetrics = metrics
            , envDdContext = ddContext
            , envWorkerHeartbeat = heartbeat
            }

-- | Run an action with borrowed process resources that require no additional release.
withEnvWithAdmission ::
    (MonadIO m) =>
    ServeAdmission ->
    MemoryMeter ->
    MirrorQueue ->
    Manager ->
    Manager ->
    MetadataCache ->
    LogEnv ->
    Telemetry ->
    WorkerHeartbeat ->
    (Env -> m a) ->
    m a
withEnvWithAdmission admission meter queue manager privateManager metadataCache logEnv telemetry heartbeat action = do
    env <- liftIO (newEnvWithAdmission admission meter queue manager privateManager metadataCache logEnv telemetry heartbeat)
    action env

-- | Project serving capabilities without exposing the application environment to the core.
serveRuntimeOf :: Env -> ServeRuntime
serveRuntimeOf env =
    ServeRuntime
        { srAdmission = envServeAdmission env
        , srMemoryMeter = envMemoryMeter env
        , srPublicManager = envManager env
        , srPublicControl = envPublicControl env
        , srPrivateManager = envPrivateManager env
        , srMetadataCache = envMetadataCache env
        , srQueue = envQueue env
        , srMetrics = metricsPortOf (envMetrics env)
        , srTracing = tracingPortOf (envTelemetry env)
        }

-- | Project worker capabilities with policies derived from the served mounts.
workerRuntimeOf :: WorkerPolicies -> Env -> WorkerRuntime
workerRuntimeOf policies env =
    WorkerRuntime
        { wrQueue = envQueue env
        , wrManager = envManager env
        , wrPublicControl = envPublicControl env
        , wrHeartbeat = envWorkerHeartbeat env
        , wrMetrics = workerMetricsPortOf (envMetrics env)
        , wrTracing = workerTracingPortOf (envTelemetry env)
        , wrInjectTraceContext = \action -> do
            dd <- liftIO $ ddPayloadNow (envDdContext env)
            katipAddContext dd action
        , wrPolicies = policies
        }

reportCooldown :: LogEnv -> HostPort -> CooldownEvent -> IO ()
reportCooldown logEnv authority event = logLine logEnv payload severity message
  where
    payload = sl "module" ("Ecluse.Runtime.Env" :: Text) <> sl "upstream_host" (hpHost authority) <> sl "upstream_port" (hpPort authority)
    (severity, message) = case event of
        CoolingDown -> (WarningS, "public upstream entered cooldown")
        Recovered -> (InfoS, "public upstream left cooldown")
