-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy role's effectful composition root.

'runProxy' receives validated process state from "Ecluse.Boot" and resolves the
proxy-specific plans. It builds the runtime-edge handles and mount bindings, then
coordinates the HTTP server with the optional mirror worker and advisory-sync tasks.
Pure plan derivation remains in "Ecluse.Composition" and its sibling modules. This
module is the boundary where those decisions become running services.
-}
module Ecluse.Proxy (
    runProxy,
    runServer,
    runWorker,
    mountBindingFor,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Conc (setNumCapabilities)
import Katip (LogEnv, Severity (ErrorS), SimpleLogPayload, katipAddContext, katipAddNamespace, logFM, runKatipContextT, sl)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (concurrently_, race_)
import UnliftIO.Async (mapConcurrently_)

import Ecluse.Boot
import Ecluse.Composition (
    PublishBudget (PublishBudget, pbBodyBudget, pbMaxRequestBytes),
    planMounts,
    planPublishTargets,
 )
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Credential (initCredentialProviders)
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpAdmissionCapacity, mpMaxRequestBytes, mpMaxResponseBytes, mpMirrorArtifactTenant, mpPublishTenant, mpQueueMemoryMaxDepth, mpShedCapabilities),
    MirrorArtifactTenant (matMaxBytes),
    PublishTenant (ptAggregateBytes),
    mirrorArtifactBytesCap,
    planCacheConfig,
 )
import Ecluse.Composition.MirrorQueue (MirrorRuntimePlan (MirrorWith, NoMirroring))
import Ecluse.Composition.Plan (BootPlan (bpMemoryPlan, bpMirrorRuntime, bpPrivateConnections, bpPublicConnections))
import Ecluse.Composition.Sizing (connectionPoolSettings)
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Composition.Worker (workerPoliciesFor)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgServer),
    LimitsSettings (limMaxNestingDepth, limMaxVersionCount),
    ServerSettings (srvPort, srvShutdownDrainTimeout),
 )
import Ecluse.Core.Credential.Refresh (CredentialError (Unconfigured), CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Cve.Slot (generationInstalledAt)
import Ecluse.Core.Ecosystem (Ecosystem, prefixFor)
import Ecluse.Core.Queue (MirrorQueue, newEnqueueBuffer, noMirrorQueue, reportWorthy)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterEcosystem,
    adapterFor,
    adapterServe,
    serveCredential,
    serveRouter,
 )
import Ecluse.Core.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount))
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmission)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps, PublishDeps)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Permanent, Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
 )
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule), Provider (CodeArtifact))
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Worker (WorkerPolicies, heartbeatHealthyNow, runWorkerM, workerLoop)
import Ecluse.Proxy.CveSync (CveSyncHandle (csEnv, csReady, csSlot), cveRuleDepsFor, cveSyncReady, cveSyncScheduleFor, katipFaultReporter, planCveSync)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncEcosystem), SyncSchedule, runCveSync)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envMetrics, envTelemetry, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Server (MountBinding (..), ServerConfig (scCheckLive, scCheckReady, scDrainTimeout, scOnException, scPort), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
import Ecluse.Runtime.Server qualified as Server
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (advisorySyncMetricsPortOf, registerAdvisoryDatabaseAge)
import Ecluse.Runtime.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredMirrorEnqueueFailure,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Runtime.Telemetry.Tracing (advisorySyncTracingPortOf, instrumentDataPlaneManagerSettings)

{- | Assemble and run the proxy role from an already validated 'BootEnv'.

Écluse refuses unsafe or incomplete wiring before it opens the listener.
-}
runProxy :: BootEnv -> IO ()
runProxy bootEnv = do
    let env = beConfig bootEnv
    let config = beConfigFull bootEnv
    let logEnv = beLogEnv bootEnv
    let telemetry = beTelemetry bootEnv
    -- Every decision below comes from the plan "Ecluse.Boot" resolved and logged. This
    -- role only applies it.
    let bootPlan = beBootPlan bootEnv
    let runtimePlan = bpMirrorRuntime bootPlan
    let plan = bpMemoryPlan bootPlan

    -- The metric instruments do not exist until the telemetry substrate is built well below. The
    -- credential provider built here records through reporters that 'installMetrics' makes live.
    deferredMetrics <- newDeferredMetrics
    let credentialReporters =
            CredentialReporters
                { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
                , crRefreshReporter = deferredRefreshReporter deferredMetrics CodeArtifact
                }
    -- Each mount's mirror-write credential derives from the mirror-target host: a static token or
    -- the CodeArtifact mint. The mint runs once eagerly here, so a misconfiguration fails at boot.
    providers <- initCredentialProviders credentialReporters config >>= orExit (T.unlines . map renderBootError)
    -- Each mount ecosystem syncs independently, so one missing artifact never holds back
    -- another. Without a bucket the map is empty, rules abstain, and readiness is ungated.
    cveSyncPlan <- planCveSync logEnv (beAmbient bootEnv) env
    let ruleDepsFor = cveRuleDepsFor cveSyncPlan (deferredBreakerReporter deferredMetrics EffectfulRule) (katipFaultReporter logEnv)
    -- Where the plan shed the capability count (the nursery was the pressure),
    -- apply it in-process before the parallel machinery spins up.
    whenJust (mpShedCapabilities plan) setNumCapabilities
    serveAdmission <- newServeAdmission (mpAdmissionCapacity plan)
    -- One process-wide byte aggregate serves every publishing mount. It exists exactly when
    -- a publication target is configured, the same predicate the plan's tenant derives from.
    publishBudget <- forM (mpPublishTenant plan) $ \tenant -> do
        bodyBudget <- newByteAdmission (ptAggregateBytes tenant)
        pure PublishBudget{pbBodyBudget = bodyBudget, pbMaxRequestBytes = mpMaxRequestBytes plan}
    let limits =
            Limits
                { maxBodyBytes = mpMaxResponseBytes plan
                , maxVersionCount = limMaxVersionCount (cfgLimits env)
                , maxNestingDepth = limMaxNestingDepth (cfgLimits env)
                }
    bindings <- planMounts mountBindingFor getCurrentTime ruleDepsFor providers limits publishBudget config >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers config)
    heartbeat <- newWorkerHeartbeat
    let serverConfig =
            (mkServerConfig bindings)
                { scPort = srvPort (cfgServer env)
                , scDrainTimeout = ShutdownDrainTimeout (srvShutdownDrainTimeout (cfgServer env))
                , scCheckReady = cveSyncReady cveSyncPlan
                , -- Fold the worker heartbeat into /livez exactly when a worker will
                  -- run. A serve-only deployment's liveness is the listener alone.
                  scCheckLive = case runtimePlan of
                    MirrorWith _ -> heartbeatHealthyNow heartbeat
                    NoMirroring -> pure True
                , scOnException = warpExceptionHook logEnv
                }
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    -- The buffered hand-off keeps the serve path off the backend's enqueue latency. The drain
    -- loop below delivers off the request path. Under NoMirroring the inert queue is unreachable.
    (queue, mirrorDrain) <- case runtimePlan of
        MirrorWith queuePlan -> do
            backendQueue <- buildMirrorQueue logEnv (mpQueueMemoryMaxDepth plan) queuePlan
            (q, drainEnqueueBuffer) <-
                bufferedMirrorHandOff (logBootWarning logEnv) (deferredMirrorEnqueueFailure deferredMetrics) backendQueue
            pure (q, Just drainEnqueueBuffer)
        NoMirroring -> pure (noMirrorQueue, Nothing)
    metadataCache <- newMetadataCache (planCacheConfig (cfgCache env) plan)

    -- Registry egress is https-only by construction, and certificate validation authenticates the
    -- dialled host, so a rebound or internal address cannot present a CA-trusted certificate for
    -- the requested name. That closes the SSRF and resolve-to-internal class. The split stays
    -- because public reads are anonymous and private reads forward the client's credential.
    publicSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    privateSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    manager <- newManager (connectionPoolSettings (bpPublicConnections bootPlan) publicSettings)
    privateManager <- newManager (connectionPoolSettings (bpPrivateConnections bootPlan) privateSettings)
    withEnvWithAdmission serveAdmission queue manager privateManager metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
        -- The instruments exist now, so installing them makes the credential provider's deferred
        -- reporters live for the rest of the run.
        installMetrics deferredMetrics (envMetrics builtEnv)
        registerAdvisoryAges builtEnv cveSyncPlan
        -- The drain loop and the sync tasks never return, so the race cancels them at shutdown.
        -- A dropped job re-enqueues on the next demand and a cancelled sync resumes on next boot.
        let syncTasks = cveSyncTasks builtEnv (cveSyncScheduleFor env) cveSyncPlan
        -- Racing the server against an empty task list would cancel it instantly, so the no-task
        -- shape below runs the server alone. Under 'MirrorWith', the only branch that builds a
        -- worker, the mirror-artifact tenant is always present, so the fallback guards an
        -- impossible case.
        let workerArtifactMaxBytes = maybe mirrorArtifactBytesCap matMaxBytes (mpMirrorArtifactTenant plan)
        case mirrorDrain of
            Just drainEnqueueBuffer ->
                race_
                    (runServices serverConfig (workerPoliciesFor builtEnv bindings publishTargets workerArtifactMaxBytes) builtEnv)
                    (concurrently_ (superviseDrain builtEnv drainEnqueueBuffer) (mapConcurrently_ id syncTasks))
            Nothing
                | null syncTasks -> runServer serverConfig builtEnv
                | otherwise ->
                    race_
                        (runServer serverConfig builtEnv)
                        (mapConcurrently_ id syncTasks)

{- The buffered hand-off in front of the mirror queue's backend. A dropped or undelivered
job is safe, because the serve path re-enqueues it on the next demand for its artifact. -}
bufferedMirrorHandOff :: (Text -> IO ()) -> IO () -> MirrorQueue -> IO (MirrorQueue, IO ())
bufferedMirrorHandOff warn countEnqueueFailure =
    newEnqueueBuffer
        Composition.mirrorEnqueueBufferDepth
        ( \drops -> do
            when (enqueueReportWorthy drops) $
                warn ("mirror enqueue buffer full: " <> show drops <> " job(s) dropped so far; each is re-enqueued on the next demand for its artifact")
            countEnqueueFailure
        )
        ( \failures detail -> do
            when (enqueueReportWorthy failures) $
                warn ("mirror enqueue delivery failed (" <> show failures <> " so far): " <> detail)
            countEnqueueFailure
        )

{- True for the first event, then every 'Composition.mirrorEnqueueReportInterval'-th. Only
the log line is rate-limited, and the metric alongside counts every event. -}
enqueueReportWorthy :: Int -> Bool
enqueueReportWorthy n = reportWorthy n Composition.mirrorEnqueueReportInterval

{- Attach each ecosystem's advisory-database age to the observable gauge, once at boot. The
callback reads the slot, which outlives the supervised sync tasks below, so the reported age
keeps climbing from the last install across a task restart. -}
registerAdvisoryAges :: Env -> Map.Map Ecosystem CveSyncHandle -> IO ()
registerAdvisoryAges builtEnv plan =
    for_ (Map.toList plan) $ \(eco, handle) ->
        registerAdvisoryDatabaseAge (envMetrics builtEnv) eco (generationInstalledAt (csSlot handle))

-- One supervised sync task per configured ecosystem. Each flips its ecosystem's one-way
-- readiness flag once its first sync lands, and a restart resumes from the remote artifact.
cveSyncTasks :: Env -> SyncSchedule -> Map.Map Ecosystem CveSyncHandle -> [IO ()]
cveSyncTasks builtEnv schedule plan =
    [ void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "cve-sync" $
        superviseLoop
            (transientPolicy ("cve-sync[" <> show (syncEcosystem (csEnv handle)) <> "]"))
            (runCveSync syncMetrics syncTracing (csEnv handle) schedule (atomically (writeTVar (csReady handle) True)))
    | handle <- Map.elems plan
    ]
  where
    syncMetrics = advisorySyncMetricsPortOf (envMetrics builtEnv)
    syncTracing = advisorySyncTracingPortOf (envTelemetry builtEnv)

{- The policy for the shell's background loops, which have no wiring fault to fail up on.
Every synchronous escape is residue and is retried. -}
transientPolicy :: Text -> SupervisionPolicy
transientPolicy label =
    SupervisionPolicy
        { spLabel = label
        , spClassify = const Transient
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}
        }

{- The enqueue-buffer drain under the shared supervision combinator. Pacing lives in the
buffer's own loop, so this wrapper only stops residue ending mirror-job delivery. -}
superviseDrain :: Env -> IO () -> IO ()
superviseDrain builtEnv drain =
    void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "mirror-enqueue-drain" $
        superviseLoop (transientPolicy "mirror-enqueue-drain") (liftIO drain)

{- Run the server and the mirror worker over one composition-root 'Env'. The worker loop
never returns, so the server's graceful return must cancel it, never wait on it. -}
runServices :: ServerConfig -> WorkerPolicies -> Env -> IO ()
runServices serverConfig policies env =
    Server.raceServerAgainstLoop (runServer serverConfig env) (runWorker policies env)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the
config-derived 'ServerConfig'.

'mountBindingFor' projects each adapter's serve surface into the bindings, so the web
layer stays ecosystem-neutral.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env = Server.runWarp cfg (`Server.tracedApplication` env)

{- Warp's exception hook over the process logger. 'Warp.defaultShouldDisplayException'
filters routine client disconnects, so an aborted download does not spam the log. -}
warpExceptionHook :: LogEnv -> Maybe Wai.Request -> SomeException -> IO ()
warpExceptionHook logEnv mRequest err =
    when (Warp.defaultShouldDisplayException err) $
        runKatipContextT logEnv (mempty :: SimpleLogPayload) "server" $
            katipAddContext payload $
                logFM ErrorS "a fault escaped to the server (a post-commit teardown, or warp's own connection handling)"
  where
    payload =
        sl "path" (maybe ("unknown" :: Text) (decodeUtf8 . Wai.rawPathInfo) mRequest)
            <> sl "detail" (displayExceptionT err)

{- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that
ecosystem has no registered adapter.

The path prefix is derived from the ecosystem ('prefixFor'), never configured (see
@docs\/architecture\/web-layer.md@ → "Multi-ecosystem mounts").
-}
mountBindingFor :: Ecosystem -> PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding
mountBindingFor eco packumentDeps publishDeps =
    adapterFor eco <&> \adapter -> mountOf adapter packumentDeps publishDeps

{- The mount projection of one adapter: its serve router under the derived prefix.
'Nothing' publish deps leave @PUT \/{pkg}@ answering @405@: no publication target. -}
mountOf :: RegistryAdapter -> PackumentDeps -> Maybe PublishDeps -> MountBinding
mountOf adapter packumentDeps publishDeps =
    MountBinding
        { bindingPrefix = prefixFor (adapterEcosystem adapter)
        , bindingRouter = serveRouter (adapterServe adapter)
        , bindingCredential = serveCredential (adapterServe adapter)
        , bindingPackumentDeps = packumentDeps
        , bindingPublishDeps = publishDeps
        }

{- | Run the supervised mirror worker over the composition-root 'Env' and the
per-ecosystem bundles.

The loop is consume → probe → re-evaluate → fetch → verify → publish → ack, so the worker
re-runs current policy against a job before it mirrors.
-}
runWorker :: WorkerPolicies -> Env -> IO ()
runWorker policies env = do
    dd <- ddPayloadNow (envDdContext env)
    void (runWorkerM (envLogEnv env) dd (workerRuntimeOf policies env) (katipAddNamespace "worker" (workerLoop workerSupervision)))

{- The worker's supervision policy. An unconfigured credential leaf is a wiring fault no
retry can fix, so it takes the process down for a restart against corrected configuration. -}
workerSupervision :: SupervisionPolicy
workerSupervision =
    SupervisionPolicy
        { spLabel = "worker"
        , spClassify = classify
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}
        }
  where
    classify fault
        | Just (Unconfigured _) <- fromException fault = Permanent
        | otherwise = Transient
