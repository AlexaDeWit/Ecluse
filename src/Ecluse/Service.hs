-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The assembly every mirror-pipeline role runs over: the composition-root 'Env' and the
services derived from it.

'withServiceRuntime' applies the 'Ecluse.Composition.Plan.BootPlan' decisions, resolves the
runtime-edge handles, mount bindings, and worker bundles, then hands the lot to a role.
"Ecluse.Proxy" adds the front door over it and "Ecluse.Mirror" runs the worker alone, so the
dedicated worker is the same worker the serve path embeds rather than a second copy of it.
Pure plan derivation stays in "Ecluse.Composition" and its siblings.
-}
module Ecluse.Service (
    -- * The role-shared runtime
    ServiceRuntime (..),
    withServiceRuntime,
    workerLiveness,

    -- * The mirror worker
    runWorker,

    -- * npm front door
    mountBindingFor,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Conc (setNumCapabilities)
import Katip (LogEnv, SimpleLogPayload, katipAddNamespace, runKatipContextT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

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
import Ecluse.Composition.MirrorRole (MirrorRole, enqueuesJobs, mirrorRoleRefusal, spawnsWorker)
import Ecluse.Composition.Plan (BootPlan (bpMemoryPlan, bpMirrorRuntime, bpPrivateConnections, bpPublicConnections))
import Ecluse.Composition.Sizing (connectionPoolSettings)
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Composition.Worker (workerPoliciesFor)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits),
    Config (configApp),
    LimitsSettings (limMaxNestingDepth, limMaxVersionCount),
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
    transientPolicy,
 )
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule), Provider (CodeArtifact))
import Ecluse.Core.Worker (Liveness, WorkerHeartbeat, WorkerPolicies, alwaysLive, heartbeatLivenessNow, runWorkerM, workerLoop)
import Ecluse.Cve.Sync (CveSyncHandle (csEnv, csReady), cveRuleDepsFor, cveSyncReady, cveSyncScheduleFor, katipFaultReporter, planCveSync)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncEcosystem, syncSlot), SyncSchedule, runCveSync)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envMetrics, envTelemetry, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Server (MountBinding (..))
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (advisorySyncMetricsPortOf, registerAdvisoryDatabaseAge)
import Ecluse.Runtime.Telemetry.Reporters (
    DeferredMetrics,
    deferredBreakerReporter,
    deferredMirrorEnqueueFailure,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Runtime.Telemetry.Tracing (advisorySyncTracingPortOf, instrumentDataPlaneManagerSettings)

{- | Everything a role needs to start its own tasks, built once by 'withServiceRuntime'. The
background tasks arrive already wrapped in their supervision policy.
-}
data ServiceRuntime = ServiceRuntime
    { svcRunsWorker :: Bool
    {- ^ Whether this process runs the mirror worker ('spawnsWorker'), the one fact both the
    spawn decision and the @\/livez@ arm below are derived from.
    -}
    , svcEnv :: Env
    , svcAppConfig :: AppConfig
    , svcBindings :: [MountBinding]
    -- ^ The resolved mounts. A worker-only role builds them for their rules, and serves none.
    , svcWorkerPolicies :: WorkerPolicies
    , svcMirrorDrain :: Maybe (IO ())
    {- ^ The supervised enqueue-buffer drain, present exactly when this role produces mirror
    jobs into a configured queue.
    -}
    , svcSyncTasks :: [IO ()]
    -- ^ One supervised advisory-sync task per configured ecosystem.
    , svcCheckReady :: IO Bool
    , svcCheckLive :: IO Liveness
    }

{- | Assemble the role's runtime and run @action@ within it. It refuses unsafe or incomplete
wiring, this role over this mirror runtime included, before it opens any listener.
-}
withServiceRuntime :: MirrorRole -> BootEnv -> (ServiceRuntime -> IO ()) -> IO ()
withServiceRuntime role bootEnv action = do
    let config = beConfig bootEnv
        appConfig = configApp config
        logEnv = beLogEnv bootEnv
        telemetry = beTelemetry bootEnv
        -- Every decision below comes from the plan "Ecluse.Boot" resolved and logged. This
        -- assembly only applies it.
        bootPlan = beBootPlan bootEnv
        mirrorRuntime = bpMirrorRuntime bootPlan
        memoryPlan = bpMemoryPlan bootPlan
    orExit (T.unlines . map renderBootError) (mirrorRoleRefusal role mirrorRuntime)

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
    -- another. Without a store the map is empty, rules abstain, and readiness is ungated.
    cveSyncPlan <- planCveSync logEnv (beS3Endpoint bootEnv) appConfig
    let ruleDepsFor = cveRuleDepsFor cveSyncPlan (deferredBreakerReporter deferredMetrics EffectfulRule) (katipFaultReporter logEnv)
    -- Where the plan shed the capability count (the nursery was the pressure),
    -- apply it in-process before the parallel machinery spins up.
    whenJust (mpShedCapabilities memoryPlan) setNumCapabilities
    serveAdmission <- newServeAdmission (mpAdmissionCapacity memoryPlan)
    -- One process-wide byte aggregate serves every publishing mount. It exists exactly when
    -- a publication target is configured, the same predicate the plan's tenant derives from.
    publishBudget <- forM (mpPublishTenant memoryPlan) $ \tenant -> do
        bodyBudget <- newByteAdmission (ptAggregateBytes tenant)
        pure PublishBudget{pbBodyBudget = bodyBudget, pbMaxRequestBytes = mpMaxRequestBytes memoryPlan}
    let limits =
            Limits
                { maxBodyBytes = mpMaxResponseBytes memoryPlan
                , maxVersionCount = limMaxVersionCount (cfgLimits appConfig)
                , maxNestingDepth = limMaxNestingDepth (cfgLimits appConfig)
                }
    bindings <- planMounts mountBindingFor getCurrentTime ruleDepsFor providers limits publishBudget config >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers config)
    heartbeat <- newWorkerHeartbeat
    let runsWorkerHere = spawnsWorker role mirrorRuntime
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    (queue, mirrorDrain) <- mirrorHandOff role logEnv deferredMetrics (mpQueueMemoryMaxDepth memoryPlan) mirrorRuntime
    metadataCache <- newMetadataCache (planCacheConfig (cfgCache appConfig) memoryPlan)

    -- The two managers stay split: public reads are anonymous and private reads forward the
    -- client's credential. Https-only egress closes the SSRF and resolve-to-internal class.
    publicSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    privateSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    manager <- newManager (connectionPoolSettings (bpPublicConnections bootPlan) publicSettings)
    privateManager <- newManager (connectionPoolSettings (bpPrivateConnections bootPlan) privateSettings)
    withEnvWithAdmission serveAdmission queue manager privateManager metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
        -- The instruments exist now, so installing them makes the credential provider's deferred
        -- reporters live for the rest of the run.
        installMetrics deferredMetrics (envMetrics builtEnv)
        registerAdvisoryAges builtEnv cveSyncPlan
        -- 'MirrorWith' always carries the artifact tenant.
        let workerArtifactMaxBytes = maybe mirrorArtifactBytesCap matMaxBytes (mpMirrorArtifactTenant memoryPlan)
        action
            ServiceRuntime
                { svcRunsWorker = runsWorkerHere
                , svcEnv = builtEnv
                , svcAppConfig = appConfig
                , svcBindings = bindings
                , svcWorkerPolicies = workerPoliciesFor builtEnv bindings publishTargets workerArtifactMaxBytes
                , svcMirrorDrain = superviseDrain builtEnv <$> mirrorDrain
                , svcSyncTasks = cveSyncTasks builtEnv (cveSyncScheduleFor appConfig) cveSyncPlan
                , svcCheckReady = cveSyncReady cveSyncPlan
                , svcCheckLive = workerLiveness runsWorkerHere heartbeat
                }

{- | The @\/livez@ arm a process answers from, given whether it runs the worker
('spawnsWorker'): the consume-loop heartbeat where it does, the listener alone where it does not.
-}
workerLiveness :: Bool -> WorkerHeartbeat -> IO Liveness
workerLiveness runningWorker heartbeat
    | runningWorker = heartbeatLivenessNow heartbeat
    | otherwise = pure alwaysLive

{- Build the role's view of the mirror queue and, for a producing role, its drain. A
worker-only role takes the backend directly, because nothing in that process enqueues. -}
mirrorHandOff :: MirrorRole -> LogEnv -> DeferredMetrics -> Int -> MirrorRuntimePlan -> IO (MirrorQueue, Maybe (IO ()))
mirrorHandOff role logEnv deferredMetrics memoryDepth = \case
    -- Under NoMirroring the inert queue is unreachable.
    NoMirroring -> pure (noMirrorQueue, Nothing)
    MirrorWith queuePlan -> do
        backendQueue <- buildMirrorQueue logEnv memoryDepth queuePlan
        if not (enqueuesJobs role)
            then pure (backendQueue, Nothing)
            else do
                -- The buffered hand-off keeps the serve path off the backend's enqueue latency.
                (queue, drainEnqueueBuffer) <-
                    bufferedMirrorHandOff (logBootWarning logEnv) (deferredMirrorEnqueueFailure deferredMetrics) backendQueue
                pure (queue, Just drainEnqueueBuffer)

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
callback reads the slot, which outlives the sync tasks, so the age survives a task restart. -}
registerAdvisoryAges :: Env -> Map.Map Ecosystem CveSyncHandle -> IO ()
registerAdvisoryAges builtEnv plan =
    for_ (Map.toList plan) $ \(eco, handle) ->
        registerAdvisoryDatabaseAge (envMetrics builtEnv) eco (generationInstalledAt (syncSlot (csEnv handle)))

-- One supervised sync task per configured ecosystem. Each flips its ecosystem's one-way
-- readiness flag once its first sync lands, and a restart resumes from the remote artifact.
cveSyncTasks :: Env -> SyncSchedule -> Map.Map Ecosystem CveSyncHandle -> [IO ()]
cveSyncTasks builtEnv schedule plan =
    [ void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "cve-sync" $
        superviseLoop
            (transientPolicy ("cve-sync[" <> show (syncEcosystem (csEnv handle)) <> "]") shellBackoff)
            (runCveSync syncMetrics syncTracing (csEnv handle) schedule (atomically (writeTVar (csReady handle) True)))
    | handle <- Map.elems plan
    ]
  where
    syncMetrics = advisorySyncMetricsPortOf (envMetrics builtEnv)
    syncTracing = advisorySyncTracingPortOf (envTelemetry builtEnv)

{- The pace every shell background loop retries a transient fault at: one second after the
first failure, doubling to a thirty-second ceiling. -}
shellBackoff :: BackoffSchedule
shellBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}

{- The enqueue-buffer drain under the shared supervision combinator. Pacing lives in the
buffer's own loop, so this wrapper only stops residue ending mirror-job delivery. -}
superviseDrain :: Env -> IO () -> IO ()
superviseDrain builtEnv drain =
    void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "mirror-enqueue-drain" $
        superviseLoop (transientPolicy "mirror-enqueue-drain" shellBackoff) (liftIO drain)

{- | Run the supervised mirror worker over the composition-root 'Env' and the per-ecosystem
bundles. The loop re-runs current policy against a job before it mirrors.
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
        , spBackoff = shellBackoff
        }
  where
    classify fault
        | Just (Unconfigured _) <- fromException fault = Permanent
        | otherwise = Transient

{- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that ecosystem
has no registered adapter. The path prefix derives from the ecosystem ('prefixFor'), never config.
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
