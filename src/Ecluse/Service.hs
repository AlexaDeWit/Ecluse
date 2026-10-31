-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared runtime assembly for proxy and mirror-worker roles from a cleared 'ExecutablePlan'.
"Ecluse.Proxy" adds the front door. "Ecluse.Mirror" runs the same worker alone.
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

import Katip (LogEnv, katipAddNamespace)

import Ecluse.Boot (BootEnv (beDeadlines, beLogEnv, beTelemetry), logBootWarning)
import Ecluse.Composition (BootWiring (bwBindings, bwPublishTargets))
import Ecluse.Composition.Executable (
    ExecutablePlan (epBootPlan),
    MirrorWiring (mwBootWiring, mwCveSync, mwDeferredMetrics, mwQueue, mwRole),
 )
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpAdmissionCapacity, mpMirrorArtifactTenant, mpTransientBudget),
    MirrorArtifactTenant (matMaxBytes),
    TransientBudget (tbBootBytes),
    mirrorArtifactBytesCap,
 )
import Ecluse.Composition.MemoryPlan.Transient (brakeBounds, meterStepBytes)
import Ecluse.Composition.MirrorQueue (MirrorRuntimePlan (MirrorWith, NoMirroring))
import Ecluse.Composition.MirrorRole (enqueuesJobs, spawnsWorker)
import Ecluse.Composition.MirrorState (bindMirrorStates, newMirrorStates)
import Ecluse.Composition.Plan (
    BootPlan (bpCacheConfig, bpLimits, bpMemoryPlan, bpMirrorRuntime, bpS3Endpoint, bpValidated),
 )
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Composition.Types (MirrorRole)
import Ecluse.Composition.Validate (ValidatedPlan (vpMounts, vpSettings))
import Ecluse.Composition.Worker (workerPoliciesWithState)
import Ecluse.Config (AppConfig)
import Ecluse.Core.Credential.Refresh (CredentialError (Unconfigured))
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem, prefixFor)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Queue.Buffer (newEnqueueBuffer, reportWorthy)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterEcosystem,
    adapterFor,
    adapterServe,
    serveCredential,
    serveRouter,
 )
import Ecluse.Core.Server.Admission (newServeAdmission, serveAdmissionOverflow)
import Ecluse.Core.Server.Admission.Brake (defaultBrakeMarks)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSettings (..), meterSnapshot, newMemoryMeter)
import Ecluse.Core.Server.Admission.Overflow (overflowWaiting)
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxBytes, cacheMaxEntries), newMetadataCacheWithProvider)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Provider (localCacheProviderWithPool)
import Ecluse.Core.Server.Context (PackumentDeps, PublishDeps)
import Ecluse.Core.Server.Readiness (Readiness)
import Ecluse.Core.Supervision (
    FaultDisposition (Permanent, Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    backgroundLoopBackoff,
    superviseBackground,
 )
import Ecluse.Core.Worker (Liveness, WorkerHeartbeat, WorkerPolicies, alwaysLive, heartbeatLivenessNow, runWorkerM, workerLoop)
import Ecluse.Cve.Sync (cveSyncReadiness, cveSyncScheduleFor, cveSyncTasks, registerAdvisoryAges)
import Ecluse.Rts.Cgroup (cgroupMemoryUse)
import Ecluse.Rts.Sampler (SamplerSettings (..), readCollector, runMemorySampler, samplerPeriodMicros, shareWindowPeriods)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envMetrics, envTelemetry, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Server (MountBinding (..))
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (registerMemoryMeter, registerOverflowQueue)
import Ecluse.Runtime.Telemetry.Reporters (
    DeferredMetrics,
    deferredMirrorEnqueueFailure,
    installMetrics,
 )
import Ecluse.Service.Internal (dataPlaneManagers)

-- | Everything a role needs to start its own tasks, built once by 'withServiceRuntime'. The background tasks arrive already wrapped in their supervision policy.
data ServiceRuntime = ServiceRuntime
    { svcRole :: MirrorRole
    -- ^ The mirror-pipeline half this runtime serves, taken from the boot plan.
    , svcRunsWorker :: Bool
    -- ^ Whether this process runs the mirror worker ('spawnsWorker'), the one fact both the spawn decision and the @\/livez@ arm derive from.
    , svcEnv :: Env
    , svcAppConfig :: AppConfig
    , svcBindings :: [MountBinding]
    -- ^ The resolved mounts. A worker-only role builds them for their rules, and serves none.
    , svcWorkerPolicies :: WorkerPolicies
    , svcMirrorDrain :: Maybe (IO ())
    -- ^ The supervised enqueue-buffer drain, present when this role produces jobs into a queue.
    , svcSyncTasks :: [IO ()]
    -- ^ One supervised advisory-sync task per configured ecosystem.
    , svcMemorySampler :: IO ()
    -- ^ The supervised memory sampler that steers the transient budget. It never returns.
    , svcCheckReady :: IO Readiness
    , svcCheckLive :: IO Liveness
    }

-- | Assemble the role's runtime and run @action@ within it. The plan it takes is post-gating, so this only builds and allocates: nothing here can refuse the boot.
withServiceRuntime :: BootEnv -> ExecutablePlan -> MirrorWiring -> (ServiceRuntime -> IO ()) -> IO ()
withServiceRuntime bootEnv plan mirror action = do
    let logEnv = beLogEnv bootEnv
        telemetry = beTelemetry bootEnv
        deadlines = beDeadlines bootEnv
        bootPlan = epBootPlan plan
        role = mwRole mirror
        appConfig = vpSettings (bpValidated bootPlan)
        mirrorRuntime = bpMirrorRuntime bootPlan
        memoryPlan = bpMemoryPlan bootPlan
        deferredMetrics = mwDeferredMetrics mirror
        cveSyncPlan = mwCveSync mirror
        baseBindings = bwBindings (mwBootWiring mirror)

    -- Only the boot turns the overflow queue on, with the sweeper it supervises. A test-built runtime refuses a full room at once.
    serveAdmission <- newServeAdmission deadlines (mpAdmissionCapacity memoryPlan)
    (memoryMeter, sampler) <- memoryAdmission deadlines memoryPlan
    heartbeat <- newWorkerHeartbeat
    let runsWorkerHere = spawnsWorker role mirrorRuntime
    (queue, mirrorDrain) <- mirrorHandOff role logEnv deferredMetrics mirrorRuntime (mwQueue mirror)
    let cacheConfig = bpCacheConfig bootPlan
    pool <- newLocalPool (cacheMaxEntries cacheConfig) (cacheMaxBytes cacheConfig)
    metadataCache <- localCacheProviderWithPool pool cacheConfig >>= newMetadataCacheWithProvider
    states <- newMirrorStates (bpS3Endpoint bootPlan) deadlines appConfig (vpMounts (bpValidated bootPlan)) (bpLimits bootPlan) cacheConfig pool
    let bindings = bindMirrorStates states baseBindings

    (manager, privateManager) <- dataPlaneManagers bootPlan
    withEnvWithAdmission serveAdmission memoryMeter queue manager privateManager deadlines metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
        -- The instruments exist now, so installing them makes the credential provider's deferred
        -- reporters live for the rest of the run.
        installMetrics deferredMetrics (envMetrics builtEnv)
        registerMemoryMeter (envMetrics builtEnv) (meterSnapshot memoryMeter)
        traverse_ (registerOverflowQueue (envMetrics builtEnv) . overflowWaiting) (serveAdmissionOverflow serveAdmission)
        registerAdvisoryAges (envMetrics builtEnv) cveSyncPlan
        -- 'MirrorWith' always carries the artifact tenant.
        let workerArtifactMaxBytes = maybe mirrorArtifactBytesCap matMaxBytes (mpMirrorArtifactTenant memoryPlan)
        action
            ServiceRuntime
                { svcRole = role
                , svcRunsWorker = runsWorkerHere
                , svcEnv = builtEnv
                , svcAppConfig = appConfig
                , svcBindings = bindings
                , svcWorkerPolicies = workerPoliciesWithState builtEnv states bindings (bwPublishTargets (mwBootWiring mirror)) workerArtifactMaxBytes
                , svcMirrorDrain = superviseBackground "mirror-enqueue-drain" (envLogEnv builtEnv) <$> mirrorDrain
                , svcSyncTasks =
                    cveSyncTasks
                        (envLogEnv builtEnv)
                        (envMetrics builtEnv)
                        (envTelemetry builtEnv)
                        (cveSyncScheduleFor appConfig)
                        cveSyncPlan
                , svcMemorySampler = superviseBackground "memory-sampler" (envLogEnv builtEnv) sampler
                , svcCheckReady = cveSyncReadiness cveSyncPlan
                , svcCheckLive = workerLiveness runsWorkerHere heartbeat
                }

-- The metadata memory meter and the sampler that steers it, both sized from the memory plan.
memoryAdmission :: Deadlines -> MemoryPlan -> IO (MemoryMeter, IO ())
memoryAdmission deadlines memoryPlan = do
    meter <-
        newMemoryMeter
            deadlines
            MeterSettings
                { msBudgetBytes = tbBootBytes (mpTransientBudget memoryPlan)
                , msStepBytes = meterStepBytes
                , msEntryRoom = mpAdmissionCapacity memoryPlan
                , msEntryWaitMicros = admissionWaitMicros
                }
    collector <- readCollector
    kernelUse <- cgroupMemoryUse
    pure
        ( meter
        , runMemorySampler
            SamplerSettings
                { ssMeter = meter
                , ssMarks = defaultBrakeMarks
                , ssBounds = brakeBounds (mpTransientBudget memoryPlan)
                , ssCollector = collector
                , ssKernelPermille = kernelUse
                , ssPeriodMicros = samplerPeriodMicros
                , ssWindowPeriods = shareWindowPeriods
                }
        )

-- | The @\/livez@ arm a process answers from, given whether it runs the worker ('spawnsWorker'): the consume-loop heartbeat where it does, the listener alone where it does not.
workerLiveness :: Bool -> WorkerHeartbeat -> IO Liveness
workerLiveness runningWorker heartbeat
    | runningWorker = heartbeatLivenessNow heartbeat
    | otherwise = pure alwaysLive

{- Build the role's view of the queue the boot already built, and, for a producing role, its
drain. A worker-only role takes the backend directly, because nothing in that process enqueues. -}
mirrorHandOff :: MirrorRole -> LogEnv -> DeferredMetrics -> MirrorRuntimePlan -> MirrorQueue -> IO (MirrorQueue, Maybe (IO ()))
mirrorHandOff role logEnv deferredMetrics mirrorRuntime backendQueue = case mirrorRuntime of
    -- Under NoMirroring the inert queue is unreachable.
    NoMirroring -> pure (backendQueue, Nothing)
    MirrorWith _
        | not (enqueuesJobs role) -> pure (backendQueue, Nothing)
        | otherwise -> do
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

-- | Run the supervised mirror worker over the composition-root 'Env' and the per-ecosystem bundles. The loop re-runs current policy against a job before it mirrors.
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
        , spBackoff = backgroundLoopBackoff
        }
  where
    classify fault
        | Just (Unconfigured _) <- fromException fault = Permanent
        | otherwise = Transient

-- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that ecosystem has no registered adapter. The path prefix derives from the ecosystem ('prefixFor'), never config.
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
