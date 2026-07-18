-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy role's effectful composition root.

'runProxy' receives validated process state from "Ecluse.Boot", resolves the
proxy-specific plans, builds the runtime-edge handles and mount bindings, and
coordinates the HTTP server with the optional mirror worker and advisory-sync
tasks. Pure plan derivation remains in "Ecluse.Composition" and its sibling
modules; this module is the boundary where those decisions become running
services.
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
import Ecluse.Composition.BootError (BootError (MemoryPlanOverrideUnsafe), renderBootError)
import Ecluse.Composition.Credential (initCredentialProviders)
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpAdmissionCapacity, mpDegradations, mpMaxRequestBytes, mpMaxResponseBytes, mpOverrideViolations, mpPublishTenant, mpQueueMemoryMaxDepth, mpShedCapabilities),
    PublishTenant (ptAggregateBytes),
    planCacheConfig,
 )
import Ecluse.Composition.MirrorQueue (MirrorRuntimePlan (MirrorWith, NoMirroring), planMirrorRuntime)
import Ecluse.Composition.Plan (resolveMemoryPlanFor)
import Ecluse.Composition.Sizing (connectionPoolSettings)
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Composition.Worker (workerPoliciesFor)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgRuntime, cfgServer),
    LimitsSettings (limMaxNestingDepth, limMaxVersionCount),
    RuntimeSettings (rtPrivateConnectionsPerHost, rtPublicConnectionsPerHost),
    ServerSettings (srvPort, srvShutdownDrainTimeout),
    mountPostureLines,
 )
import Ecluse.Core.Credential.Refresh (CredentialError (Unconfigured), CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem, prefixFor)
import Ecluse.Core.Queue (MirrorQueue, newEnqueueBuffer, noMirrorQueue, reportWorthy)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterEcosystem,
    adapterFor,
    adapterServe,
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
import Ecluse.Proxy.CveSync (CveSyncHandle (csEnv, csReady), cveRuleDepsFor, cveSyncReady, cveSyncScheduleFor, katipFaultReporter, planCveSync)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncEcosystem), SyncSchedule, runCveSync)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envMetrics, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Server (MountBinding (..), ServerConfig (scCheckLive, scCheckReady, scDrainTimeout, scOnException, scPort), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
import Ecluse.Runtime.Server qualified as Server
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredMirrorEnqueueFailure,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Runtime.Telemetry.Tracing (instrumentDataPlaneManagerSettings)

{- | Assemble and run the proxy role from an already validated 'BootEnv'.

Resolve the memory, mirroring, credential, mount, and advisory-sync plans, refusing
unsafe or incomplete wiring before opening the listener. On success, build the
data-plane managers, mirror queue, metadata cache, and runtime 'Env', then run the
HTTP server with the configured background services ('runServer' and 'runWorker').
-}
runProxy :: BootEnv -> IO ()
runProxy bootEnv = do
    let env = beConfig bootEnv
    let config = beConfigFull bootEnv
    let logEnv = beLogEnv bootEnv
    let telemetry = beTelemetry bootEnv

    -- The metric instruments do not exist until the telemetry substrate is built, well
    -- below; this deferred handle lets the credential provider (constructed here, at
    -- boot) record through reporters that stay inert until 'installMetrics' makes them
    -- live (in the 'withEnvWithAdmission' body). With telemetry off the eventual instruments are the
    -- no-op-meter ones, so the reporters are inert either way.
    deferredMetrics <- newDeferredMetrics
    let credentialReporters =
            CredentialReporters
                { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
                , crRefreshReporter = deferredRefreshReporter deferredMetrics CodeArtifact
                }
    -- Build the process-global mirror-target write provider(s) each mount's resolved
    -- credential names: the static token, or the CodeArtifact mint (derived from the
    -- mirror-target host, minting once eagerly so a misconfiguration fails loudly here
    -- at boot).
    providers <- initCredentialProviders credentialReporters config >>= orExit (T.unlines . map renderBootError)
    -- The advisory-database sync plan: with a bucket configured, every configured
    -- mount ecosystem gets its own slot (the shadow-swap read side), its own
    -- supervised sync task, and its own one-way first-sync readiness flag, each
    -- independent so one ecosystem's missing artifact never holds back
    -- another's. Without a bucket the map is empty: rules abstain and
    -- readiness is ungated.
    cveSyncPlan <- planCveSync logEnv (beAmbient bootEnv) env
    let ruleDepsFor = cveRuleDepsFor cveSyncPlan (deferredBreakerReporter deferredMetrics EffectfulRule) (katipFaultReporter logEnv)
    -- The effective admission capacity: explicit config, else computed from the
    -- effective (post-apply, observed) capability count, logged with its provenance
    -- beside the runtime lines. This bounds metadata materialisation only; the
    -- private manager's pool is sized independently below, since a trusted tarball
    -- hit streams outside admission (see 'Composition.resolvePrivateConnections'
    -- and issue #634).
    -- Whether a mirror runtime exists at all, and which queue backend it rides,
    -- decided from the URL's shape BEFORE any byte is budgeted: only the memory
    -- backend spends heap on queued jobs, so the queue tenant must be conditional
    -- on this selection, never allocated ahead of it.
    runtimePlan <-
        orExit (T.unlines . map renderBootError) (planMirrorRuntime (beAmbient bootEnv) config)
    -- The memory plan: the named tenants partitioned from the effective heap
    -- ceiling, with admission bounded jointly by CPU and the material share.
    -- Shed-ladder steps are loud warnings and the process boots regardless; only
    -- an explicit override breaking the combined invariant refuses.
    let (plan, planLines) = resolveMemoryPlanFor env (beRuntimePlan bootEnv) runtimePlan
    traverse_ (logBootInfo logEnv) planLines
    traverse_ (logBootWarning logEnv) (mpDegradations plan)
    unless (null (mpOverrideViolations plan)) $
        orExit (T.unlines . map renderBootError) (Left [MemoryPlanOverrideUnsafe (mpOverrideViolations plan)])
    -- Where the plan shed the capability count (the nursery was the pressure),
    -- apply it in-process before the parallel machinery spins up.
    whenJust (mpShedCapabilities plan) setNumCapabilities
    serveAdmission <- newServeAdmission (mpAdmissionCapacity plan)
    -- The publish-body byte discipline, present exactly when a publication
    -- target is configured (the plan's tenant derives from the same predicate):
    -- one process-wide aggregate shared by every publishing mount.
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
    -- The private-upstream connection pool: an explicit override, else computed from the
    -- process file-descriptor limit (the pool's real ceiling, since each pooled
    -- connection is one descriptor). Sized for the un-admitted private-hit streaming
    -- fan-out, not the admission capacity.
    fdLimit <- Composition.openFileSoftLimit
    let (privateConnections, privateConnectionsLine) = Composition.resolvePrivateConnections (rtPrivateConnectionsPerHost (cfgRuntime env)) fdLimit
    logBootInfo logEnv privateConnectionsLine
    -- The public pool: an explicit override, else computed from the same
    -- file-descriptor datapoint at half the private share. The onboarding
    -- fail-over's artifact streams and the worker's back-fill fetches ride this
    -- manager without coalescing, so its retention must cover that transient
    -- fan-out, not only the admission-bounded metadata misses.
    let (publicConnections, publicConnectionsLine) = Composition.resolvePublicConnections (rtPublicConnectionsPerHost (cfgRuntime env)) fdLimit
    logBootInfo logEnv publicConnectionsLine
    heartbeat <- newWorkerHeartbeat
    let serverConfig =
            (mkServerConfig bindings)
                { scPort = srvPort (cfgServer env)
                , scDrainTimeout = ShutdownDrainTimeout (srvShutdownDrainTimeout (cfgServer env))
                , scCheckReady = cveSyncReady cveSyncPlan
                , -- Fold the worker heartbeat into /livez exactly when a worker will
                  -- run; a serve-only deployment's liveness is the listener alone.
                  scCheckLive = case runtimePlan of
                    MirrorWith _ -> heartbeatHealthyNow heartbeat
                    NoMirroring -> pure True
                , scOnException = warpExceptionHook logEnv
                }
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    -- One posture line per mount: mirrored (and where the back-fill lands) or
    -- serve-only. The mode is derived from the declared endpoints, so this is the
    -- loud surface a dropped mirrorTarget shows up on.
    traverse_ (logBootInfo logEnv) (mountPostureLines config)
    -- The mirror runtime's queue, or nothing at all. Under MirrorWith the
    -- config-selected backend is built once here (the single constructor call) --
    -- the durable AWS SQS backend, or the bounded in-memory backend, which first
    -- emits a loud boot warning (it is non-durable / best-effort) -- and wrapped in
    -- the buffered hand-off that decouples the serve path from the backend's own
    -- enqueue latency (the drain loop below, raced against the services, delivers
    -- off the request path). Under NoMirroring nothing is built: Env carries the
    -- inert queue, unreachable by construction (no mount enqueues, no worker polls).
    (queue, mirrorDrain) <- case runtimePlan of
        MirrorWith queuePlan -> do
            backendQueue <- buildMirrorQueue logEnv (mpQueueMemoryMaxDepth plan) queuePlan
            (q, drainEnqueueBuffer) <-
                bufferedMirrorHandOff (logBootWarning logEnv) (deferredMirrorEnqueueFailure deferredMetrics) backendQueue
            pure (q, Just drainEnqueueBuffer)
        NoMirroring -> do
            logBootInfo logEnv "mirror runtime disabled: no mount mirrors, so no queue is built and no worker starts"
            pure (noMirrorQueue, Nothing)
    metadataCache <- newMetadataCache (planCacheConfig (cfgCache env) plan)

    -- Two data-plane managers, one per origin. Both are the standard validating TLS
    -- manager: registry egress is https-only by construction (a non-https endpoint
    -- fails closed at boot), and certificate validation authenticates the dialled
    -- host, so a rebound or internal address cannot present a CA-trusted certificate
    -- for the requested name (the SSRF / resolve-to-internal class is closed by
    -- certificate validation). The split is retained
    -- because the two origins differ in credential handling (the public reads are
    -- anonymous; the private reads forward the client's credential) and in the
    -- @dist.tarball@ host gate's trust. Both are built inside the telemetry bracket so
    -- that, with telemetry enabled, each carries the http-client instrumentation
    -- (child spans + W3C context propagation) hung off the substrate's installed
    -- providers; with it off the instrumentation step is the identity.
    publicSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    privateSettings <- instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
    manager <- newManager (connectionPoolSettings publicConnections publicSettings)
    privateManager <- newManager (connectionPoolSettings privateConnections privateSettings)
    withEnvWithAdmission serveAdmission queue manager privateManager metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
        -- The instruments now exist (built in 'withEnvWithAdmission' from the telemetry handle);
        -- install them so the credential provider's deferred reporters go live for
        -- the rest of the run. They are the no-op-meter instruments when telemetry
        -- is off, so this is inert in that posture.
        installMetrics deferredMetrics (envMetrics builtEnv)
        -- The enqueue-buffer drain loop and the advisory sync tasks never return,
        -- so they are raced against the services: when the services finish
        -- (shutdown), the race cancels them. A dropped buffered job is the queue's
        -- safe loss (re-enqueued on the next demand), and a cancelled sync simply
        -- resumes from the remote artifact on next boot, so neither holds up
        -- shutdown. Each sync task runs its boot burst immediately, so a healthy
        -- deployment is rules-engine complete within seconds of boot.
        let syncTasks = cveSyncTasks builtEnv (cveSyncScheduleFor env) cveSyncPlan
        -- The worker's per-ecosystem bundles: one reusable construction
        -- ('Ecluse.Composition.Worker.workerPoliciesFor') over the served mounts,
        -- the resolved publish targets, and the adapter registry, so a future
        -- worker-only binary reuses the same function rather than re-deriving
        -- this wiring. With no mirror runtime there is no worker arm and no drain
        -- loop, only the server (and the sync tasks when a bucket is configured;
        -- racing the server against an EMPTY task list would cancel it instantly,
        -- so the no-task shape runs the server alone).
        case mirrorDrain of
            Just drainEnqueueBuffer ->
                race_
                    (runServices serverConfig (workerPoliciesFor builtEnv bindings publishTargets) builtEnv)
                    (concurrently_ (superviseDrain builtEnv drainEnqueueBuffer) (mapConcurrently_ id syncTasks))
            Nothing
                | null syncTasks -> runServer serverConfig builtEnv
                | otherwise ->
                    race_
                        (runServer serverConfig builtEnv)
                        (mapConcurrently_ id syncTasks)

{- The buffered hand-off in front of the mirror queue's backend. Drops and delivery
failures are logged rate-limited ('enqueueReportWorthy') and each counts an enqueue
failure; both are safe, since a lost job is re-enqueued on the next demand for its
artifact. -}
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

{- Report-worthy event counts for the enqueue-buffer warnings: the first, then every
'Composition.mirrorEnqueueReportInterval'-th, mirroring the bounded memory queue's
rate-limited drop reporting. The metric alongside counts every event; only the log
line is rate-limited. -}
enqueueReportWorthy :: Int -> Bool
enqueueReportWorthy n = reportWorthy n Composition.mirrorEnqueueReportInterval

-- One advisory sync task per configured ecosystem: each runs under the boot log's
-- "cve-sync" namespace, supervised by the shared combinator (residue restarts the
-- task, which simply resumes from the remote artifact), and flips its ecosystem's
-- one-way readiness flag once its first sync lands.
cveSyncTasks :: Env -> SyncSchedule -> Map.Map Ecosystem CveSyncHandle -> [IO ()]
cveSyncTasks builtEnv schedule plan =
    [ void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "cve-sync" $
        superviseLoop
            (transientPolicy ("cve-sync[" <> show (syncEcosystem (csEnv handle)) <> "]"))
            (runCveSync (csEnv handle) schedule (atomically (writeTVar (csReady handle) True)))
    | handle <- Map.elems plan
    ]

{- A policy for the shell's background loops with no wiring fault to fail up on:
every synchronous escape is residue, retried from one second towards a
30-second cap. -}
transientPolicy :: Text -> SupervisionPolicy
transientPolicy label =
    SupervisionPolicy
        { spLabel = label
        , spClassify = const Transient
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}
        }

{- The enqueue-buffer drain under the shared supervision combinator: its
per-delivery pacing over the typed fault channel lives inside the buffer's own
drain loop ('Ecluse.Core.Queue.newEnqueueBuffer'); this wrapper contains only
residue, so one contract escape cannot silently end mirror-job delivery for the
rest of the run. -}
superviseDrain :: Env -> IO () -> IO ()
superviseDrain builtEnv drain =
    void . runKatipContextT (envLogEnv builtEnv) (mempty :: SimpleLogPayload) "mirror-enqueue-drain" $
        superviseLoop (transientPolicy "mirror-enqueue-drain") (liftIO drain)

{- Run the server and the mirror worker over one composition-root 'Env', the shape
the single-process program uses. The two are independent (each depends only on the
handles in 'Env', not on each other), so splitting into separate binaries later is
two thin entry points calling 'runServer' \/ 'runWorker' -- no rearchitecting. The
server's settings (its derived mount bindings and port) are supplied by the
composition root and threaded to 'runServer'.

The server is raced against the worker through
'Ecluse.Runtime.Server.raceServerAgainstLoop' (see there for the shutdown invariant):
the worker loop never returns, so the server's graceful return must cancel it rather
than a 'concurrently_' wedging on it forever.
-}
runServices :: ServerConfig -> WorkerPolicies -> Env -> IO ()
runServices serverConfig policies env =
    Server.raceServerAgainstLoop (runServer serverConfig env) (runWorker policies env)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the
config-derived 'ServerConfig'.

The mount wiring behind the served bindings comes from the ecosystem adapter
registry: 'mountBindingFor' resolves each configured ecosystem through
'Ecluse.Core.Registry.Adapter.adapterFor' and projects the resolved adapter's serve
surface into the otherwise ecosystem-neutral web layer
('Ecluse.Runtime.Server.runServer'), so the agnostic server stays closed over the
shared 'Ecluse.Core.Server.Route.Route' set. Splitting the server into its own
binary later reuses this same entry.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env = Server.runWarp cfg (`Server.tracedApplication` env)

{- Warp's exception hook over the process logger: a post-commit teardown the
request perimeter rethrew, or a fault in warp's own connection handling, logged
structured at 'ErrorS' with the request path when one is known.
'Warp.defaultShouldDisplayException' filters the routine client-disconnect noise,
so an aborted download does not spam the log. -}
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
ecosystem has no registered adapter. The adapter registry
('Ecluse.Core.Registry.Adapter.adapterFor') answers which ecosystems this build
supports; the resolved adapter's serve surface supplies the router (the
'Ecluse.Core.Server.Context.MountRouter'), and the path prefix is __derived__
from the ecosystem ('prefixFor') rather than configured, so the ecosystem is the
single thing that drives the binding (see
@docs\/architecture\/web-layer.md@ → "Multi-ecosystem mounts"). The composition
root supplies the packument-serve dependencies once the per-mount registry set is
resolved.

An ecosystem with no registered adapter resolves to 'Nothing': a loud miss at the
call site rather than a silently half-wired mount.
-}
mountBindingFor :: Ecosystem -> PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding
mountBindingFor eco packumentDeps publishDeps =
    adapterFor eco <&> \adapter -> mountOf adapter packumentDeps publishDeps

{- The mount projection of one adapter: the ecosystem's serve router under its derived
prefix, paired with the
packument-serve and first-party publish dependencies the composition root supplies
('Nothing' publish deps leave a @PUT \/{pkg}@ the @405@ opt-out: no publication
target).
-}
mountOf :: RegistryAdapter -> PackumentDeps -> Maybe PublishDeps -> MountBinding
mountOf adapter packumentDeps publishDeps =
    MountBinding
        { bindingPrefix = prefixFor (adapterEcosystem adapter)
        , bindingRouter = serveRouter (adapterServe adapter)
        , bindingPackumentDeps = packumentDeps
        , bindingPublishDeps = publishDeps
        }

{- | Run the supervised mirror worker over the composition-root 'Env' and the
per-ecosystem bundles: the
consume → probe → re-evaluate → fetch → verify → publish →
ack loop against the queue, in
the worker monad ('Ecluse.Core.Worker.WorkerM') over the worker runtime
('Ecluse.Runtime.Env.workerRuntimeOf'). The bundles carry the same prepared rules,
artifact request formation, and public origin the serve path gates with, plus each
mount's married mirror-write capability, so the worker re-runs current policy
against a job before mirroring it and publishes through the job ecosystem's own
protocol and target.

This is the composition-root __hoist point__: it resolves the request-independent @dd@
correlation object (the service identity; no span is active at the worker entry) and
installs it as the worker's initial @katip@ context, then discharges the loop to 'IO'
through 'Ecluse.Core.Worker.runWorkerM', the worker analogue of the serve path's
'Ecluse.Core.Server.Context.runHandler' boundary. The loop logic lives in
"Ecluse.Core.Worker"; the single-process program runs this alongside 'runServer'.
-}
runWorker :: WorkerPolicies -> Env -> IO ()
runWorker policies env = do
    dd <- ddPayloadNow (envDdContext env)
    void (runWorkerM (envLogEnv env) dd (workerRuntimeOf policies env) (katipAddNamespace "worker" (workerLoop workerSupervision)))

{- The worker's supervision policy: residue is transient (logged, retried with a
bounded exponential backoff from one second), except the wiring fault no retry
can fix -- an unconfigured credential leaf reached at runtime -- which fails up
through the services race and takes the
process down, so the orchestrator restarts it against corrected configuration
instead of the loop retrying a permanently-broken wiring forever. -}
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
