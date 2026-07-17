-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Écluse: a supply-chain policy proxy for package registries.

Écluse (package @ecluse@) sits between consumers (developers, CI) and a package
registry, applying a configurable resilience policy before any dependency reaches
a build, without hosting packages itself. The name is French for a canal lock: a
chamber whose gates never open at once. Every dependency is held and cleared
through that controlled passage before it is admitted to a build.

The goal is __resilience, not malware detection__: shrink the blast radius of a
bad publish (a hijacked maintainer account, a race-to-publish, a typosquat)
rather than promise to recognise malice. Écluse is __not a registry__: storage is
delegated to whatever backend the operator runs (AWS CodeArtifact, GCP Artifact
Registry), and Écluse only governs what may be fetched from, and mirrored to,
those backends. npm is the first ecosystem; the domain model is ecosystem-agnostic
so PyPI and RubyGems can follow.

== How a request is cleared

Écluse speaks a registry's native protocol across three read-path registries (the
client's, a /private upstream/ of already-vetted packages, and the /public/
registry), and the two request shapes use them differently:

* A __tarball__ request is gated for that one version: a private-upstream hit is
  streamed unfiltered (already vetted); on a miss, the proxy fetches the
  version's public metadata, evaluates the rules, and either streams it from
  public __and enqueues an asynchronous mirror job__ or returns a denial.
* A __packument__ (metadata) request is a /merge/: the private and public
  upstreams are fetched in parallel, public versions are filtered by the rules
  while private versions are trusted, and the two are combined into one document
  (private wins a version collision, an integrity divergence is flagged as a
  supply-chain signal, and @latest@ is repointed to the newest survivor).

Two properties run through both shapes: the rules engine is __deny by default__ (a
version is admitted only if some rule allows it and none denies it), and
__mirroring is demand-driven__, so only versions actually pulled are mirrored,
never on the request's critical path.

== How the code is organised

Écluse is a __functional core with effects at the edges__: the policy and
protocol logic is pure and trivially testable, and @IO@ is confined to a thin
shell. Swappable backends sit behind /handles/ (records of functions chosen at a
single composition root), so a new cloud or a new ecosystem is an added
implementation behind an existing handle, not a structural change.

The library's vocabulary, roughly from the pure core outward:

* __Domain model__: "Ecluse.Core.Package" (the ecosystem-agnostic package vocabulary
  the rules reason over), "Ecluse.Core.Version" (version identity and per-ecosystem
  ordering), and "Ecluse.Core.Ecosystem" (the ecosystem tag the rest dispatches on).
* __Policy__: "Ecluse.Core.Rules" (deny-by-default evaluation) over the rule types
  in "Ecluse.Core.Rules.Types".
* __Protocol boundary__: "Ecluse.Core.Registry" (the shared registry-protocol
  vocabulary), "Ecluse.Core.Registry.Adapter" (the ecosystem adapter registry this
  composition root projects each mount's, publish target's, and worker's ecosystem
  wiring from), "Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" (the lenient npm
  wire decoders and their projection onto the domain model),
  "Ecluse.Core.Registry.Npm.Route" (the npm path grammar), and "Ecluse.Core.Server.Route"
  (the shared serve-action 'Route' set and the injected route classifier).
* __Cloud handles__: "Ecluse.Core.Credential" (minting the mirror-target write token)
  and "Ecluse.Core.Queue" (the durable mirror-job hand-off to the worker).
* __Mirror worker__: "Ecluse.Core.Worker" (the supervised consume loop that fetches,
  verifies against the job's integrity digest, and publishes an approved artifact).

'run' is the entry point the @ecluse@ executable invokes (see "Main"). It lives
in the library, not in @app\/Main.hs@, so the composition root is a single
importable unit and @app\/Main.hs@ stays a thin shell that only calls it.

== Further reading

@docs\/architecture.md@ is the systems-design index: the vision, the end-to-end
request lifecycle, and a map to the per-concern design documents. @CONTRIBUTING.md@
covers the codebase layout and testing strategy, and @STYLE.md@ the coding and
documentation conventions.
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
import Katip (LogEnv, Severity (ErrorS), SimpleLogPayload, katipAddContext, katipAddNamespace, logFM, runKatipContextT, sl)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (concurrently_, race_)
import UnliftIO.Async (mapConcurrently_)

import Ecluse.Boot
import Ecluse.Composition (
    planMounts,
    planPublishTargets,
 )
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Credential (initCredentialProviders)
import Ecluse.Composition.MemoryBudget (
    MemoryBudget (mbMaxRequestBytes, mbMaxResponseBytes, mbQueueMemoryMaxDepth),
    budgetCacheConfig,
    resolveMemoryBudget,
 )
import Ecluse.Composition.MirrorQueue (MirrorRuntimePlan (MirrorWith, NoMirroring), planMirrorRuntime)
import Ecluse.Composition.Sizing (connectionPoolSettings)
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Composition.Worker (workerPoliciesFor)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgQueue, cfgRuntime, cfgServer),
    LimitsSettings (limMaxNestingDepth, limMaxVersionCount),
    RuntimeSettings (rtPrivateConnectionsPerHost, rtPublicConnectionsPerHost, rtServeMaxInFlight),
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
import Ecluse.Rts (effectiveCapabilities)
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncEcosystem), SyncSchedule, runCveSync)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envMetrics, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Server (MountBinding (..), RequestSizeLimit (RequestSizeLimit), ServerConfig (scCheckLive, scCheckReady, scDrainTimeout, scOnException, scPort, scSizeLimit), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
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

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

Assemble the composition root from configuration. Parse the environment layer and
the optional config document, __validate everything and fail fast at boot__ on any
problem (a malformed env, an unresolved rule policy, a configured mount with no
adapter, a credential reference that does not resolve, or a mirror-queue backend
not built in this binary), aggregating the failures so a single run reports them
all. On success, build the handles (the shared HTTP @Manager@, the config-selected
mirror queue, the metadata cache, the logger, the process-global credential
provider, and the telemetry substrate, off unless @ECLUSE_OBSERVABILITY__TELEMETRY@ enables it)
into an 'Env', derive the served mount bindings, then run the server and the mirror
worker __concurrently__ over that single 'Env' ('runServer' and 'runWorker').
Bracketing the 'Env' (and the telemetry providers) for the lifetime of both tears
down their shared resources along every exit path.
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
    let (capabilities, _capsProvenance) = effectiveCapabilities (beRuntimePlan bootEnv)
        (serveMaxInFlight, admissionLine) = Composition.resolveServeAdmission (rtServeMaxInFlight (cfgRuntime env)) capabilities
    logBootInfo logEnv admissionLine
    serveAdmission <- newServeAdmission serveMaxInFlight
    -- The memory budget: every byte-valued bound resolved as a share of the heap
    -- ceiling the runtime posture found (or its shipped fallback), each with its
    -- provenance line. The admission capacity above is its working-space divisor.
    let (budget, budgetLines) =
            resolveMemoryBudget (cfgCache env) (cfgLimits env) (cfgQueue env) (beRuntimePlan bootEnv) serveMaxInFlight
    traverse_ (logBootInfo logEnv) budgetLines
    let limits =
            Limits
                { maxBodyBytes = mbMaxResponseBytes budget
                , maxVersionCount = limMaxVersionCount (cfgLimits env)
                , maxNestingDepth = limMaxNestingDepth (cfgLimits env)
                }
    bindings <- planMounts mountBindingFor getCurrentTime ruleDepsFor providers limits config >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers config)
    -- Whether a mirror runtime exists at all, then which queue backend it rides:
    -- zero mirroring mounts is a serve-only deployment (no queue, no worker; the
    -- queue configuration is not consulted), and with any mirroring mount the
    -- backend selection applies exactly as before (the GCP arm is a fail-loud
    -- "not built" boot error, never a silent fall-through).
    runtimePlan <-
        orExit (T.unlines . map renderBootError) (planMirrorRuntime (beAmbient bootEnv) (mbQueueMemoryMaxDepth budget) config)
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
                , -- The request-body cap, resolved by the memory budget (configured
                  -- or a share of the heap ceiling).
                  scSizeLimit = RequestSizeLimit (fromIntegral (mbMaxRequestBytes budget))
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
            backendQueue <- buildMirrorQueue logEnv queuePlan
            (q, drainEnqueueBuffer) <-
                bufferedMirrorHandOff (logBootWarning logEnv) (deferredMirrorEnqueueFailure deferredMetrics) backendQueue
            pure (q, Just drainEnqueueBuffer)
        NoMirroring -> do
            logBootInfo logEnv "mirror runtime disabled: no mount mirrors, so no queue is built and no worker starts"
            pure (noMirrorQueue, Nothing)
    metadataCache <- newMetadataCache (budgetCacheConfig (cfgCache env) budget)

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

They are 'race_'d, not 'concurrently_'d, and the choice is a shutdown invariant. The
worker loop never returns, so a 'concurrently_' would keep waiting on it after the
server has gracefully drained and returned, which in turn wedges the composition
root's outer 'race_' and leaves the telemetry and 'Env' brackets un-unwound: no
flush, the process hanging until a second signal or the orchestrator's kill. 'race_'
lets the server's graceful return cancel the worker and unwind those brackets (flush
and exit cleanly), while a fault thrown by either side still propagates ('race_'
re-raises it) so a genuine failure fails the process up rather than being swallowed.
-}
runServices :: ServerConfig -> WorkerPolicies -> Env -> IO ()
runServices serverConfig policies env =
    race_ (runServer serverConfig env) (runWorker policies env)

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
runServer cfg env = Server.runWarp cfg (Server.tracedApplication cfg env)

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
