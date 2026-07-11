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
* __Protocol boundary__: "Ecluse.Core.Registry" (the registry-protocol handle),
  "Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" (the lenient npm
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
    npmServerConfig,
    mountBindingFor,
    unconfiguredRegistry,
    planCveSync,
    CveSyncHandle (..),
    cveRuleDepsFor,
    cveSyncReady,
    cveSyncScheduleFor,
) where

import Amazonka qualified as AWS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Katip (LogEnv, Severity (ErrorS), SimpleLogPayload, katipAddContext, katipAddNamespace, logFM, runKatipContextT, sl)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, (</>))
import UnliftIO (concurrently_, race_, throwIO)
import UnliftIO.Async (mapConcurrently_)
import UnliftIO.Exception (catchAny)

import Ecluse.Boot
import Ecluse.Composition (
    PublishTarget (ptCredentials, ptEcosystem, ptMirrorUrl),
    planMounts,
    planPublishTargets,
 )
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Credential (initCredentialProviders)
import Ecluse.Composition.MirrorQueue (planMirrorQueue)
import Ecluse.Composition.MirrorQueue qualified as Composition
import Ecluse.Composition.Sizing (connectionPoolSettings)
import Ecluse.Composition.Sizing qualified as Composition
import Ecluse.Config (
    AppConfig (cfgAwsEndpointUrl, cfgCveDbPollInterval, cfgMaxOsvDbBytes, cfgMounts, cfgOsvDataDir, cfgPort, cfgPrivateConnectionsPerHost, cfgPublicConnectionsPerHost, cfgServeMaxInFlight, cfgShutdownDrainTimeout, cfgVulnerabilityDatabaseBucket),
 )
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Credential (AuthToken (..), currentToken)
import Ecluse.Core.Credential.Refresh (CredentialError (Unconfigured), CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Cve.Slot (CveSlot, currentAdvisoryEtag, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName, parseEcosystem, prefixFor)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Core.Queue (MirrorQueue, newEnqueueBuffer, reportWorthy)
import Ecluse.Core.Registry (
    ParseError (..),
    RegistryClient (..),
    RegistryUnconfigured (RegistryUnconfigured),
 )
import Ecluse.Core.Registry.Metadata (fetchVersionDetails)
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmPublishClient)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (RuleDeps (..))
import Ecluse.Core.Security (Origin (UntrustedOrigin), defaultLimits, thgPublicHost)
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Cache (Source (Source), newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps, PublishDeps, pdLimits, pdMinIntegrity, pdNewMetadataClient, pdNow, pdPublicBaseUrl, pdRules, pdTarballHostGate, tarballHostHonoured)
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached))
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Permanent, Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
 )
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule), Provider (CodeArtifact), Upstream (Public))
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (..), runWorkerM, workerLoop)
import Ecluse.Runtime.Cve.Sync (SyncEnv (..), SyncSchedule (SyncSchedule, schedBootBackoff, schedPollDelay), bootBackoffDelays, runCveSync, s3CveFetch)
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envManager, envMetadataCache, envMetrics, envTelemetry, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Runtime.Pilot.Export (buildS3Env)
import Ecluse.Runtime.Server (MountBinding (..), ServerConfig (scCheckReady, scDrainTimeout, scOnException, scPort), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
import Ecluse.Runtime.Server qualified as Server
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Instruments (metricsPortOf)
import Ecluse.Runtime.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredMirrorEnqueueFailure,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Runtime.Telemetry.Tracing (instrumentDataPlaneManagerSettings, tracingPortOf)

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

Assemble the composition root from configuration. Parse the environment layer and
the optional config document, __validate everything and fail fast at boot__ on any
problem (a malformed env, an unresolved rule policy, a configured mount with no
adapter, a credential reference that does not resolve, or a mirror-queue backend
not built in this binary), aggregating the failures so a single run reports them
all. On success, build the handles (the shared HTTP @Manager@, the config-selected
mirror queue, the metadata cache, the logger, the process-global credential
provider, and the telemetry substrate, off unless @ECLUSE_TELEMETRY@ enables it)
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
    -- live (in the 'withEnv' body). With telemetry off the eventual instruments are the
    -- no-op-meter ones, so the reporters are inert either way.
    deferredMetrics <- newDeferredMetrics
    let credentialReporters =
            CredentialReporters
                { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
                , crRefreshReporter = deferredRefreshReporter deferredMetrics CodeArtifact
                }
    -- Build the process-global mirror-target write provider(s) selected by config:
    -- the static token, or the CodeArtifact mint (whose inputs are validated and which
    -- mints once eagerly, so a misconfiguration fails loudly here at boot).
    providers <- initCredentialProviders credentialReporters env >>= orExit (T.unlines . map renderBootError)
    -- The advisory-database sync plan: with a bucket configured, every configured
    -- mount ecosystem gets its own slot (the shadow-swap read side), its own
    -- supervised sync task, and its own one-way first-sync readiness flag, each
    -- independent so one ecosystem's missing artifact never holds back
    -- another's. Without a bucket the map is empty: rules abstain and
    -- readiness is ungated.
    cveSyncPlan <- planCveSync env
    let ruleDepsFor = cveRuleDepsFor cveSyncPlan (deferredBreakerReporter deferredMetrics EffectfulRule)
    bindings <- planMounts mountBindingFor getCurrentTime ruleDepsFor providers config >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers config)
    -- Select the mirror-queue backend from config (the GCP arm is a fail-loud
    -- "not built" boot error, never a silent fall-through); the resulting plan is
    -- handed to the one queue-construction site below.
    queuePlan <- orExit (T.unlines . map renderBootError) (planMirrorQueue env)
    -- The effective admission capacity: explicit config, else computed from the
    -- post-runtime-posture capability count, logged with its provenance beside the
    -- runtime lines. This bounds metadata materialisation only; the private manager's
    -- pool is sized independently below, since a trusted tarball hit streams outside
    -- admission (see 'Composition.resolvePrivateConnections' and issue #634).
    capabilities <- getNumCapabilities
    let (serveMaxInFlight, admissionLine) = Composition.resolveServeAdmission (cfgServeMaxInFlight env) capabilities
    logBootInfo logEnv admissionLine
    serveAdmission <- newServeAdmission serveMaxInFlight
    -- The private-upstream connection pool: an explicit override, else computed from the
    -- process file-descriptor limit (the pool's real ceiling, since each pooled
    -- connection is one descriptor). Sized for the un-admitted private-hit streaming
    -- fan-out, not the admission capacity.
    fdLimit <- Composition.openFileSoftLimit
    let (privateConnections, privateConnectionsLine) = Composition.resolvePrivateConnections (cfgPrivateConnectionsPerHost env) fdLimit
    logBootInfo logEnv privateConnectionsLine
    -- The public pool: an explicit override, else computed from the same
    -- file-descriptor datapoint at half the private share. The onboarding
    -- fail-over's artifact streams and the worker's back-fill fetches ride this
    -- manager without coalescing, so its retention must cover that transient
    -- fan-out, not only the admission-bounded metadata misses.
    let (publicConnections, publicConnectionsLine) = Composition.resolvePublicConnections (cfgPublicConnectionsPerHost env) fdLimit
    logBootInfo logEnv publicConnectionsLine
    let serverConfig =
            (mkServerConfig bindings)
                { scPort = cfgPort env
                , scDrainTimeout = ShutdownDrainTimeout (cfgShutdownDrainTimeout env)
                , scCheckReady = cveSyncReady cveSyncPlan
                , scOnException = warpExceptionHook logEnv
                }
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    -- The config-selected mirror queue, built once here (the single constructor
    -- call) from the validated plan: the durable AWS SQS backend, or the bounded
    -- in-memory backend -- which first emits a loud boot warning (it is
    -- non-durable / best-effort) and logs each rate-limited cap-overflow drop.
    backendQueue <- buildMirrorQueue logEnv queuePlan
    -- Decouple the serve path from the backend's own enqueue latency: what Env
    -- captures is the buffered hand-off (an STM write), and the drain loop below --
    -- raced against the services -- delivers to the backend (an SQS round trip on
    -- that backend) off the request path, where it would otherwise hold the served
    -- connection's turn.
    (queue, drainEnqueueBuffer) <-
        bufferedMirrorHandOff (logBootWarning logEnv) (deferredMirrorEnqueueFailure deferredMetrics) backendQueue
    metadataCache <- newMetadataCache (Composition.cacheConfigFor env)
    heartbeat <- newWorkerHeartbeat

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
    -- The mirror worker's publish-side registry client, resolved per ecosystem from
    -- the configured mirror target and its write credential. It writes to the
    -- operator-configured, trusted mirror target, so it uses the trusted private
    -- manager (the private origin's credential-forwarding path).
    publishClient <- resolvePublishClient privateManager publishTargets
    withEnvWithAdmission serveAdmission publishClient queue manager privateManager metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
        -- The instruments now exist (built in 'withEnv' from the telemetry handle);
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
        race_
            (runServices serverConfig (workerPoliciesFor builtEnv bindings) builtEnv)
            (concurrently_ (superviseDrain builtEnv drainEnqueueBuffer) (mapConcurrently_ id syncTasks))

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

{- | The rules' boot-bound capabilities for one mount ecosystem: the CVE
lookup borrows through that ecosystem's own slot when the sync plan carries
one, and abstains otherwise, so a mount's rules can never read a neighbouring
ecosystem's advisory database.
-}
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter eco =
    RuleDeps
        { rdWithCveLookup = maybe (\use -> use Nothing) (withSlotLookup . csSlot) (Map.lookup eco plan)
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . csSlot) (Map.lookup eco plan)
        , rdBreakerReporter = reporter
        }

{- | The readiness gate over the sync plan: ready once every configured
ecosystem's advisory database has first-synced. The flags flip one way, so
readiness never flaps on this; an empty plan (no bucket) is vacuously ready.
-}
cveSyncReady :: Map.Map Ecosystem CveSyncHandle -> IO Bool
cveSyncReady plan = allM (readTVarIO . csReady) (Map.elems plan)

{- | The sync tasks' timing: the shipped boot burst over the configured poll
interval. The microsecond conversion cannot wrap: the config decoder bounds
the interval to @[1, maxBound div 1_000_000]@ seconds.
-}
cveSyncScheduleFor :: AppConfig -> SyncSchedule
cveSyncScheduleFor env =
    SyncSchedule
        { schedBootBackoff = bootBackoffDelays
        , schedPollDelay = round (cfgCveDbPollInterval env) * 1_000_000
        }

-- | One configured ecosystem's advisory-sync wiring.
data CveSyncHandle = CveSyncHandle
    { csSlot :: CveSlot
    -- ^ The slot this ecosystem's mount rules borrow through.
    , csReady :: TVar Bool
    -- ^ The one-way first-sync readiness flag.
    , csEnv :: SyncEnv
    -- ^ The sync task's environment.
    }

{- | Build the advisory-sync plan from config: nothing without a configured
vulnerability-database bucket; otherwise one 'CveSyncHandle' per configured
mount ecosystem, each against its own stable per-ecosystem object key and
canonical on-disk path under the OSV data dir. Prepares the data dir (created
if missing; stray @.tmp@ downloads from an interrupted run swept) so the sync
tasks start clean. Note the readiness consequence: an operator who mounts an
ecosystem Pilot does not compile has declared an artifact that never arrives,
and the pod honestly never reports ready.
-}
planCveSync :: AppConfig -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync appCfg = case cfgVulnerabilityDatabaseBucket appCfg of
    Nothing -> pure Map.empty
    Just bucket -> do
        let dataDir = cfgOsvDataDir appCfg
        createDirectoryIfMissing True dataDir
        sweepStaleTemps dataDir
        awsEnv <- buildS3Env (cfgAwsEndpointUrl appCfg >>= Composition.parseEndpointUrl)
        Map.fromList <$> traverse (cveSyncHandleFor appCfg awsEnv bucket) (Map.keys (cfgMounts appCfg))

-- One ecosystem's sync wiring: a fresh slot and readiness flag, and the sync
-- environment against the ecosystem's stable object key and canonical on-disk
-- path under the OSV data dir.
cveSyncHandleFor :: AppConfig -> AWS.Env -> Text -> Ecosystem -> IO (Ecosystem, CveSyncHandle)
cveSyncHandleFor appCfg awsEnv bucket eco = do
    slot <- newCveSlot
    ready <- newTVarIO False
    let key = osvDbFileName (ecosystemName eco)
        syncEnv =
            SyncEnv
                { syncFetch = s3CveFetch awsEnv bucket (toText key) (cfgMaxOsvDbBytes appCfg)
                , syncEcosystem = eco
                , syncDbPath = cfgOsvDataDir appCfg </> key
                , syncSlot = slot
                }
    pure (eco, CveSyncHandle{csSlot = slot, csReady = ready, csEnv = syncEnv})

-- Sweep stray in-progress downloads an interrupted run left beside the
-- canonical artifacts (relevant to in-pod container restarts, where an
-- emptyDir survives). Best-effort: an unreadable dir is a fresh start.
sweepStaleTemps :: FilePath -> IO ()
sweepStaleTemps dataDir =
    ( do
        entries <- listDirectory dataDir
        forM_ [e | e <- entries, "tmp" `isExtensionOf` e] (\e -> removeFile (dataDir </> e) `catchAny` const pass)
    )
        `catchAny` const pass

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
runServices serverConfig policies env = race_ (runServer serverConfig env) (runWorker policies env)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the
config-derived 'ServerConfig'.

This is the npm-aware composition site: 'mountBindingFor' mounts npm -- its path
grammar ("Ecluse.Core.Registry.Npm.Route") and its denial renderer
("Ecluse.Core.Registry.Npm.Serve") -- into the otherwise ecosystem-neutral web layer
('Ecluse.Runtime.Server.runServer'), so the agnostic server stays closed over the shared
'Ecluse.Core.Server.Route.Route' set and only this one place names an ecosystem.
Splitting the server into its own binary later reuses this same entry.
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

{- | The fallback server settings: a single npm mount with __no__ packument-serve
or publish dependencies, so the packument route is the recognised-but-unserved @501@
stub and a publish is @405@ (no publication target). Exposed so the composed front
door can be driven directly without binding a socket (e.g. embedded in another @wai@
application, or exercised in tests through 'Ecluse.Runtime.Server.application') to assert the
routing and the unwired-mount surface; a real launch derives its bindings from
configuration in 'run'.
-}
npmServerConfig :: ServerConfig
npmServerConfig = mkServerConfig [npmMount Nothing Nothing]

{- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that
ecosystem has no adapter wired. The ecosystem selects its path grammar (the
'Ecluse.Core.Server.Route.Classifier') and its denial renderer (the
'Ecluse.Core.Server.Response.MountRenderer'), and its path prefix is __derived__
from it ('prefixFor') rather than configured, so the ecosystem is the single thing
that drives the binding (see @docs\/architecture\/hosting.md@ → "Mounts"). The
composition root supplies the packument-serve dependencies once the per-mount
registry set is resolved; 'Nothing' for them leaves the packument route the
recognised-but-unserved @501@ stub.

npm is the only ecosystem with an adapter; the others have no registry client or
renderer, so they resolve to 'Nothing', a loud miss at the call site rather than a
silently half-wired mount.
-}
mountBindingFor :: Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding
mountBindingFor eco packumentDeps publishDeps = case eco of
    Npm -> Just (npmMount packumentDeps publishDeps)
    _ -> Nothing

{- The npm mount: npm's complete wiring under its derived @\/npm@ prefix -- its path
grammar and its denial renderer -- taking the packument-serve and first-party publish
dependencies the composition root supplies ('Nothing' packument deps leave the
packument route the recognised-but-unserved @501@ stub; 'Nothing' publish deps leave a
@PUT \/{pkg}@ the @405@ opt-out -- no publication target).
-}
npmMount :: Maybe PackumentDeps -> Maybe PublishDeps -> MountBinding
npmMount packumentDeps publishDeps =
    MountBinding
        { bindingPrefix = prefixFor Npm
        , bindingClassifier = Npm.classify
        , bindingPackumentDeps = packumentDeps
        , bindingPublishDeps = publishDeps
        , bindingRenderer = npmRenderer
        }

{- | Run the supervised mirror worker over the composition-root 'Env' and the
per-ecosystem re-evaluation bundles: the consume → re-evaluate → fetch → verify → publish →
ack loop against the queue, the publish-side registry client, and the credential handle, in
the worker monad ('Ecluse.Core.Worker.WorkerM') over the worker runtime
('Ecluse.Runtime.Env.workerRuntimeOf'). The bundles carry the same prepared rules and public origin
the serve path gates with, so the worker re-runs current policy against a job before
mirroring it.

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
bounded exponential backoff from one second), except the wiring faults no retry
can fix -- an unconfigured registry handle or an unconfigured credential leaf
reached at runtime -- which fail up through the services race and take the
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
        | Just RegistryUnconfigured <- fromException fault = Permanent
        | Just (Unconfigured _) <- fromException fault = Permanent
        | otherwise = Transient

{- | Resolve the worker's per-ecosystem re-evaluation bundles from the served mounts: for
each mount that serves a packument (carries 'PackumentDeps'), a bundle keyed by the
ecosystem its path prefix names. A mount left at the recognised-but-unserved stub
contributes none, and a job for an ecosystem absent here is fail-closed at the worker. The
bundles reuse each mount's __own__ prepared rules, so the serve gate and the ingest
re-evaluation share one prepared rule set (and any per-source breaker state) rather than
preparing a second.
-}
workerPoliciesFor :: Env -> [MountBinding] -> WorkerPolicies
workerPoliciesFor env bindings =
    Map.fromList
        [ (eco, workerPolicyFor env deps)
        | binding <- bindings
        , let prefixHead :| _ = bindingPrefix binding
        , Just eco <- [parseEcosystem prefixHead]
        , Just deps <- [bindingPackumentDeps binding]
        ]

{- Build one mount's worker re-evaluation bundle from its packument-serve dependencies:
the single-version resolver over the guarded public origin through the shared metadata
cache (the same fetch-and-project the serve path runs), the mount's prepared rules, its
configured integrity floor, its tarball-host gate, and its injected clock -- every
decision input taken from the mount's __own__ 'PackumentDeps', so the ingest decision
cannot diverge from the serve decision. The metadata client is built through the same
injected constructor the serve path uses ('pdNewMetadataClient', over the same shared
manager 'srPublicManager' is wired to), anonymous (no client credential reaches the
public origin), inheriting the resolved-IP SSRF recheck. Its own failure and
dropped-entry logs are elided (the worker logs its own re-evaluation outcome per job),
while the upstream-fetch metrics still record through the shared instruments. -}
workerPolicyFor :: Env -> PackumentDeps -> WorkerPolicy
workerPolicyFor env deps =
    WorkerPolicy
        { wpResolveVersion = fetchVersionDetails client
        , wpRules = pdRules deps
        , wpMinIntegrity = pdMinIntegrity deps
        , wpArtifactHostHonoured =
            -- The same host-gate composition the serve path applies before its public
            -- artifact fetch, closed against the public upstream host (the reference
            -- host the public leg gates dist.tarball hosts by).
            tarballHostHonoured UntrustedOrigin deps (thgPublicHost (pdTarballHostGate deps))
        , wpNow = pdNow deps
        }
  where
    client =
        pdNewMetadataClient
            deps
            (tracingPortOf (envTelemetry env))
            (metricsPortOf (envMetrics env))
            Public
            (Cached (envMetadataCache env) (Source (pdPublicBaseUrl deps)))
            (\_ _ -> pure ())
            (\_ _ -> pure ())
            (\_ -> pure ())
            (pdLimits deps)
            (envManager env)
            (pdPublicBaseUrl deps)
            Nothing

{- Build the worker's publish-side registry client from the resolved per-ecosystem
publish targets, over the given (trusted) manager.

The publish client speaks the registry protocol; the only ecosystem with an adapter
is npm, so a target is wired into an npm client pointed at the mirror-target
endpoint. The credential is minted fresh per publish through the provider's
'currentToken'. When no mount is configured there is nothing to publish, so
the slot holds the refusing 'unconfiguredRegistry' placeholder, whose effectful
fields fail loudly if ever called. -}
resolvePublishClient :: Manager -> [PublishTarget] -> IO RegistryClient
resolvePublishClient manager targets =
    case find ((== Npm) . ptEcosystem) targets of
        Nothing -> pure unconfiguredRegistry
        Just target -> do
            let mintToken = Just . authSecret <$> currentToken (ptCredentials target)
            newNpmPublishClient
                NpmClientConfig
                    { npmBaseUrl = ptMirrorUrl target
                    , npmManager = manager
                    , npmToken = Nothing
                    , npmLimits = defaultLimits
                    }
                mintToken

{- | Raised by 'unconfiguredRegistry' when an effectful registry field is called
with no backend wired in: a composition-root misconfiguration. A distinct typed
exception (not a stringly @userError@), so the refusal is observable in a test,
catchable by type, and never mistaken for a configured backend's own failure.
-}

{- | A registry handle with no backend behind it: every effectful field __refuses
loudly__ (a typed 'RegistryUnconfigured') and every pure @parse*@ field returns
'Left', so an unconfigured fetch\/publish or parse fails explicitly rather than
silently returning a fabricated success. It holds the handle slot in the
composition root where a configured backend is selected elsewhere. The fetch field's
type carries a 'Ecluse.Core.Registry.FetchFault' channel, but this handle does not
use it: an unwired backend is a composition fault with no per-request decision, so
it stays a justified typed throw rather than a value a caller might fall through.
-}
unconfiguredRegistry :: RegistryClient
unconfiguredRegistry =
    RegistryClient
        { fetchMetadata = const refuse
        , publishArtifact = \_ _ _ _ -> refuse
        , parsePackageInfo = \_ _ -> Left notConfigured
        , parseVersionDetails = \_ _ -> Left notConfigured
        , parseVersionList = const (Left notConfigured)
        }
  where
    refuse :: IO a
    refuse = throwIO RegistryUnconfigured

    notConfigured :: ParseError
    notConfigured = ParseError{parseErrorMessage = "no registry backend configured"}

{- | A credential handle with no backend behind it: a static, non-expiring empty
secret. It holds the 'CredentialProvider' slot in the composition root until a live
backend is selected, for the mirror-target write and for the private-upstream read
under the @service@ \/ @delegated-cache@ strategies. The default @passthrough@
strategy needs no read credential at all (reads forward the caller's own token), so
this empty placeholder is harmless on the serve path there. See
@docs\/architecture\/access-model.md@.
-}
