{- | Écluse — a supply-chain resilience proxy for package registries.

Écluse (package @ecluse@) is a lightweight proxy that sits between consumers
(developers, CI) and a package registry, applying a configurable __resilience__
policy before any dependency reaches a build — without taking on the cost of
hosting packages itself. The name is French for a canal lock: a chamber whose
gates never open at once. That is the posture — not a wall that blocks, but a
controlled passage every dependency is held in and cleared through before it is
admitted to a build.

The goal is __resilience, not malware detection__: shrink the blast radius of a
bad publish — a hijacked maintainer account, a race-to-publish, a typosquat —
rather than promise to recognise malice. And Écluse is __not a registry__:
storage is delegated to whatever backend the operator runs (e.g. AWS
CodeArtifact, GCP Artifact Registry), and Écluse only governs what may be fetched
from, and mirrored to, those backends. npm is the first ecosystem; the domain
model is deliberately ecosystem-agnostic so that PyPI and RubyGems can follow.

== How a request is cleared

Écluse speaks a registry's native protocol across three read-path registries —
the client's, a /private upstream/ of already-vetted packages, and the /public/
registry — and the two request shapes use them differently:

* A __tarball__ request is gated for that one version: a private-upstream hit is
  streamed unfiltered (already vetted); on a miss, the proxy fetches the
  version's public metadata, evaluates the rules, and either streams it from
  public __and enqueues an asynchronous mirror job__ or returns a denial.
* A __packument__ (metadata) request is a /merge/: the private and public
  upstreams are fetched in parallel, public versions are filtered by the rules
  while private versions are trusted, and the two are combined into one document
  (private wins a version collision, an integrity divergence is flagged as a
  supply-chain signal, and @latest@ is repointed to the newest survivor).

Two properties run through both shapes: the rules engine is __deny by default__
— a version is admitted only if some rule allows it and none denies it — and
__mirroring is demand-driven__, so only versions actually pulled are mirrored,
and never on the request's critical path.

== How the code is organized

Écluse is a __functional core with effects at the edges__: the policy and
protocol logic is pure and trivially testable, and @IO@ is confined to a thin
shell. Swappable backends sit behind /handles/ — records of functions chosen at a
single composition root — so a new cloud or a new ecosystem is an added
implementation behind an existing handle, not a structural change.

The library's vocabulary, roughly from the pure core outward:

* __Domain model__ — "Ecluse.Core.Package" (the ecosystem-agnostic package vocabulary
  the rules reason over), "Ecluse.Core.Version" (version identity and per-ecosystem
  ordering), and "Ecluse.Core.Ecosystem" (the ecosystem tag the rest dispatches on).
* __Policy__ — "Ecluse.Core.Rules" (deny-by-default evaluation) over the rule types
  in "Ecluse.Core.Rules.Types".
* __Protocol boundary__ — "Ecluse.Core.Registry" (the registry-protocol handle),
  "Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" (the lenient npm
  wire decoders and their projection onto the domain model),
  "Ecluse.Core.Registry.Npm.Route" (the npm path grammar), and "Ecluse.Core.Server.Route"
  (the shared serve-action 'Route' set and the injected route classifier).
* __Cloud handles__ — "Ecluse.Core.Credential" (minting the mirror-target write token)
  and "Ecluse.Core.Queue" (the durable mirror-job hand-off to the worker).
* __Mirror worker__ — "Ecluse.Core.Worker" (the supervised consume loop that fetches,
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
module Ecluse (
    -- * Entry point
    run,

    -- * Split-ready services
    runServer,
    runWorker,

    -- * npm front door
    npmServerConfig,
    mountBindingFor,

    -- * Composition glue (exposed for direct testing)
    mirrorWriteProvider,
    orExit,
    BootAborted (..),

    -- * Default handles
    unconfiguredRegistry,
    unconfiguredCredentials,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Katip (Environment (Environment), LogEnv, Severity (InfoS, WarningS), katipAddNamespace, logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnvironment)
import UnliftIO (concurrently_, throwIO)

import Ecluse.Composition (
    CredentialProviders,
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    PublishTarget (ptCredentials, ptEcosystem, ptMirrorUrl),
    initCredentialProviders,
    memoryQueueDropWarning,
    mirrorQueuePlanWarning,
    planMirrorQueue,
    planMounts,
    planPublishTargets,
    renderBootError,
 )
import Ecluse.Composition qualified as Composition
import Ecluse.Config (
    ConfigDoc,
    CredentialBackend,
    EnvConfig (cfgLogFormat, cfgMirrorTargetCredentialProvider, cfgPort, cfgShutdownDrainTimeout, cfgTelemetry),
    decodeDocument,
    parseEnv,
    renderEnvErrors,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, currentToken, mkSecret, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), prefixFor)
import Ecluse.Core.Queue (MirrorQueue, newBoundedInMemoryQueue)
import Ecluse.Core.Queue.Sqs (newSqsQueue)
import Ecluse.Core.Registry (
    ParseError (..),
    RegistryClient (..),
 )
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmClient)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Rules (renderBootOrder)
import Ecluse.Core.Security (defaultLimits, lowerCaseHosts)
import Ecluse.Core.Security.Egress (guardedManagerSettings)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps, PublishDeps, pdRules)
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), Provider (CodeArtifact))
import Ecluse.Core.Worker (runWorkerM, workerLoop)
import Ecluse.Env (Env, envDdContext, envLogEnv, envMetrics, newWorkerHeartbeat, withEnv, workerRuntimeOf)
import Ecluse.Log (moduleField, newLogEnv)
import Ecluse.Server (MountBinding (..), ServerConfig (scDrainTimeout, scPort), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
import Ecluse.Server qualified as Server
import Ecluse.Telemetry (TelemetrySwitch (TelemetryOff, TelemetryOn), withTelemetry)
import Ecluse.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Telemetry.Resolve (prepareTelemetry)
import Ecluse.Telemetry.Tracing (instrumentDataPlaneManagerSettings)

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

It assembles the composition root from configuration: it parses the environment
layer and the optional config document, __validates everything and fails fast at
boot__ on any problem (a malformed env, an unresolved rule policy, a configured
mount with no adapter, a credential reference that does not resolve, or a
mirror-queue backend that is not built in this binary), aggregating the failures so
a single run reports them all. On success it builds the handles — the shared HTTP
@Manager@, the config-selected mirror queue, the metadata cache, the logger, the
process-global credential provider, and the telemetry substrate (off unless
@PROXY_TELEMETRY@ enables it) — into an 'Env', derives the served mount bindings,
then runs the
server and the mirror worker __concurrently__ over that single 'Env' ('runServer'
and 'runWorker'). Bracketing the 'Env' (and the telemetry providers) for the
lifetime of both means their shared resources are torn down along every exit path.
-}
run :: IO ()
run = do
    env <- parseEnv >>= orExit renderEnvErrors
    mDoc <- loadDocument
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
    bindings <- planMounts mountBindingFor getCurrentTime providers env mDoc >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers env mDoc)
    -- Select the mirror-queue backend from config (the GCP arm is a fail-loud
    -- "not built" boot error, never a silent fall-through); the resulting plan is
    -- handed to the one queue-construction site below.
    queuePlan <- orExit (T.unlines . map renderBootError) (planMirrorQueue env)
    let serverConfig =
            (mkServerConfig bindings)
                { scPort = cfgPort env
                , scDrainTimeout = ShutdownDrainTimeout (cfgShutdownDrainTimeout env)
                }
    logEnv <- newLogEnv (cfgLogFormat env) (Environment "production")
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    -- The config-selected mirror queue, built once here (the single constructor
    -- call) from the validated plan and captured in Env: the durable AWS SQS backend,
    -- or the bounded in-memory backend — which first emits a loud boot warning (it is
    -- non-durable / best-effort) and logs each rate-limited cap-overflow drop.
    queue <- buildMirrorQueue logEnv queuePlan
    metadataCache <- newMetadataCache (Composition.cacheConfigFor env)
    heartbeat <- newWorkerHeartbeat
    -- Resolve the telemetry identity (DD_* / OTEL_*) and normalise the OTEL_*
    -- environment the SDK reads, before the substrate initialises. A no-op when
    -- telemetry is off.
    prepareTelemetryBoot (cfgTelemetry env) logEnv
    withTelemetry (cfgTelemetry env) logEnv $ \telemetry -> do
        -- Two data-plane managers, split by trust. The guarded one rechecks every
        -- resolved outbound IP against the internal-range block (DNS-rebinding /
        -- resolve-to-internal SSRF) and serves the untrusted upstreams — the public
        -- upstream and every artifact stream — blocking every internal resolved address
        -- (an empty opt-in: the secure default). The trusted one serves the private
        -- origin only: the private base URL is operator-configured and may legitimately
        -- resolve to an internal address, so it is deliberately not rechecked. Both are
        -- built inside the telemetry bracket so that, with telemetry enabled, each
        -- carries the http-client instrumentation (child spans + W3C context
        -- propagation) hung off the substrate's installed providers; with it off the
        -- instrumentation step is the identity, so the managers are exactly the guarded
        -- and trusted ones.
        manager <- newManager =<< instrumentDataPlaneManagerSettings telemetry (guardedManagerSettings (lowerCaseHosts Set.empty) tlsManagerSettings)
        privateManager <- newManager =<< instrumentDataPlaneManagerSettings telemetry tlsManagerSettings
        -- The mirror worker's publish-side registry client, resolved per ecosystem from
        -- the configured mirror target and its write credential. It writes to the
        -- operator-configured, trusted mirror target, so it uses the trusted private
        -- manager (no resolved-IP recheck — that guards only the untrusted public fetch).
        publishClient <- resolvePublishClient privateManager publishTargets
        withEnv publishClient queue (mirrorWriteProvider (cfgMirrorTargetCredentialProvider env) providers) manager privateManager metadataCache logEnv telemetry heartbeat $ \builtEnv -> do
            -- The instruments now exist (built in 'withEnv' from the telemetry handle);
            -- install them so the credential provider's deferred reporters go live for
            -- the rest of the run. They are the no-op-meter instruments when telemetry
            -- is off, so this is inert in that posture.
            installMetrics deferredMetrics (envMetrics builtEnv)
            runServices serverConfig builtEnv

{- | Read the optional structured config document from the @PROXY_CONFIG@ env blob,
decoding it strictly. 'Nothing' when unset — an env-only deployment supplies no
document and runs on the built-in default policy. A set-but-undecodable blob is a
fail-fast boot error (an operator typo must not be silently ignored).

@PROXY_CONFIG@ is the documented, named source for the inline document (see
@docs\/architecture\/configuration.md@ → "Configuration"); the alternative
file form has no documented env var for its path yet, so it is not read here.
-}
loadDocument :: IO (Maybe ConfigDoc)
loadDocument =
    lookupEnv "PROXY_CONFIG" >>= \case
        Nothing -> pure Nothing
        Just blob -> Just <$> orExit ("PROXY_CONFIG: " <>) (decodeDocument (encodeUtf8 blob))

{- Build the config-selected mirror queue from its plan: the durable AWS SQS backend,
or the bounded in-memory backend. The in-memory arm first emits the loud boot warning
('mirrorQueuePlanWarning' — it is non-durable / best-effort) through the
composition-root logger, then constructs the bounded queue with a drop callback that
logs each rate-limited cap-overflow drop at a warning. (A drop /metric/ hooks in
alongside the log once the @ecluse.mirror.*@ catalogue lands.) -}
buildMirrorQueue :: LogEnv -> MirrorQueuePlan -> IO MirrorQueue
buildMirrorQueue logEnv plan = do
    whenJust (mirrorQueuePlanWarning plan) (logBootWarning logEnv)
    case plan of
        SqsBackend sqsConfig -> newSqsQueue sqsConfig
        MemoryBackend memoryConfig ->
            newBoundedInMemoryQueue memoryConfig (logBootWarning logEnv . memoryQueueDropWarning)

{- Log one line at 'WarningS' through the composition-root 'LogEnv', tagged with this
module — the plain-'IO' katip path the boot phase uses (it holds no @Handler@ reader),
the same shape "Ecluse.Telemetry.Resolve" and "Ecluse.Core.Server.Pipeline.Internal" use. -}
logBootWarning :: LogEnv -> Text -> IO ()
logBootWarning logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM WarningS (ls message))

{- Log one line at 'InfoS' through the composition-root 'LogEnv', the same plain-'IO'
katip path 'logBootWarning' uses, for non-warning boot diagnostics. -}
logBootInfo :: LogEnv -> Text -> IO ()
logBootInfo logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM InfoS (ls message))

{- Log every wired mount's resolved rule boot order ('renderBootOrder' — the single
total order evaluation walks), one line per rule, so an operator can read the
effective policy resolution straight from the start-up log. A mount with no packument
deps (the unserved stub) contributes nothing. -}
logRuleBootOrder :: LogEnv -> [MountBinding] -> IO ()
logRuleBootOrder logEnv = traverse_ logMount
  where
    logMount binding = whenJust (bindingPackumentDeps binding) $ \deps -> do
        let label = T.intercalate "/" (toList (bindingPrefix binding))
        logBootInfo logEnv ("rule boot order for mount " <> label <> ":")
        traverse_ (logBootInfo logEnv) (renderBootOrder (pdRules deps))

{- The process-global mirror-write credential provider stored in 'Env' for the
worker, selected by the configured provider backend
('Ecluse.Config.cfgMirrorTargetCredentialProvider'): the static token or the
CodeArtifact mint. In the common case there is a single provider; the no-backend
placeholder only holds the slot when the selected provider was not built — a mount
that references it has already failed the boot-time credential check by this point,
so the worker (the slot's only consumer) never reaches the placeholder. -}
mirrorWriteProvider :: CredentialBackend -> CredentialProviders -> CredentialProvider
mirrorWriteProvider backend providers =
    fromMaybe unconfiguredCredentials (Composition.lookupProvider backend providers)

{- | Raised to abort start-up after a boot phase has reported its aggregated
failure to stderr. A distinct type — rather than a bare 'exitFailure' — so the
abort is observable in a test without the process actually exiting; uncaught, it
propagates to 'main' and the runtime exits non-zero, the operator-facing fail-fast.
-}
data BootAborted = BootAborted
    deriving stock (Eq, Show)

instance Exception BootAborted

{- Report the rendered failure to stderr and abort the boot when a phase fails,
otherwise yield its value. The aggregated failure block is written so an operator
sees every problem from a single failed launch, then 'BootAborted' unwinds to
'main'. -}
orExit :: (e -> Text) -> Either e a -> IO a
orExit render = \case
    Right a -> pure a
    Left err -> TIO.hPutStrLn stderr (render err) >> throwIO BootAborted

{- Prepare the telemetry substrate before the SDK initialises: when enabled, resolve
the identity, normalise the @OTEL_*@ environment the SDK reads, and install the
throttled export-error handler ("Ecluse.Telemetry.Resolve.prepareTelemetry"). A no-op
when telemetry is off, so an unset @PROXY_TELEMETRY@ reads no process environment and
configures nothing. -}
prepareTelemetryBoot :: TelemetrySwitch -> LogEnv -> IO ()
prepareTelemetryBoot switch logEnv = case switch of
    TelemetryOff -> pass
    TelemetryOn -> do
        environment <- getEnvironment
        prepareTelemetry logEnv environment

{- Run the server and the mirror worker concurrently over one composition-root
'Env', the shape the single-process program uses. The two are independent (each
depends only on the handles in 'Env', not on each other), so splitting into
separate binaries later is two thin entry points calling 'runServer' \/
'runWorker' — no rearchitecting. The server's settings (its derived mount bindings
and port) are supplied by the composition root and threaded to 'runServer'.
-}
runServices :: ServerConfig -> Env -> IO ()
runServices serverConfig env = concurrently_ (runServer serverConfig env) (runWorker env)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the
config-derived 'ServerConfig'.

This is the npm-aware composition site: 'mountBindingFor' mounts npm — its path
grammar ("Ecluse.Core.Registry.Npm.Route") and its denial renderer
("Ecluse.Core.Registry.Npm.Serve") — into the otherwise ecosystem-neutral web layer
('Ecluse.Server.runServer'), so the agnostic server stays closed over the shared
'Ecluse.Core.Server.Route.Route' set and only this one place names an ecosystem.
Splitting the server into its own binary later reuses this same entry.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer = Server.runServer

{- | The fallback server settings: a single npm mount with __no__ packument-serve
or publish dependencies, so the packument route is the recognised-but-unserved @501@
stub and a publish is @405@ (no publication target). Exposed so the composed front
door can be driven directly without binding a socket (e.g. embedded in another @wai@
application, or exercised in tests through 'Ecluse.Server.application') to assert the
routing and the unwired-mount surface; a real launch derives its bindings from
configuration in 'run'.
-}
npmServerConfig :: ServerConfig
npmServerConfig = mkServerConfig [npmMount Nothing Nothing]

{- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that
ecosystem has no adapter wired. The ecosystem selects its path
grammar (the 'Ecluse.Core.Server.Route.Classifier') and its denial renderer (the
'Ecluse.Core.Server.Response.MountRenderer'), and its path prefix is __derived__ from it
('prefixFor') rather than configured — so the ecosystem is the single thing that
drives the binding (see @docs\/architecture\/hosting.md@ → "Mounts"). The
packument-serve dependencies are passed in (the composition root supplies them once
the per-mount registry set is resolved); 'Nothing' for them leaves the packument
route the recognised-but-unserved @501@ stub.

npm is the only ecosystem with an adapter; the others have no registry
client or renderer, so they resolve to 'Nothing' — a loud miss at the call
site rather than a silently half-wired mount.
-}
mountBindingFor :: Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding
mountBindingFor eco packumentDeps publishDeps = case eco of
    Npm -> Just (npmMount packumentDeps publishDeps)
    _ -> Nothing

{- The npm mount: npm's complete wiring under its derived @\/npm@ prefix — its path
grammar and its denial renderer — taking the packument-serve and first-party publish
dependencies the composition root supplies ('Nothing' packument deps leave the
packument route the recognised-but-unserved @501@ stub; 'Nothing' publish deps leave a
@PUT \/{pkg}@ the @405@ opt-out — no publication target).
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

{- | Run the supervised mirror worker over the composition-root 'Env': the
consume → fetch → verify → publish → ack loop against the queue, the publish-side
registry client, and the credential handle, in the worker monad
('Ecluse.Core.Worker.WorkerM') over the worker runtime ('Ecluse.Env.workerRuntimeOf').

This is the composition-root __hoist point__: it resolves the request-independent @dd@
correlation object (the service identity; no span is active at the worker entry) and
installs it as the worker's initial @katip@ context, then discharges the loop to 'IO'
through 'Ecluse.Core.Worker.runWorkerM' — the worker analogue of the serve path's
'Ecluse.Core.Server.Context.runHandler' boundary. The loop logic lives in
"Ecluse.Core.Worker"; the single-process program runs this alongside 'runServer'.
-}
runWorker :: Env -> IO ()
runWorker env = do
    dd <- ddPayloadNow (envDdContext env)
    runWorkerM (envLogEnv env) dd (workerRuntimeOf env) (katipAddNamespace "worker" workerLoop)

{- Build the worker's publish-side registry client from the resolved per-ecosystem
publish targets, over the given (trusted) manager.

The publish client speaks the registry protocol; the only ecosystem with an adapter
is npm, so a target is wired into an npm client pointed at the mirror-target
endpoint and carrying the bearer minted from the target's credential provider. The
credential is read once here at the composition root (the @static@ provider never
expires, so a baked token is correct for it). When no mount is configured there is
nothing to publish, so the slot holds the refusing 'unconfiguredRegistry'
placeholder, whose effectful fields fail loudly if ever called — the worker only
reaches it once a job exists, which only a configured mount produces. -}
resolvePublishClient :: Manager -> [PublishTarget] -> IO RegistryClient
resolvePublishClient manager targets =
    case find ((== Npm) . ptEcosystem) targets of
        Nothing -> pure unconfiguredRegistry
        Just target -> do
            token <- authSecret <$> currentToken (ptCredentials target)
            newNpmClient
                NpmClientConfig
                    { npmBaseUrl = ptMirrorUrl target
                    , npmManager = manager
                    , npmToken = Just token
                    , npmLimits = defaultLimits
                    }

{- | Raised by 'unconfiguredRegistry' when an effectful registry field is called
with no backend wired in — a composition-root misconfiguration. A distinct typed
exception (not a stringly @userError@), so the refusal is observable in a test,
catchable by type, and never mistaken for a configured backend's own failure.
-}
data RegistryUnconfigured = RegistryUnconfigured
    deriving stock (Eq, Show)

instance Exception RegistryUnconfigured

{- | A registry handle with no backend behind it: every effectful field __refuses
loudly__ (a typed 'RegistryUnconfigured') and every pure @parse*@ field returns 'Left', so an
unconfigured fetch\/publish or parse fails explicitly rather than silently
returning a fabricated success. It holds the handle slot in the composition root
where a configured backend is selected elsewhere.
-}
unconfiguredRegistry :: RegistryClient
unconfiguredRegistry =
    RegistryClient
        { fetchMetadata = const refuse
        , fetchArtifact = \_ _ -> refuse
        , publishArtifact = \_ _ _ -> refuse
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
backend is selected — for the mirror-target write, and for the private-upstream read
under the @service@ \/ @delegated-cache@ strategies. The default @passthrough@
strategy needs no read credential at all (reads forward the caller's own token), so
this empty placeholder is harmless on the serve path there. See
@docs\/architecture\/access-model.md@.
-}
unconfiguredCredentials :: CredentialProvider
unconfiguredCredentials =
    staticProvider AuthToken{authSecret = mkSecret "", authExpiresAt = Nothing}
