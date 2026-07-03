{- | Écluse -- a supply-chain resilience proxy for package registries.

Écluse (package @ecluse@) is a lightweight proxy that sits between consumers
(developers, CI) and a package registry, applying a configurable __resilience__
policy before any dependency reaches a build -- without taking on the cost of
hosting packages itself. The name is French for a canal lock: a chamber whose
gates never open at once. That is the posture -- not a wall that blocks, but a
controlled passage every dependency is held in and cleared through before it is
admitted to a build.

The goal is __resilience, not malware detection__: shrink the blast radius of a
bad publish -- a hijacked maintainer account, a race-to-publish, a typosquat --
rather than promise to recognise malice. And Écluse is __not a registry__:
storage is delegated to whatever backend the operator runs (e.g. AWS
CodeArtifact, GCP Artifact Registry), and Écluse only governs what may be fetched
from, and mirrored to, those backends. npm is the first ecosystem; the domain
model is deliberately ecosystem-agnostic so that PyPI and RubyGems can follow.

== How a request is cleared

Écluse speaks a registry's native protocol across three read-path registries --
the client's, a /private upstream/ of already-vetted packages, and the /public/
registry -- and the two request shapes use them differently:

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
-- a version is admitted only if some rule allows it and none denies it -- and
__mirroring is demand-driven__, so only versions actually pulled are mirrored,
and never on the request's critical path.

== How the code is organised

Écluse is a __functional core with effects at the edges__: the policy and
protocol logic is pure and trivially testable, and @IO@ is confined to a thin
shell. Swappable backends sit behind /handles/ -- records of functions chosen at a
single composition root -- so a new cloud or a new ecosystem is an added
implementation behind an existing handle, not a structural change.

The library's vocabulary, roughly from the pure core outward:

* __Domain model__ -- "Ecluse.Core.Package" (the ecosystem-agnostic package vocabulary
  the rules reason over), "Ecluse.Core.Version" (version identity and per-ecosystem
  ordering), and "Ecluse.Core.Ecosystem" (the ecosystem tag the rest dispatches on).
* __Policy__ -- "Ecluse.Core.Rules" (deny-by-default evaluation) over the rule types
  in "Ecluse.Core.Rules.Types".
* __Protocol boundary__ -- "Ecluse.Core.Registry" (the registry-protocol handle),
  "Ecluse.Core.Registry.Npm.Wire" and "Ecluse.Core.Registry.Npm.Project" (the lenient npm
  wire decoders and their projection onto the domain model),
  "Ecluse.Core.Registry.Npm.Route" (the npm path grammar), and "Ecluse.Core.Server.Route"
  (the shared serve-action 'Route' set and the injected route classifier).
* __Cloud handles__ -- "Ecluse.Core.Credential" (minting the mirror-target write token)
  and "Ecluse.Core.Queue" (the durable mirror-job hand-off to the worker).
* __Mirror worker__ -- "Ecluse.Core.Worker" (the supervised consume loop that fetches,
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
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Katip (katipAddNamespace)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import UnliftIO (concurrently_, throwIO)

import Ecluse.Boot
import Ecluse.Composition (
    PublishTarget (ptCredentials, ptEcosystem, ptMirrorUrl),
    connectionPoolSettings,
    initCredentialProviders,
    planMirrorQueue,
    planMounts,
    planPublishTargets,
    renderBootError,
 )
import Ecluse.Composition qualified as Composition
import Ecluse.Config (
    AppConfig (cfgPort, cfgPublicConnectionsPerHost, cfgServeMaxInFlight, cfgShutdownDrainTimeout),
 )
import Ecluse.Core.Credential (AuthToken (..), currentToken)
import Ecluse.Core.Credential.Refresh (CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), parseEcosystem, prefixFor)
import Ecluse.Core.Registry (
    ParseError (..),
    RegistryClient (..),
 )
import Ecluse.Core.Registry.Metadata (fetchVersionDetails)
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmPublishClient)
import Ecluse.Core.Registry.Npm.Route qualified as Npm
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Admission (newServeAdmission)
import Ecluse.Core.Server.Cache (Source (Source), newMetadataCache)
import Ecluse.Core.Server.Context (PackumentDeps, PublishDeps, pdLimits, pdNow, pdPublicBaseUrl, pdRules)
import Ecluse.Core.Server.Metadata (ManifestCaching (Cached), newNpmMetadataClient)
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint), Provider (CodeArtifact), Upstream (Public))
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (..), runWorkerM, workerLoop)
import Ecluse.Env (Env, envDdContext, envLogEnv, envManager, envMetadataCache, envMetrics, envTelemetry, newWorkerHeartbeat, withEnvWithAdmission, workerRuntimeOf)
import Ecluse.Server (MountBinding (..), ServerConfig (scDrainTimeout, scPort), ShutdownDrainTimeout (ShutdownDrainTimeout), mkServerConfig)
import Ecluse.Server qualified as Server
import Ecluse.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Telemetry.Instruments (metricsPortOf)
import Ecluse.Telemetry.Reporters (
    deferredBreakerReporter,
    deferredRefreshReporter,
    installMetrics,
    newDeferredMetrics,
 )
import Ecluse.Telemetry.Tracing (instrumentDataPlaneManagerSettings, tracingPortOf)

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

It assembles the composition root from configuration: it parses the environment
layer and the optional config document, __validates everything and fails fast at
boot__ on any problem (a malformed env, an unresolved rule policy, a configured
mount with no adapter, a credential reference that does not resolve, or a
mirror-queue backend that is not built in this binary), aggregating the failures so
a single run reports them all. On success it builds the handles -- the shared HTTP
@Manager@, the config-selected mirror queue, the metadata cache, the logger, the
process-global credential provider, and the telemetry substrate (off unless
@ECLUSE_TELEMETRY@ enables it) -- into an 'Env', derives the served mount bindings,
then runs the
server and the mirror worker __concurrently__ over that single 'Env' ('runServer'
and 'runWorker'). Bracketing the 'Env' (and the telemetry providers) for the
lifetime of both means their shared resources are torn down along every exit path.
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
    bindings <- planMounts mountBindingFor getCurrentTime providers config >>= orExit (T.unlines . map renderBootError)
    publishTargets <- orExit (T.unlines . map renderBootError) (planPublishTargets providers config)
    -- Select the mirror-queue backend from config (the GCP arm is a fail-loud
    -- "not built" boot error, never a silent fall-through); the resulting plan is
    -- handed to the one queue-construction site below.
    queuePlan <- orExit (T.unlines . map renderBootError) (planMirrorQueue env)
    -- The effective admission capacity: explicit config, else computed from the
    -- post-runtime-posture capability count, logged with its provenance beside the
    -- runtime lines. The private manager's pool below follows this value by
    -- construction (never a separate knob; see issue #634).
    capabilities <- getNumCapabilities
    let (serveMaxInFlight, admissionLine) = Composition.resolveServeAdmission (cfgServeMaxInFlight env) capabilities
    logBootInfo logEnv admissionLine
    serveAdmission <- newServeAdmission serveMaxInFlight
    let serverConfig =
            (mkServerConfig bindings)
                { scPort = cfgPort env
                , scDrainTimeout = ShutdownDrainTimeout (cfgShutdownDrainTimeout env)
                }
    -- Log each mount's resolved rule boot order so an operator sees at start-up exactly
    -- how their policy will resolve (highest precedence first, then name).
    logRuleBootOrder logEnv bindings
    -- The config-selected mirror queue, built once here (the single constructor
    -- call) from the validated plan and captured in Env: the durable AWS SQS backend,
    -- or the bounded in-memory backend -- which first emits a loud boot warning (it is
    -- non-durable / best-effort) and logs each rate-limited cap-overflow drop.
    queue <- buildMirrorQueue logEnv queuePlan
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
    manager <- newManager (connectionPoolSettings (cfgPublicConnectionsPerHost env) publicSettings)
    privateManager <- newManager (connectionPoolSettings serveMaxInFlight privateSettings)
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
        runServices serverConfig (workerPoliciesFor builtEnv bindings) builtEnv

{- Run the server and the mirror worker concurrently over one composition-root
'Env', the shape the single-process program uses. The two are independent (each
depends only on the handles in 'Env', not on each other), so splitting into
separate binaries later is two thin entry points calling 'runServer' \/
'runWorker' -- no rearchitecting. The server's settings (its derived mount bindings
and port) are supplied by the composition root and threaded to 'runServer'.
-}
runServices :: ServerConfig -> WorkerPolicies -> Env -> IO ()
runServices serverConfig policies env = concurrently_ (runServer serverConfig env) (runWorker policies env)

{- | Run the proxy's HTTP front door over the composition-root 'Env' with the
config-derived 'ServerConfig'.

This is the npm-aware composition site: 'mountBindingFor' mounts npm -- its path
grammar ("Ecluse.Core.Registry.Npm.Route") and its denial renderer
("Ecluse.Core.Registry.Npm.Serve") -- into the otherwise ecosystem-neutral web layer
('Ecluse.Server.runServer'), so the agnostic server stays closed over the shared
'Ecluse.Core.Server.Route.Route' set and only this one place names an ecosystem.
Splitting the server into its own binary later reuses this same entry.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env = Server.runWarp cfg (Server.tracedApplication cfg env)

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
('prefixFor') rather than configured -- so the ecosystem is the single thing that
drives the binding (see @docs\/architecture\/hosting.md@ → "Mounts"). The
packument-serve dependencies are passed in (the composition root supplies them once
the per-mount registry set is resolved); 'Nothing' for them leaves the packument
route the recognised-but-unserved @501@ stub.

npm is the only ecosystem with an adapter; the others have no registry
client or renderer, so they resolve to 'Nothing' -- a loud miss at the call
site rather than a silently half-wired mount.
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
('Ecluse.Env.workerRuntimeOf'). The bundles carry the same prepared rules and public origin
the serve path gates with, so the worker re-runs current policy against a job before
mirroring it.

This is the composition-root __hoist point__: it resolves the request-independent @dd@
correlation object (the service identity; no span is active at the worker entry) and
installs it as the worker's initial @katip@ context, then discharges the loop to 'IO'
through 'Ecluse.Core.Worker.runWorkerM' -- the worker analogue of the serve path's
'Ecluse.Core.Server.Context.runHandler' boundary. The loop logic lives in
"Ecluse.Core.Worker"; the single-process program runs this alongside 'runServer'.
-}
runWorker :: WorkerPolicies -> Env -> IO ()
runWorker policies env = do
    dd <- ddPayloadNow (envDdContext env)
    runWorkerM (envLogEnv env) dd (workerRuntimeOf policies env) (katipAddNamespace "worker" workerLoop)

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

{- Build one mount's worker re-evaluation bundle from its packument-serve dependencies: the
single-version resolver over the guarded public origin through the shared metadata cache
(the same fetch-and-project the serve path runs, so the ingest decision does not diverge
from the serve decision), the mount's prepared rules, and its injected clock. The metadata
client is anonymous (no client credential reaches the public origin) and reuses the guarded
data-plane manager, so the worker's re-fetch inherits the resolved-IP SSRF recheck. Its own
failure and dropped-entry logs are elided (the worker logs its own re-evaluation outcome per
job), while the upstream-fetch metrics still record through the shared instruments. -}
workerPolicyFor :: Env -> PackumentDeps -> WorkerPolicy
workerPolicyFor env deps =
    WorkerPolicy
        { wpResolveVersion = fetchVersionDetails client
        , wpRules = pdRules deps
        , wpNow = pdNow deps
        }
  where
    client =
        newNpmMetadataClient
            (tracingPortOf (envTelemetry env))
            (metricsPortOf (envMetrics env))
            Public
            (Cached (envMetadataCache env) (Source (pdPublicBaseUrl deps)))
            (\_ _ -> pure ())
            (\_ _ -> pure ())
            (\_ -> pure ())
            NpmClientConfig
                { npmBaseUrl = pdPublicBaseUrl deps
                , npmManager = envManager env
                , npmToken = Nothing
                , npmLimits = pdLimits deps
                }

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
with no backend wired in -- a composition-root misconfiguration. A distinct typed
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
backend is selected -- for the mirror-target write, and for the private-upstream read
under the @service@ \/ @delegated-cache@ strategies. The default @passthrough@
strategy needs no read credential at all (reads forward the caller's own token), so
this empty placeholder is harmless on the serve path there. See
@docs\/architecture\/access-model.md@.
-}
