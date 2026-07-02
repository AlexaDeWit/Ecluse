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

== How the code is organized

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
module Ecluse.Boot (
    BootEnv (..),
    withBootEnv,
    BootAborted (..),
    orExit,
    logBootWarning,
    logBootInfo,
    logRuleBootOrder,
    buildMirrorQueue,
) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Katip (Environment (Environment), LogEnv, Severity (InfoS, WarningS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import System.Environment (getEnvironment)
import System.IO.Error (isDoesNotExistError)
import UnliftIO (throwIO, tryJust)

import Ecluse.Composition (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    memoryQueueDropWarning,
    mirrorQueuePlanWarning,
 )
import Ecluse.Config (
    AppConfig (cfgCores, cfgLogFormat, cfgMaxHeapBytes, cfgTelemetry),
    Config (configApp),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Core.Queue (MirrorQueue, newBoundedInMemoryQueue)
import Ecluse.Core.Queue.Sqs (newSqsQueue)
import Ecluse.Core.Rules (renderBootOrder)
import Ecluse.Core.Server.Context (PackumentDeps (pdRules))
import Ecluse.Log (moduleField, newLogEnv)
import Ecluse.Runtime (applyRuntimePosture)
import Ecluse.Server (MountBinding (bindingPackumentDeps, bindingPrefix))
import Ecluse.Telemetry (Telemetry, TelemetrySwitch (TelemetryOff, TelemetryOn), withTelemetry)
import Ecluse.Telemetry.Resolve (prepareTelemetry)

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
data BootEnv = BootEnv
    { beConfig :: AppConfig
    , beLogEnv :: LogEnv
    , beTelemetry :: Telemetry
    , beConfigFull :: Config
    }

withBootEnv :: (BootEnv -> IO ()) -> IO ()
withBootEnv action = do
    envVars <- getEnvironment
    mDocBlob <- tryJust (guard . isDoesNotExistError) (BS.readFile "/etc/ecluse/config.yaml")
    let docBlob = either (const Nothing) Just mDocBlob
    config <- orExit (T.unlines . map renderConfigError) (loadConfig envVars docBlob)
    let env = configApp config
    logEnv <- newLogEnv (cfgLogFormat env) (Environment "production")
    -- Resolve and apply the runtime posture before anything else spins up: this may
    -- exec the binary in place (same PID; see Ecluse.Runtime) to enforce a heap
    -- ceiling, so nothing stateful must precede it beyond config and the logger.
    applyRuntimePosture (logBootInfo logEnv) (logBootWarning logEnv) (cfgCores env) (cfgMaxHeapBytes env)
    logBootInfo logEnv ("Loaded configuration: " <> show config)
    prepareTelemetryBoot (cfgTelemetry env) logEnv
    withTelemetry (cfgTelemetry env) logEnv $ \telemetry ->
        action
            BootEnv
                { beConfig = env
                , beLogEnv = logEnv
                , beTelemetry = telemetry
                , beConfigFull = config
                }

{- Build the config-selected mirror queue from its plan: the durable AWS SQS backend,
or the bounded in-memory backend. The in-memory arm first emits the loud boot warning
('mirrorQueuePlanWarning' -- it is non-durable / best-effort) through the
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
module -- the plain-'IO' katip path the boot phase uses (it holds no @Handler@ reader),
the same shape "Ecluse.Telemetry.Resolve" and "Ecluse.Core.Server.Pipeline.Internal" use. -}
logBootWarning :: LogEnv -> Text -> IO ()
logBootWarning logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM WarningS (ls message))

{- Log one line at 'InfoS' through the composition-root 'LogEnv', the same plain-'IO'
katip path 'logBootWarning' uses, for non-warning boot diagnostics. -}
logBootInfo :: LogEnv -> Text -> IO ()
logBootInfo logEnv message =
    runKatipContextT logEnv (moduleField "Ecluse") mempty (logFM InfoS (ls message))

{- Log every wired mount's resolved rule boot order ('renderBootOrder' -- the single
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
('Ecluse.Config.'): the static token or the
CodeArtifact mint. In the common case there is a single provider; the no-backend
placeholder only holds the slot when the selected provider was not built -- a mount
that references it has already failed the boot-time credential check by this point,
so the worker (the slot's only consumer) never reaches the placeholder. -}

{- | Raised to abort start-up after a boot phase has reported its aggregated
failure to stderr. A distinct type -- rather than a bare 'exitFailure' -- so the
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
when telemetry is off, so an unset @ECLUSE_TELEMETRY@ reads no process environment and
configures nothing. -}
prepareTelemetryBoot :: TelemetrySwitch -> LogEnv -> IO ()
prepareTelemetryBoot switch logEnv = case switch of
    TelemetryOff -> pass
    TelemetryOn -> do
        environment <- getEnvironment
        prepareTelemetry logEnv environment
