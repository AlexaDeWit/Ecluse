-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Écluse: a supply-chain policy proxy for package registries.

Écluse (package @ecluse@) sits between clients (developers, CI) and a package
registry. It applies a configurable resilience policy before any dependency reaches
a build, and it hosts no packages itself. The name is French for a canal lock: a
chamber whose gates never open at once. Every dependency is held and cleared
through that controlled passage before it enters a build.

The goal is __resilience, not malware detection__. Shrink the blast radius of a bad
publish (a hijacked maintainer account, a race-to-publish, a typosquat), rather than
promise to recognise malice.

Écluse is __not a registry__. The operator's own backend stores the packages (AWS
CodeArtifact, GCP Artifact Registry). Écluse governs only what may be fetched from,
and mirrored to, those backends. npm is the first ecosystem. The domain model is
ecosystem-agnostic, so PyPI and RubyGems can follow.

== How a request is cleared

Écluse speaks a registry's native protocol across three read-path registries: the
client's, a /private upstream/ of already-vetted packages, and the /public/
registry. The two request shapes use them differently:

* Écluse gates a __tarball__ request for that one version. It streams a
  private-upstream hit unfiltered, because it is already vetted. On a miss, the
  proxy fetches the version's public metadata and evaluates the rules. It then
  streams the version from public __and enqueues an asynchronous mirror job__, or
  returns a denial.
* A __packument__ (metadata) request is a /merge/. Écluse fetches the private and
  public upstreams in parallel, filters the public versions through the rules, and
  trusts the private ones. It then combines the two into one document. Private wins
  a version collision, Écluse flags an integrity divergence as a supply-chain
  signal, and @latest@ repoints to the newest survivor.

Two properties run through both shapes. The rules engine is __deny by default__: a
version is admitted only if some rule allows it and none denies it. __Mirroring is
demand-driven__, so Écluse mirrors only the versions a client actually pulls, never
on the request's critical path.

== How the code is organised

Écluse is a __functional core with effects at the edges__. The policy and protocol
logic is pure and easy to test, and a thin shell confines @IO@. Swappable
backends sit behind /handles/, records of functions chosen at a single composition
root. A new cloud or a new ecosystem is then one more implementation behind an
existing handle, not a structural change.

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
* __Supervision__: "Ecluse.Core.Supervision" (the one background-loop combinator every
  long-running task runs under) and, in this module, the typed process perimeter
  ('superviseProcess' and its 'exitCodeFor' table).

'run' is the entry point the @ecluse@ executable invokes (see "Main"). It lives
in the library, not in @app\/Main.hs@, so the composition root is a single
importable unit. @app\/Main.hs@ stays a thin shell that only calls it.

== Further reading

@docs\/architecture.md@ is the systems-design index: the vision, the end-to-end
request lifecycle, and a map to the per-concern design documents. @CONTRIBUTING.md@
covers the codebase layout and testing strategy, and @STYLE.md@ the coding and
documentation conventions.
-}
module Ecluse (
    -- * Entry point
    run,

    -- * The typed process supervisor
    ProcessOutcome (..),
    superviseProcess,
    exitCodeFor,

    -- * Split-ready services
    runServer,
    runWorker,

    -- * npm front door
    mountBindingFor,

    -- * Composition glue (exposed for direct testing)
    orExit,
    BootAborted (..),
) where

import Control.Exception (AsyncException (ThreadKilled, UserInterrupt), SomeAsyncException)
import Control.Exception qualified as Exception
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Ecluse.Boot
import Ecluse.CLI (AppCommand (..), execCLI)
import Ecluse.CheckConfig (runCheckConfig)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Dredger
import Ecluse.Pilot
import Ecluse.Proxy

run :: IO ()
run = do
    cmd <- execCLI
    outcome <- superviseProcess (runCommand cmd)
    case outcome of
        ServiceExited detail -> TIO.hPutStrLn stderr ("ecluse: service exited: " <> detail)
        RunCancelled -> TIO.hPutStrLn stderr "ecluse: run cancelled"
        _ -> pass
    exitWith (exitCodeFor outcome)

{- Dispatch one subcommand under the process perimeter. check-config runs outside
'withBootEnv': no logger, no telemetry, no services. -}
runCommand :: AppCommand -> IO ()
runCommand = \case
    RunCheckConfig -> runCheckConfig
    RunProxy -> withBootEnv runProxy
    RunPilot -> withBootEnv runPilot
    RunPilotCompile opts ->
        withBootEnv $ \bootEnv ->
            void (runPilotCompile (beLogEnv bootEnv) (beTelemetry bootEnv) (beS3Endpoint bootEnv) (beConfig bootEnv) opts)
    RunDredger -> withBootEnv runDredger

{- | How one whole service run ended. Each constructor owns one exit code ('exitCodeFor'), so
an orchestrator reads the ending from the status alone.
-}
data ProcessOutcome
    = -- | The services drained and returned (a graceful shutdown): exit 0.
      ShutdownRequested
    | -- | A service failed up with the carried rendered fault: exit 1.
      ServiceExited Text
    | {- | The boot aborted ('BootAborted'), after the boot phase reported its
      errors to standard error: exit 2.
      -}
      BootFault
    | -- | The run was cancelled from outside (a kill, an interrupt): exit 3.
      RunCancelled
    deriving stock (Eq, Show)

{- | Run the service under the typed process perimeter and classify its ending. The base
'Exception.try' and 'Exception.throwIO' are deliberate: what leaves here async must leave async.
-}
superviseProcess :: IO () -> IO ProcessOutcome
superviseProcess service =
    Exception.try service >>= \case
        Right () -> pure ShutdownRequested
        Left err
            | Just BootAborted <- fromException err -> pure BootFault
            | Just (code :: ExitCode) <- fromException err -> Exception.throwIO code
            | Just (killed :: AsyncException) <- fromException err ->
                pure $ case killed of
                    ThreadKilled -> RunCancelled
                    UserInterrupt -> RunCancelled
                    -- StackOverflow / HeapOverflow: resource exhaustion is a
                    -- fault of the run, not a cancellation.
                    other -> ServiceExited (displayExceptionT other)
            | Just (_ :: SomeAsyncException) <- fromException err -> Exception.throwIO err
            | otherwise -> pure (ServiceExited (displayExceptionT err))

-- | The process exit status each 'ProcessOutcome' owns.
exitCodeFor :: ProcessOutcome -> ExitCode
exitCodeFor = \case
    ShutdownRequested -> ExitSuccess
    ServiceExited _ -> ExitFailure 1
    BootFault -> ExitFailure 2
    RunCancelled -> ExitFailure 3
