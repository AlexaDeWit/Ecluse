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
    case cmd of
        -- check-config validates and prints without booting anything, and owns
        -- its own exit codes (0 valid, 2 refused): no services, no supervision.
        RunCheckConfig -> runCheckConfig
        serviceCmd -> do
            outcome <- superviseProcess (withBootEnv (dispatch serviceCmd))
            case outcome of
                ServiceExited detail -> TIO.hPutStrLn stderr ("ecluse: service exited: " <> detail)
                RunCancelled -> TIO.hPutStrLn stderr "ecluse: run cancelled"
                _ -> pass
            exitWith (exitCodeFor outcome)
  where
    dispatch cmd bootEnv = case cmd of
        RunProxy -> runProxy bootEnv
        RunPilot -> runPilot bootEnv
        RunPilotCompile opts -> void (runPilotCompile (beLogEnv bootEnv) (beTelemetry bootEnv) (beAmbient bootEnv) (beConfig bootEnv) opts)
        RunDredger -> runDredger bootEnv
        RunCheckConfig -> pass

{- | How one whole service run ended: the typed outer perimeter of the process.
Each constructor owns one exit code ('exitCodeFor'), so an orchestrator reads the
ending from the status alone.
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

{- | Run the whole service under the typed process perimeter and classify its
ending as a 'ProcessOutcome'. This is the one place that reads the process's
exception channel, so nothing above it interprets exceptions.

The classification, in order:

* A normal return is 'ShutdownRequested' (warp's graceful drain returns).
* 'BootAborted' is 'BootFault'.
* An 'ExitCode' rethrows, so a deliberate exit request keeps its code, the
  local-dev halt's 130 included.
* The recognised kill deliveries ('ThreadKilled', 'UserInterrupt') are
  'RunCancelled'.
* Any __other__ asynchronous exception is not ours to interpret and propagates, so
  a 'System.Timeout.timeout' or an @async@ cancellation a test wraps around 'run'
  keeps its own semantics.
* Every remaining synchronous escape is 'ServiceExited' with its rendered detail.

This is the one deliberate base-'Exception.try' in the codebase: the process
perimeter must observe asynchronous delivery to classify a kill. The
async-hygienic @unliftio@ catches refuse to hand it over on purpose, because they
would rethrow the kill and the classification arm could never run. The rethrows go
through the base 'Exception.throwIO' for the same reason: what leaves here
async must leave async.
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
