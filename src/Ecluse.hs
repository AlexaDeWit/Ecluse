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

Écluse speaks a registry's native protocol across three registries — the
client's, a /private upstream/ of already-vetted packages, and the /public/
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
shell. Swappable backends sit behind /seams/ — records of functions chosen at a
single composition root — so a new cloud or a new ecosystem is an added
implementation behind an existing seam, not a structural change.

The library's vocabulary, roughly from the pure core outward:

* __Domain model__ — "Ecluse.Package" (the ecosystem-agnostic package vocabulary
  the rules reason over), "Ecluse.Version" (version identity and per-ecosystem
  ordering), and "Ecluse.Ecosystem" (the ecosystem tag the rest dispatches on).
* __Policy__ — "Ecluse.Rules" (deny-by-default evaluation) over the rule types
  in "Ecluse.Rules.Types".
* __Protocol boundary__ — "Ecluse.Registry" (the registry-protocol seam),
  "Ecluse.Registry.Npm.Wire" (the lenient npm wire decoders that project onto
  the domain model), and "Ecluse.Server.Route" (the pure npm request router).
* __Cloud seams__ — "Ecluse.Credential" (minting the mirror-target write token)
  and "Ecluse.Queue" (the durable mirror-job hand-off to the worker).

'run' is the entry point the @ecluse@ executable invokes (see "Main"). It lives
in the library, not in @app\/Main.hs@, so the program's behaviour is exercised
by the test suite rather than hidden in the executable.

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

    -- * Default seams
    unconfiguredRegistry,
    unconfiguredCredentials,
) where

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Error (userError)
import UnliftIO (concurrently_, throwIO)

import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Env (Env, withEnv)
import Ecluse.Queue (newInMemoryQueue)
import Ecluse.Registry (
    ParseError (..),
    RegistryClient (..),
 )

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

It assembles the composition root — the seams plus a shared HTTP @Manager@ — into
an 'Env', then runs the server and the mirror worker __concurrently__ over that
single 'Env' ('runServer' and 'runWorker'). Bracketing the 'Env' for the
lifetime of both means their shared resources are torn down along every exit
path.
-}
run :: IO ()
run = do
    manager <- HTTP.newManager tlsManagerSettings
    queue <- newInMemoryQueue
    withEnv unconfiguredRegistry queue unconfiguredCredentials manager runServices

{- | Run the server and the mirror worker concurrently over one composition-root
'Env', the shape the single-process program uses. The two are independent (each
depends only on the seams in 'Env', not on each other), so splitting into
separate binaries later is two thin entry points calling 'runServer' \/
'runWorker' — no rearchitecting.
-}
runServices :: Env -> IO ()
runServices env = concurrently_ (runServer env) (runWorker env)

{- | Serve the proxy's HTTP front door over the composition-root 'Env'. Request
handlers read the 'Env' in plain 'IO'.

The WAI @Application@, routing, and Warp listener are not yet wired here; this
is a minimal stub.
-}
runServer :: Env -> IO ()
runServer _env = pass

{- | Run the supervised mirror worker over the composition-root 'Env': the
consume → fetch → verify → publish → ack loop against the queue and credential
seams, in the @App@ orchestration monad.

The consume loop and its health signal are not yet wired here; this is a
minimal stub.
-}
runWorker :: Env -> IO ()
runWorker _env = pass

{- | A registry seam with no backend behind it: every effectful field __refuses
loudly__ ('throwIO') and every pure @parse*@ field returns 'Left', so an
unconfigured fetch\/publish or parse fails explicitly rather than silently
returning a fabricated success. It holds the seam slot in the composition root
where a configured backend is selected elsewhere.
-}
unconfiguredRegistry :: RegistryClient
unconfiguredRegistry =
    RegistryClient
        { fetchMetadata = const refuse
        , fetchArtifact = \_ _ -> refuse
        , publishArtifact = \_ _ _ -> refuse
        , parsePackageInfo = const (Left notConfigured)
        , parseVersionDetails = \_ _ -> Left notConfigured
        , parseVersionList = const (Left notConfigured)
        }
  where
    refuse :: IO a
    refuse = throwIO (userError "registry: no backend configured")

    notConfigured :: ParseError
    notConfigured = ParseError{parseErrorMessage = "no registry backend configured"}

{- | A credential seam with no backend behind it: a static, non-expiring empty
secret. Credentials are mirror-write only and never read on the serve path, so
this holds the seam slot in the composition root without minting a live token.
-}
unconfiguredCredentials :: CredentialProvider
unconfiguredCredentials =
    staticProvider AuthToken{authSecret = mkSecret "", authExpiresAt = Nothing}
