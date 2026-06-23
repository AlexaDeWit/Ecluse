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
shell. Swappable backends sit behind /handles/ — records of functions chosen at a
single composition root — so a new cloud or a new ecosystem is an added
implementation behind an existing handle, not a structural change.

The library's vocabulary, roughly from the pure core outward:

* __Domain model__ — "Ecluse.Package" (the ecosystem-agnostic package vocabulary
  the rules reason over), "Ecluse.Version" (version identity and per-ecosystem
  ordering), and "Ecluse.Ecosystem" (the ecosystem tag the rest dispatches on).
* __Policy__ — "Ecluse.Rules" (deny-by-default evaluation) over the rule types
  in "Ecluse.Rules.Types".
* __Protocol boundary__ — "Ecluse.Registry" (the registry-protocol handle),
  "Ecluse.Registry.Npm.Wire" and "Ecluse.Registry.Npm.Project" (the lenient npm
  wire decoders and their projection onto the domain model),
  "Ecluse.Registry.Npm.Route" (the npm path grammar), and "Ecluse.Server.Route"
  (the shared serve-action 'Route' set and the injected route classifier).
* __Cloud handles__ — "Ecluse.Credential" (minting the mirror-target write token)
  and "Ecluse.Queue" (the durable mirror-job hand-off to the worker).

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

    -- * Default handles
    unconfiguredRegistry,
    unconfiguredCredentials,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Katip (Environment (Environment))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Error (userError)
import UnliftIO (concurrently_, throwIO)

import Ecluse.Composition (
    CredentialProviders,
    initCredentialProviders,
    planMounts,
    renderBootError,
 )
import Ecluse.Composition qualified as Composition
import Ecluse.Config (
    ConfigDoc,
    CredentialBackend (StaticCredential),
    EnvConfig (cfgCacheMaxEntries, cfgCacheTtl, cfgHelpMessage, cfgLogFormat, cfgPort),
    decodeDocument,
    parseEnv,
    renderEnvErrors,
 )
import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm), prefixFor)
import Ecluse.Env (Env, withEnv)
import Ecluse.Log (newLogEnv)
import Ecluse.Queue (newInMemoryQueue)
import Ecluse.Registry (
    ParseError (..),
    RegistryClient (..),
 )
import Ecluse.Registry.Npm.Route qualified as Npm
import Ecluse.Registry.Npm.Serve (npmRenderer)
import Ecluse.Server (MountBinding (..), ServerConfig (scPort), mkServerConfig)
import Ecluse.Server qualified as Server
import Ecluse.Server.Cache (CacheConfig (CacheConfig, cacheMaxEntries, cacheTtl), newMetadataCache)
import Ecluse.Server.Context (PackumentDeps)
import Ecluse.Server.Response (mkHelpMessage)

{- | Start Écluse: the entry point the @ecluse@ executable runs (see "Main").

It assembles the composition root from configuration: it parses the environment
layer and the optional config document, __validates everything and fails fast at
boot__ on any problem (a malformed env, an unresolved rule policy, a configured
mount with no adapter, a credential reference that does not resolve), aggregating
the failures so a single run reports them all. On success it builds the handles —
the shared HTTP @Manager@, the metadata cache, the logger, and the
process-global credential provider — into an 'Env', derives the served mount
bindings, then runs the server and the mirror worker __concurrently__ over that
single 'Env' ('runServer' and 'runWorker'). Bracketing the 'Env' for the lifetime
of both means their shared resources are torn down along every exit path.
-}
run :: IO ()
run = do
    env <- parseEnv >>= orExit renderEnvErrors
    mDoc <- loadDocument
    providers <- initCredentialProviders env
    let help = mkHelpMessage <$> cfgHelpMessage env
    bindings <- orExit (T.unlines . map renderBootError) (planMounts mountBindingFor getCurrentTime help providers env mDoc)
    let serverConfig = (mkServerConfig bindings){scPort = cfgPort env}
    manager <- HTTP.newManager tlsManagerSettings
    queue <- newInMemoryQueue
    metadataCache <- newMetadataCache (cacheConfigFor env)
    logEnv <- newLogEnv (cfgLogFormat env) (Environment "production")
    withEnv unconfiguredRegistry queue (mirrorWriteProvider providers) manager metadataCache logEnv (runServices serverConfig)

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

{- The metadata-cache tunables drawn from the validated environment layer (TTL and
the entry bound), so a deployment's cache settings flow from config rather than the
built-in defaults. -}
cacheConfigFor :: EnvConfig -> CacheConfig
cacheConfigFor env =
    CacheConfig
        { cacheTtl = cfgCacheTtl env
        , cacheMaxEntries = cfgCacheMaxEntries env
        }

{- The process-global mirror-write credential provider stored in 'Env' for the
worker. In the collapses-to-one common case there is a single provider; the
@static@ leaf is selected when a static write token is configured, else the
no-backend placeholder holds the slot (the worker — its only consumer — is a stub
in this build; the per-ecosystem worker-publish wiring lands with the worker
slice). A mount that references an uninitialized provider has already failed the
boot-time credential check by this point. -}
mirrorWriteProvider :: CredentialProviders -> CredentialProvider
mirrorWriteProvider providers =
    fromMaybe unconfiguredCredentials (Composition.lookupProvider StaticCredential providers)

{- Exit the process with the rendered failure when a boot phase fails, otherwise
yield its value. The aggregated failure block is written to stderr so an operator
sees every problem from a single failed launch. -}
orExit :: (e -> Text) -> Either e a -> IO a
orExit render = \case
    Right a -> pure a
    Left err -> do
        TIO.hPutStrLn stderr (render err)
        exitFailure

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
grammar ("Ecluse.Registry.Npm.Route") and its denial renderer
("Ecluse.Registry.Npm.Serve") — into the otherwise ecosystem-neutral web layer
('Ecluse.Server.runServer'), so the agnostic server stays closed over the shared
'Ecluse.Server.Route.Route' set and only this one place names an ecosystem.
Splitting the server into its own binary later reuses this same entry.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer = Server.runServer

{- | The fallback server settings: a single npm mount with __no__ packument-serve
dependencies, so the packument route is the recognised-but-unserved @501@ stub.
Exposed so the composed front door can be driven directly without binding a socket
(e.g. embedded in another @wai@ application, or exercised in tests through
'Ecluse.Server.application') to assert the routing and the unwired-mount surface; a
real launch derives its bindings from configuration in 'run'.
-}
npmServerConfig :: ServerConfig
npmServerConfig = mkServerConfig [npmMount Nothing]

{- | Resolve an 'Ecosystem' to its complete 'MountBinding', or 'Nothing' when that
ecosystem has no adapter wired in this build. The ecosystem selects its path
grammar (the 'Ecluse.Server.Route.Classifier') and its denial renderer (the
'Ecluse.Server.Response.MountRenderer'), and its path prefix is __derived__ from it
('prefixFor') rather than configured — so the ecosystem is the single thing that
drives the binding (see @docs\/architecture\/hosting.md@ → "Mounts"). The
packument-serve dependencies are passed in (the composition root supplies them once
the per-mount registry set is resolved); 'Nothing' for them leaves the packument
route the recognised-but-unserved @501@ stub.

npm is the only ecosystem with an adapter at launch; the others have no registry
client or renderer yet, so they resolve to 'Nothing' — a loud miss at the call
site rather than a silently half-wired mount.
-}
mountBindingFor :: Ecosystem -> Maybe PackumentDeps -> Maybe MountBinding
mountBindingFor eco packumentDeps = case eco of
    Npm -> Just (npmMount packumentDeps)
    _ -> Nothing

{- The npm mount: npm's complete wiring under its derived @\/npm@ prefix — its path
grammar and its denial renderer — taking the packument-serve dependencies the
composition root supplies ('Nothing' leaves the packument route the
recognised-but-unserved @501@ stub).
-}
npmMount :: Maybe PackumentDeps -> MountBinding
npmMount packumentDeps =
    MountBinding
        { bindingPrefix = prefixFor Npm
        , bindingClassifier = Npm.classify
        , bindingPackumentDeps = packumentDeps
        , bindingRenderer = npmRenderer
        }

{- | Run the supervised mirror worker over the composition-root 'Env': the
consume → fetch → verify → publish → ack loop against the queue and credential
handles, in the @App@ orchestration monad.
-}
runWorker :: Env -> IO ()
runWorker _env = pass

{- | A registry handle with no backend behind it: every effectful field __refuses
loudly__ ('throwIO') and every pure @parse*@ field returns 'Left', so an
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
        , parsePackageInfo = const (Left notConfigured)
        , parseVersionDetails = \_ _ -> Left notConfigured
        , parseVersionList = const (Left notConfigured)
        }
  where
    refuse :: IO a
    refuse = throwIO (userError "registry: no backend configured")

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
