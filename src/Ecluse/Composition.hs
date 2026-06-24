{- | The composition-root wiring: turn a validated 'Config' and the
process-global credential providers into the served 'MountBinding's, failing fast
and __aggregated__ on any boot problem.

This is the pure, IO-free heart of the composition root ("Ecluse" calls it): it
holds no sockets, no network, and no real clock of its own — the clock and the
ecosystem-to-adapter resolver are injected — so the boot-time validation is
unit-tested without opening a listener, mirroring how 'Ecluse.Env' assembly is
kept pure of IO.

== Global providers, per-mount reference

A 'Ecluse.Credential.CredentialProvider' is the service's own cloud identity,
built __once__ from the environment layer ('initCredentialProviders') and held
process-global; a mount does not carry a provider, it only __names__ which backend
it draws on (its @mtCredential@). The boot-time check is the resolution of that
reference: every distinct credential backend named across all mounts must resolve
to an __initialized__ provider, or the app halts at boot (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider"). Only the
@static@ backend has a leaf (from @MIRROR_TARGET_TOKEN@); a mount naming
@codeartifact@ or @adc@ resolves to no provider and is an honest boot failure.

== Fail-fast at boot

Three boot failures are aggregated into one report so a single run shows every
problem: a rule policy that does not resolve ('PolicyBootError', surfaced by
'Ecluse.Config.loadConfig'), a configured mount whose ecosystem has no adapter
wired ('MissingAdapter'), and a mount naming a credential backend with no
initialized provider ('UnresolvedCredential'). A bad configuration is thus a
loud, immediate startup failure, never a quietly mis-enforced or half-wired state
(see @docs\/architecture\/configuration.md@ → "Validation").
-}
module Ecluse.Composition (
    -- * Global credential providers
    CredentialProviders,
    initCredentialProviders,
    initializedBackends,
    lookupProvider,

    -- * Boot-time wiring
    BootError (..),
    renderBootError,
    planMounts,
    composeBindings,

    -- * Config-derived runtime settings
    cacheConfigFor,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Config (
    Config (..),
    ConfigDoc,
    CredentialBackend (..),
    EnvConfig (..),
    MirrorTarget (mtCredential, mtUrl),
    Mount (..),
    MountRegistries (..),
    PolicyError,
    loadConfig,
    renderCredentialBackend,
    renderPolicyError,
    unUrl,
 )
import Ecluse.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Ecosystem (Ecosystem, ecosystemName, prefixFor)
import Ecluse.Security (TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), lowerCaseHosts)
import Ecluse.Server.Cache (CacheConfig (..))
import Ecluse.Server.Context (MountBinding, PackumentDeps (..))
import Ecluse.Server.Response (HelpMessage, mkHelpMessage)

-- ── global credential providers ───────────────────────────────────────────────

{- | The process-global credential providers, keyed by the backend they
implement. Built __once__ at the composition root from the environment layer; a
mount references one by name and never holds its own.

The keyset (see 'initializedBackends') is the boot-check's pure surface — a mount
that names a backend absent from it has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map CredentialBackend CredentialProvider)

{- | Build the global credential providers from the environment layer. Only the
backends whose leaf can be constructed from ambient credentials are
initialized:

* @static@ — built from @MIRROR_TARGET_TOKEN@ ('cfgMirrorTargetToken') when set;
  absent, no static provider is initialized, so a mount naming @static@ fails the
  boot-time credential-reference check.

@codeartifact@ and @adc@ have no mint leaf, so they are deliberately __not__
initialized here: a mount naming one is an honest boot failure — the unresolved
credential reference the boot check rejects — which is the fail-fast mechanism
working, not a gap.
-}
initCredentialProviders :: EnvConfig -> IO CredentialProviders
initCredentialProviders env =
    pure (CredentialProviders (Map.fromList (catMaybes [staticEntry])))
  where
    -- The static provider, when a static write token is supplied. A static token
    -- never expires, so the in-memory 'staticProvider' leaf is the whole policy.
    staticEntry :: Maybe (CredentialBackend, CredentialProvider)
    staticEntry = do
        token <- cfgMirrorTargetToken env
        pure
            ( StaticCredential
            , staticProvider AuthToken{authSecret = token, authExpiresAt = Nothing}
            )

{- | The set of credential backends that resolved to an initialized provider — the
pure surface the boot-time credential-reference check reasons over.
-}
initializedBackends :: CredentialProviders -> Set CredentialBackend
initializedBackends (CredentialProviders ps) = Map.keysSet ps

{- | Look up the initialized provider for a backend, 'Nothing' when none is
initialized (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: CredentialBackend -> CredentialProviders -> Maybe CredentialProvider
lookupProvider backend (CredentialProviders ps) = Map.lookup backend ps

-- ── boot-time wiring ──────────────────────────────────────────────────────────

{- | A reason the composition root refuses to start. Every case is a __fail-loud__
boot failure; they are aggregated so a single run reports every problem an
operator must fix.
-}
data BootError
    = -- | A rule policy did not resolve (surfaced by 'loadConfig').
      PolicyBootError PolicyError
    | {- | A configured mount's ecosystem has no adapter wired, so it
      cannot be served (a loud miss, never a silent drop). Carries the ecosystem.
      -}
      MissingAdapter Ecosystem
    | {- | A mount names a credential backend with no initialized provider. Carries
      the ecosystem of the mount and the unresolved backend.
      -}
      UnresolvedCredential Ecosystem CredentialBackend
    deriving stock (Eq, Show)

-- | Render a 'BootError' as a human-facing line for the aggregated failure block.
renderBootError :: BootError -> Text
renderBootError = \case
    PolicyBootError err -> renderPolicyError err
    MissingAdapter eco ->
        "mount " <> ecosystemName eco <> " has no adapter wired in this build"
    UnresolvedCredential eco backend ->
        "mount "
            <> ecosystemName eco
            <> " names credential source "
            <> renderCredentialBackend backend
            <> ", not initialized in this build"

{- | Validate the environment layer and optional document into the served mount
bindings, or the aggregated boot errors. The composition root's single entry: it
runs 'loadConfig' (whose policy errors become 'PolicyBootError's) and then
'composeBindings', so policy, missing-adapter, and unresolved-credential failures
all surface from one call.

The ecosystem-to-adapter resolver and the wall-clock source are injected (the
composition root supplies @mountBindingFor@ and 'Data.Time.getCurrentTime'), so
this validation is pure of IO and unit-testable without a socket.
-}
planMounts ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    CredentialProviders ->
    EnvConfig ->
    Maybe ConfigDoc ->
    Either [BootError] [MountBinding]
planMounts resolveAdapter clock providers env mDoc =
    first (map PolicyBootError) (loadConfig env mDoc)
        >>= composeBindings resolveAdapter clock providers

{- | Turn a validated 'Config' into the served 'MountBinding's, or the aggregated
boot errors. For each mount, in ecosystem order: its credential reference must
resolve to an initialized provider, and its ecosystem must resolve to an adapter
(through the injected resolver, fed real 'PackumentDeps' so the packument route is
served rather than the @501@ stub). Errors aggregate across every mount.
-}
composeBindings ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    CredentialProviders ->
    Config ->
    Either [BootError] [MountBinding]
composeBindings resolveAdapter clock providers config =
    case partitionEithers (map bindingFor (Map.elems (configMounts config))) of
        ([], bindings) -> Right bindings
        (errs, _) -> Left (concat errs)
  where
    inboundToken :: Maybe Secret
    inboundToken = cfgAuthToken (configEnv config)

    -- The resolved tarball-host policy for every mount, from the secure-default
    -- environment toggle: honour a cross-host dist.tarball only when explicitly
    -- opted in (and even then, never past the allowlist or the internal block).
    tarballHostPolicy :: TarballHostPolicy
    tarballHostPolicy =
        if cfgRespectUpstreamTarballHost (configEnv config)
            then AnyAllowlistedHost
            else SameHostAsPackument

    -- The operator help message, derived from the environment layer like the
    -- inbound token, so every mount's denials carry it.
    helpMessage :: Maybe HelpMessage
    helpMessage = mkHelpMessage <$> cfgHelpMessage (configEnv config)

    {- Resolve one mount to its binding, or the boot errors that block it. Both the
    credential reference and the adapter are checked even when one already failed,
    so a mount missing both reports both in one run rather than one at a time. -}
    bindingFor :: Mount -> Either [BootError] MountBinding
    bindingFor mount =
        case (credentialError mount, resolveAdapter (mountEcosystem mount) (Just (packumentDepsFor mount))) of
            (Nothing, Just binding) -> Right binding
            (mCredErr, mBinding) ->
                Left (maybeToList mCredErr <> [MissingAdapter (mountEcosystem mount) | isNothing mBinding])

    -- The credential reference of a mount: an error when the named backend is not
    -- initialized, nothing when it resolves.
    credentialError :: Mount -> Maybe BootError
    credentialError mount =
        let backend = mtCredential (regMirrorTarget (mountRegistries mount))
         in if backend `Set.member` initializedBackends providers
                then Nothing
                else Just (UnresolvedCredential (mountEcosystem mount) backend)

    {- Build a mount's 'PackumentDeps' from its registries, resolved rules, the
    inbound edge token, the injected clock, and the operator help message. The
    mount's externally-visible base URL is its derived prefix path (@\/npm@): a
    relative base, so a rewritten @dist.tarball@ (@\/npm\/{pkg}\/-\/{file}@)
    resolves against the registry endpoint a client is pointed at — the proxy
    mount itself — keeping artifact fetches on the gated path without a separately
    configured public base (see @docs\/architecture\/hosting.md@ → "URL rewriting"). -}
    packumentDepsFor :: Mount -> PackumentDeps
    packumentDepsFor mount =
        let regs = mountRegistries mount
         in PackumentDeps
                { pdPrivateBaseUrl = unUrl (regPrivateUpstream regs)
                , pdPublicBaseUrl = unUrl (regPublicUpstream regs)
                , pdMountBaseUrl = mountBasePath (mountEcosystem mount)
                , pdMirrorTarget = unUrl (mtUrl (regMirrorTarget regs))
                , pdRules = mountPolicy mount
                , -- No effectful rule type is wired into the policy model yet, so the
                  -- effectful tier is empty here and gating reduces to the pure tier.
                  pdEffectfulRules = []
                , pdTarballHostPolicy = tarballHostPolicy
                , -- The internal-range opt-in for an honoured tarball host is empty —
                  -- the composition root's secure default, matching the guarded
                  -- manager's resolved-IP recheck (built with an empty opt-in too).
                  pdAllowedInternalHosts = lowerCaseHosts mempty
                , pdInboundToken = inboundToken
                , pdNow = clock
                , pdHelp = helpMessage
                }

-- The mount's externally-visible base path, derived from its ecosystem prefix
-- (@npm@ → @\/npm@): a leading slash and the prefix segments joined, so it is the
-- relative path a client's registry endpoint maps onto.
mountBasePath :: Ecosystem -> Text
mountBasePath eco = "/" <> T.intercalate "/" (toList (prefixFor eco))

-- ── config-derived runtime settings ───────────────────────────────────────────

{- | The metadata-cache tunables drawn from the validated environment layer — its
TTL and entry bound — so a deployment's cache settings flow from config rather than
the built-in defaults (see "Ecluse.Server.Cache").
-}
cacheConfigFor :: EnvConfig -> CacheConfig
cacheConfigFor env =
    CacheConfig
        { cacheTtl = cfgCacheTtl env
        , cacheMaxEntries = cfgCacheMaxEntries env
        }
