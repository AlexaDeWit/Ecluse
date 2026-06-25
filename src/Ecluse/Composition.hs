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

    -- * Mirror-queue backend selection
    planMirrorQueue,

    -- * Publish-side wiring
    PublishTarget (..),
    planPublishTargets,

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
    QueueBackend (..),
    loadConfig,
    renderCredentialBackend,
    renderPolicyError,
    renderQueueBackend,
    unUrl,
 )
import Ecluse.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Ecosystem (Ecosystem, ecosystemName, prefixFor)
import Ecluse.Package.Integrity (MinIntegrity)
import Ecluse.Queue.Sqs (SqsConfig, defaultSqsConfig)
import Ecluse.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), lowerCaseHosts)
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
    | {- | The configured mirror-queue backend has no implementation compiled into
      this binary, so no queue can be built for it. Carries the unavailable backend.
      An honest refusal — never a silent fall-through to a different backend.
      -}
      QueueProviderUnavailable QueueBackend
    | {- | The SQS mirror-queue backend was selected but no AWS region was supplied
      (@AWS_REGION@), so the queue cannot be scoped to a region.
      -}
      QueueRegionMissing
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
    QueueProviderUnavailable backend ->
        "mirror queue provider "
            <> renderQueueBackend backend
            <> " is not available in this build"
    QueueRegionMissing ->
        "mirror queue provider "
            <> renderQueueBackend SqsQueue
            <> " requires AWS_REGION to be set"

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

    -- The response-bound budget every mount enforces on its upstream fetches and
    -- decodes (security.md invariant 4), assembled from the validated environment
    -- ceilings. Carried onto each mount's deps so the data plane reads the metadata
    -- body bounded, and refuses an over-deep or version-flooded document fail-closed.
    limits :: Limits
    limits =
        Limits
            { maxBodyBytes = cfgMaxResponseBytes (configEnv config)
            , maxVersionCount = cfgMaxVersionCount (configEnv config)
            , maxNestingDepth = cfgMaxNestingDepth (configEnv config)
            }

    -- The operator help message, derived from the environment layer like the
    -- inbound token, so every mount's denials carry it.
    helpMessage :: Maybe HelpMessage
    helpMessage = mkHelpMessage <$> cfgHelpMessage (configEnv config)

    -- The global public-integrity admission floor, validated at config load, carried
    -- onto every mount's deps so the public gate refuses a below-floor version.
    minIntegrity :: MinIntegrity
    minIntegrity = cfgMinPublicIntegrity (configEnv config)

    -- A mount's externally-visible base URL for the dist.tarball rewrite. Absolute
    -- under PROXY_PUBLIC_URL when set (so a served tarball is a full URL an npm
    -- client can fetch); otherwise the relative prefix path, retained for
    -- compatibility. A trailing slash on the configured URL is dropped so the join
    -- with the leading-slash mount path yields exactly one separator.
    mountBaseUrl :: Ecosystem -> Text
    mountBaseUrl eco =
        case cfgPublicUrl (configEnv config) of
            Nothing -> mountBasePath eco
            Just public -> T.dropWhileEnd (== '/') (unUrl public) <> mountBasePath eco

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
    mount's externally-visible base URL drives the @dist.tarball@ rewrite: an
    __absolute__ URL under @PROXY_PUBLIC_URL@ (@{public}\/npm\/{pkg}\/-\/{file}@)
    when one is configured, so an @npm@ client fetches the artifact back through the
    proxy on the gated path; otherwise the relative prefix path (@\/npm@), retained
    for compatibility — but note @npm@ cannot consume a relative @dist.tarball@ (it
    reads a leading slash as a @file:@ path), so a real install path must set
    @PROXY_PUBLIC_URL@ (see @mountBaseUrl@ and
    @docs\/architecture\/hosting.md@ → "URL rewriting"). -}
    packumentDepsFor :: Mount -> PackumentDeps
    packumentDepsFor mount =
        let regs = mountRegistries mount
         in PackumentDeps
                { pdPrivateBaseUrl = unUrl (regPrivateUpstream regs)
                , pdPublicBaseUrl = unUrl (regPublicUpstream regs)
                , pdMountBaseUrl = mountBaseUrl (mountEcosystem mount)
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
                , pdLimits = limits
                , pdInboundToken = inboundToken
                , pdNow = clock
                , pdHelp = helpMessage
                , pdMinIntegrity = minIntegrity
                }

-- The mount's externally-visible base path, derived from its ecosystem prefix
-- (@npm@ → @\/npm@): a leading slash and the prefix segments joined, so it is the
-- relative path a client's registry endpoint maps onto.
mountBasePath :: Ecosystem -> Text
mountBasePath eco = "/" <> T.intercalate "/" (toList (prefixFor eco))

-- ── publish-side wiring ───────────────────────────────────────────────────────

{- | One ecosystem's resolved __publish__ target: the mirror-target endpoint the
mirror worker writes approved artifacts to, paired with the credential provider
that mints its bearer token.

This is the publish side of the per-ecosystem composition (the serve side is the
mount's 'PackumentDeps'). The worker's single consumer builds a registry-protocol
client from these — the endpoint as its base URL, the provider's token as its
bearer — so the publish client is resolved here at the composition root rather than
re-derived per request.
-}
data PublishTarget = PublishTarget
    { ptEcosystem :: Ecosystem
    -- ^ The ecosystem this publish target serves.
    , ptMirrorUrl :: Text
    -- ^ The mirror-target endpoint approved artifacts are published to.
    , ptCredentials :: CredentialProvider
    -- ^ The provider minting the mirror-target write token.
    }

{- | Resolve each configured mount to its publish target, or the aggregated boot
errors. The publish side of 'planMounts': it validates the same config and resolves
each mount's mirror-target endpoint and write credential, so the worker's publish
client can be built at the composition root.

An unresolved credential reference is the same fail-loud boot error 'composeBindings'
reports for the serve side, so the two surfaces never disagree on what is wired.
-}
planPublishTargets ::
    CredentialProviders ->
    EnvConfig ->
    Maybe ConfigDoc ->
    Either [BootError] [PublishTarget]
planPublishTargets providers env mDoc =
    first (map PolicyBootError) (loadConfig env mDoc)
        >>= composePublishTargets providers

-- Resolve every mount's publish target from a validated config, aggregating an
-- unresolved-credential error per mount (the same check 'composeBindings' applies).
composePublishTargets ::
    CredentialProviders ->
    Config ->
    Either [BootError] [PublishTarget]
composePublishTargets providers config =
    case partitionEithers (map targetFor (Map.elems (configMounts config))) of
        ([], targets) -> Right targets
        (errs, _) -> Left (concat errs)
  where
    targetFor :: Mount -> Either [BootError] PublishTarget
    targetFor mount =
        let target = regMirrorTarget (mountRegistries mount)
            backend = mtCredential target
         in case lookupProvider backend providers of
                Just provider ->
                    Right
                        PublishTarget
                            { ptEcosystem = mountEcosystem mount
                            , ptMirrorUrl = unUrl (mtUrl target)
                            , ptCredentials = provider
                            }
                Nothing ->
                    Left [UnresolvedCredential (mountEcosystem mount) backend]

-- ── mirror-queue backend selection ────────────────────────────────────────────

{- | Select the mirror-queue backend from the environment layer, yielding the
'SqsConfig' the composition root builds the AWS SQS queue from, or the aggregated
boot errors that block it.

This is the pure half of the queue's backend choice — the single place that knows
which backends this binary can build. The AWS @sqs@ backend resolves to an
'SqsConfig' (the queue URL and region, with the provider knobs at their defaults);
the composition root passes that to @Ecluse.Queue.Sqs.newSqsQueue@, so the one SDK
constructor call lives there while the decision lives here. The GCP @pubsub@ arm is
recognised but not built, so it is a fail-loud 'QueueProviderUnavailable' boot error
rather than a silent fall-through; a missing @AWS_REGION@ under @sqs@ is a
'QueueRegionMissing' boot error. Errors are returned as a list so they aggregate
with the rest of the boot-time validation.
-}
planMirrorQueue :: EnvConfig -> Either [BootError] SqsConfig
planMirrorQueue env = case cfgQueueBackend env of
    PubSubQueue -> Left [QueueProviderUnavailable PubSubQueue]
    SqsQueue -> case T.strip <$> cfgAwsRegion env of
        Just region | not (T.null region) -> Right (defaultSqsConfig (unUrl (cfgQueueUrl env)) region)
        _ -> Left [QueueRegionMissing]

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
