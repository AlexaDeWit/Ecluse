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

    -- * Mirror-target credential provider selection
    planMirrorCredential,
    resolveCodeArtifactConfig,

    -- * Boot-time wiring
    BootError (..),
    renderBootError,
    planMounts,
    composeBindings,

    -- * Mirror-queue backend selection
    MirrorQueuePlan (..),
    planMirrorQueue,
    mirrorQueuePlanWarning,
    memoryQueueBootWarning,
    memoryQueueDropWarning,

    -- * Publish-side wiring
    PublishTarget (..),
    planPublishTargets,

    -- * Config-derived runtime settings
    cacheConfigFor,
) where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import UnliftIO (tryAny)

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
    renderMirrorCredentialProvider,
    renderPolicyError,
    renderQueueBackend,
    unUrl,
 )
import Ecluse.Credential (AuthToken (..), CredentialProvider, Secret, mkSecret, staticProvider)
import Ecluse.Credential.CodeArtifact (CodeArtifactConfig (..), newCodeArtifactProvider)
import Ecluse.Ecosystem (Ecosystem, ecosystemName, prefixFor)
import Ecluse.Package.Integrity (MinIntegrity)
import Ecluse.Queue (MemoryQueueConfig, defaultMemoryQueueConfig)
import Ecluse.Queue.Sqs (SqsConfig (sqsEndpoint), SqsEndpoint (..), defaultSqsConfig)
import Ecluse.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), hostAddress, lowerCaseHosts)
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

{- | Build the global credential providers from the environment layer, or the
aggregated boot errors that block them. The mirror-target write provider is
selected by 'cfgMirrorTargetCredentialProvider' (see 'planMirrorCredential'):

* @static@ — built from @MIRROR_TARGET_TOKEN@ ('cfgMirrorTargetToken') when set;
  absent, no static provider is initialized, so a mount naming @static@ fails the
  boot-time credential-reference check.
* @codeartifact@ — the CodeArtifact inputs are resolved
  ('resolveCodeArtifactConfig'); a required input that resolves by neither an
  explicit key nor the mirror-target host is a fail-loud boot error. On success the
  generic refresh\/cache wrapper around the CodeArtifact mint leaf
  ('newCodeArtifactProvider') is built, which mints once eagerly — so a misconfigured
  identity fails here at boot. AWS credentials are the ambient container\/task role
  (the standard chain), never an Écluse key. A mint that throws at boot (a transient
  AWS error, or a permanent one like a bad domain\/region or missing permission) is
  caught and rendered as a 'CodeArtifactMintFailed' boot error rather than escaping as
  a raw exception, so it joins the aggregated failure block.
* @gcp-artifact-registry@ — recognised but not built in this binary, so selecting
  it is a fail-loud boot error rather than a silent fall-through.

The @static@ provider is also built whenever @MIRROR_TARGET_TOKEN@ is present,
independent of the selector, so a static token never goes unused.
-}
initCredentialProviders :: EnvConfig -> IO (Either [BootError] CredentialProviders)
initCredentialProviders env = case planMirrorCredential env of
    Left errs -> pure (Left errs)
    Right Nothing -> pure (Right (providersFrom Nothing))
    Right (Just caConfig) ->
        -- The CodeArtifact leaf mints once eagerly at construction, so an unreachable
        -- or unauthorised identity fails the boot here rather than on the first write.
        -- Catch that mint so its failure renders through the aggregated boot block
        -- instead of escaping as a raw amazonka exception.
        tryAny (newCodeArtifactProvider caConfig) <&> \case
            Left err -> Left [CodeArtifactMintFailed (toText (displayException err))]
            Right provider -> Right (providersFrom (Just provider))
  where
    -- Assemble the provider map from the static leaf (when a token is present) plus
    -- the optionally-built CodeArtifact provider.
    providersFrom :: Maybe CredentialProvider -> CredentialProviders
    providersFrom mCodeArtifact =
        CredentialProviders
            (Map.fromList (catMaybes [staticEntry] <> maybeToList ((CodeArtifactCredential,) <$> mCodeArtifact)))

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

-- ── mirror-target credential provider selection ───────────────────────────────

{- | Decide what mirror-target write provider the environment layer selects, as the
pure half of 'initCredentialProviders': 'Nothing' when the @static@ provider is
selected (its leaf is the @MIRROR_TARGET_TOKEN@ already handled there), 'Just' a
resolved 'CodeArtifactConfig' when @codeartifact@ is selected, or the aggregated
boot errors that block the selection.

The @gcp-artifact-registry@ arm is recognised but not built in this binary, so it is
a fail-loud 'MirrorCredentialProviderUnavailable' boot error — never a silent
fall-through to a different provider, mirroring how 'planMirrorQueue' treats the GCP
queue arm.
-}
planMirrorCredential :: EnvConfig -> Either [BootError] (Maybe CodeArtifactConfig)
planMirrorCredential env = case cfgMirrorTargetCredentialProvider env of
    StaticCredential -> Right Nothing
    CodeArtifactCredential -> Just <$> resolveCodeArtifactConfig env
    AdcCredential -> Left [MirrorCredentialProviderUnavailable AdcCredential]

{- | Resolve the CodeArtifact inputs for the mirror-target token, or the aggregated
boot errors naming each input that could not be resolved.

Each required input is resolved __(a) from its explicit @MIRROR_TARGET_CODEARTIFACT_*@
key, else (b) by parsing the mirror-target URL host__ of the form
@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@ (the documented host
fallback). The region resolves explicit key → host → @AWS_REGION@: the endpoint host
encodes the domain's authoritative region, so it outranks the process-wide
@AWS_REGION@ (a cross-region deploy mints against the domain's region, not the
caller's). The mirror-target URL is the resolved one — an unset @MIRROR_TARGET_URL@
has already folded onto the private upstream — so a private-upstream CodeArtifact
endpoint is parsed too. The optional token-duration carries through
('cfgMirrorCodeArtifactTokenDuration').

The @{owner}@ is a 12-digit AWS account id: a resolved owner (from either source)
that is not 12 digits is a fail-loud 'CodeArtifactConfigInvalid' error, and a host
whose tail after the last hyphen is not an account id is not a CodeArtifact endpoint
at all (so it falls through to the named-key check). If a required input resolves by
__neither__ source, that is a fail-loud 'CodeArtifactConfigMissing' boot error naming
the exact key the operator must set, aggregated so one run reports every problem.
-}
resolveCodeArtifactConfig :: EnvConfig -> Either [BootError] CodeArtifactConfig
resolveCodeArtifactConfig env =
    case partitionEithers [domainE, ownerE, regionE] of
        ([], [domain, owner, region]) ->
            Right
                CodeArtifactConfig
                    { caRegion = region
                    , caDomain = domain
                    , caDomainOwner = Just owner
                    , caDurationSeconds = cfgMirrorCodeArtifactTokenDuration env
                    }
        (errs, _) -> Left errs
  where
    -- The parsed (domain, owner, region) of the resolved mirror-target host, when it
    -- is a CodeArtifact endpoint — the (b) fallback source for each input.
    parsed :: Maybe (Text, Text, Text)
    parsed = parseCodeArtifactHost (hostAddress mirrorTargetUrl)

    mirrorTargetUrl :: Text
    mirrorTargetUrl = maybe (unUrl (cfgPrivateUpstream env)) unUrl (cfgMirrorTarget env)

    -- The first non-blank of the precedence-ordered candidates, or the named-key
    -- boot error. Each candidate is trimmed, so a blank explicit value falls through.
    resolve :: Text -> [Maybe Text] -> Either BootError Text
    resolve key candidates =
        maybe (Left (CodeArtifactConfigMissing key)) Right (asum (map (>>= nonBlank) candidates))

    domainE = resolve "MIRROR_TARGET_CODEARTIFACT_DOMAIN" [cfgMirrorCodeArtifactDomain env, fst3 <$> parsed]
    ownerE =
        resolve "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER" [cfgMirrorCodeArtifactDomainOwner env, snd3 <$> parsed]
            >>= validateAccountId "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER"
    regionE = resolve "MIRROR_TARGET_CODEARTIFACT_REGION" [cfgMirrorCodeArtifactRegion env, thd3 <$> parsed, cfgAwsRegion env]

    -- A resolved owner must be a 12-digit AWS account id (an explicit key can supply a
    -- malformed one; a host owner is already validated by 'parseCodeArtifactHost').
    validateAccountId :: Text -> Text -> Either BootError Text
    validateAccountId key owner
        | isAccountId owner = Right owner
        | otherwise = Left (CodeArtifactConfigInvalid key "expected a 12-digit AWS account id")

    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

-- Whether a value is a 12-digit AWS account id.
isAccountId :: Text -> Bool
isAccountId t = T.length t == 12 && T.all isDigit t

{- Parse a CodeArtifact npm endpoint host into its (domain, owner, region). The host
shape is @{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@; the @{owner}@ is the
12-digit account id after the __last__ hyphen of the first label, so a domain may
itself contain hyphens. 'Nothing' for any host that is not this shape — including one
whose tail after the last hyphen is not an account id, so a hyphen-bearing
non-CodeArtifact host never mis-parses into a bogus owner. -}
parseCodeArtifactHost :: Text -> Maybe (Text, Text, Text)
parseCodeArtifactHost host = do
    [domainOwner, regionTail] <- Just (T.splitOn ".d.codeartifact." host)
    region <- nonBlank =<< T.stripSuffix ".amazonaws.com" regionTail
    let (domainDash, owner) = T.breakOnEnd "-" domainOwner
    domain <- nonBlank (T.dropEnd 1 domainDash)
    guard (isAccountId owner)
    pure (domain, owner, region)

-- A 'Text' that is non-empty after trimming, or 'Nothing'.
nonBlank :: Text -> Maybe Text
nonBlank t = let trimmed = T.strip t in if T.null trimmed then Nothing else Just trimmed

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
    | {- | The configured SQS endpoint override (@AWS_ENDPOINT_URL_SQS@ \/
      @AWS_ENDPOINT_URL@) is not a parseable endpoint URL. Carries the offending value.
      -}
      QueueEndpointMalformed Text
    | {- | The selected mirror-target credential provider has no implementation
      compiled into this binary. Carries the unavailable provider. An honest refusal,
      never a silent fall-through.
      -}
      MirrorCredentialProviderUnavailable CredentialBackend
    | {- | A required CodeArtifact input for the mirror-target token could not be
      resolved from either its explicit key or the mirror-target host. Carries the
      name of the key the operator must set.
      -}
      CodeArtifactConfigMissing Text
    | {- | A CodeArtifact input resolved but is malformed (e.g. a domain owner that is
      not a 12-digit AWS account id). Carries the key and a reason.
      -}
      CodeArtifactConfigInvalid Text Text
    | {- | The eager boot-time CodeArtifact mint threw — a transient AWS error (worth a
      retry) or a permanent one (a bad domain\/region or missing permission, to be
      fixed). Carries the rendered exception so the cause is legible and aggregated.
      -}
      CodeArtifactMintFailed Text
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
    QueueEndpointMalformed url ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS / AWS_ENDPOINT_URL) is not a valid endpoint URL: " <> url
    MirrorCredentialProviderUnavailable backend ->
        "mirror-target credential provider "
            <> renderMirrorCredentialProvider backend
            <> " is not available in this build"
    CodeArtifactConfigMissing key ->
        "mirror-target credential provider codeartifact requires "
            <> key
            <> " (set it explicitly, or use a CodeArtifact MIRROR_TARGET_URL it can be parsed from)"
    CodeArtifactConfigInvalid key reason ->
        "mirror-target credential provider codeartifact: " <> key <> " is invalid (" <> reason <> ")"
    CodeArtifactMintFailed detail ->
        "mirror-target credential provider codeartifact failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry; a permanent one — bad domain/region or missing permission — must be fixed)"

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

{- | Which mirror-queue backend the composition root will build, resolved from
config: the durable AWS @sqs@ backend (with its 'SqsConfig'), or the bounded
best-effort in-memory backend (with its 'MemoryQueueConfig'). The pure decision
'planMirrorQueue' yields; the composition root pattern-matches it to make the one
constructor call, and 'mirrorQueuePlanWarning' tells it whether a boot warning is due.
-}
data MirrorQueuePlan
    = -- | The durable AWS SQS backend, built by @Ecluse.Queue.Sqs.newSqsQueue@.
      SqsBackend SqsConfig
    | {- | The bounded in-memory backend, built by
      'Ecluse.Queue.newBoundedInMemoryQueue'. Non-durable and best-effort — boot warns.
      -}
      MemoryBackend MemoryQueueConfig
    deriving stock (Eq, Show)

{- | Select the mirror-queue backend from the environment layer, yielding the
'MirrorQueuePlan' the composition root builds the queue from, or the aggregated boot
errors that block it.

This is the pure half of the queue's backend choice — the single place that knows
which backends this binary can build. The AWS @sqs@ backend resolves to a
'SqsBackend' carrying its 'SqsConfig' (the queue URL and region, with the provider
knobs at their defaults); the composition root passes that to
@Ecluse.Queue.Sqs.newSqsQueue@. The @memory@ backend resolves to a 'MemoryBackend'
carrying its depth cap, built in-process with no cloud queue (@MIRROR_QUEUE_URL@ and
@AWS_REGION@ are not consulted for it) — an explicit operator choice for a simple,
single-node, or air-gapped deployment, never an automatic fallback (which would
soften the fail-loud-on-misconfig posture); the composition root emits the
'memoryQueueBootWarning' on selection. The GCP @pubsub@ arm is recognised but not
built, so it is a fail-loud 'QueueProviderUnavailable' boot error rather than a
silent fall-through; a missing @AWS_REGION@ under @sqs@ is a 'QueueRegionMissing'
boot error. Errors are returned as a list so they aggregate with the rest of the
boot-time validation.

When an endpoint override is configured (@AWS_ENDPOINT_URL_SQS@, else
@AWS_ENDPOINT_URL@ — the AWS-SDK-standard variables), it is parsed into the
backend's 'SqsEndpoint' so the released image can target a local emulator
(@ministack@) or a VPC endpoint without a test-only code path; a malformed override URL is
a fail-loud 'QueueEndpointMalformed' boot error. With no override, the SQS backend
uses AWS's default endpoint and credential resolution.
-}
planMirrorQueue :: EnvConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue env = case cfgQueueBackend env of
    PubSubQueue -> Left [QueueProviderUnavailable PubSubQueue]
    MemoryQueue -> Right (MemoryBackend (defaultMemoryQueueConfig (cfgQueueMemoryMaxDepth env)))
    SqsQueue -> case T.strip <$> cfgAwsRegion env of
        Just region | not (T.null region) -> do
            endpoint <- resolveSqsEndpoint env
            Right (SqsBackend (defaultSqsConfig (unUrl (cfgQueueUrl env)) region){sqsEndpoint = endpoint})
        _ -> Left [QueueRegionMissing]

{- | The loud boot warning a 'MirrorQueuePlan' warrants before its queue is built, or
'Nothing' for a durable backend that needs none. The composition root logs the
'Just' at @WarningS@ on selection, so an operator who chose the in-memory backend is
told plainly that the mirror is non-durable — never a silent surprise.
-}
mirrorQueuePlanWarning :: MirrorQueuePlan -> Maybe Text
mirrorQueuePlanWarning = \case
    SqsBackend _ -> Nothing
    MemoryBackend _ -> Just memoryQueueBootWarning

{- | The boot warning emitted when the in-memory mirror-queue backend is selected: it
states plainly that the mirror is in-memory, non-durable, and best-effort, and that a
lost job is re-mirrored on the next demand (so there is no data loss, only deferred
mirroring), so the choice is never mistaken for a durable cloud backend.
-}
memoryQueueBootWarning :: Text
memoryQueueBootWarning =
    "mirror queue provider 'memory' selected: the mirror queue is IN-MEMORY, NON-DURABLE, and BEST-EFFORT. "
        <> "Jobs are dropped on cap overflow and lost on restart or redeploy; each is re-mirrored on the next "
        <> "demand (no data loss, only deferred mirroring). Use a durable backend ('sqs') for a production mirror "
        <> "that must not shed under load."

{- | The cap-overflow drop warning for the in-memory backend, carrying the running
total of dropped jobs (this report is rate-limited at the queue, so it does not fire
per dropped job). A note on a one-line follow-up: a drop __metric__
(@ecluse.mirror.*@, S26 PR2) hooks in alongside this log once that catalogue lands.
-}
memoryQueueDropWarning :: Int -> Text
memoryQueueDropWarning dropped =
    "mirror queue at capacity: dropped a mirror job (drop-newest); "
        <> show dropped
        <> " job(s) dropped so far. Each is re-mirrored on the next demand; raise "
        <> "MIRROR_QUEUE_MEMORY_MAX_DEPTH to shed fewer under load."

{- Resolve the optional SQS endpoint override into an 'SqsEndpoint', or 'Nothing' for
AWS's default resolution. The AWS-SDK-standard @AWS_ENDPOINT_URL_SQS@ takes precedence
over the generic @AWS_ENDPOINT_URL@; the override URL is parsed into its TLS flag,
host, and port, and the request signing keys are taken from the standard
@AWS_ACCESS_KEY_ID@\/@AWS_SECRET_ACCESS_KEY@ (an emulator is off the ambient chain).
A malformed override URL is a fail-loud boot error. -}
resolveSqsEndpoint :: EnvConfig -> Either [BootError] (Maybe SqsEndpoint)
resolveSqsEndpoint env =
    case nonBlank =<< (cfgAwsEndpointUrlSqs env <|> cfgAwsEndpointUrl env) of
        Nothing -> Right Nothing
        Just url -> case parseEndpointUrl url of
            Just (secure, host, port) ->
                Right
                    ( Just
                        SqsEndpoint
                            { endpointSecure = secure
                            , endpointHost = host
                            , endpointPort = port
                            , endpointAccessKey = fromMaybe "" (cfgAwsAccessKeyId env)
                            , -- Carried as a redacted 'Secret' end to end (never unwrapped here).
                              endpointSecretKey = fromMaybe (mkSecret "") (cfgAwsSecretAccessKey env)
                            }
                    )
            Nothing -> Left [QueueEndpointMalformed url]

{- Parse an endpoint URL into its (TLS flag, host, port). The scheme picks the TLS
flag and the default port (443\/80) when none is given; an absent scheme or a
non-numeric port yields 'Nothing'. A bracketed IPv6 literal authority
(@[::1]:4566@) is split on the closing bracket, not on an inner colon, and the host
is returned without brackets. -}
parseEndpointUrl :: Text -> Maybe (Bool, Text, Int)
parseEndpointUrl raw = do
    (secure, afterScheme) <-
        ((True,) <$> T.stripPrefix "https://" raw) <|> ((False,) <$> T.stripPrefix "http://" raw)
    let authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
    (hostText, portText) <- splitAuthority authority
    host <- nonBlank hostText
    port <- case T.stripPrefix ":" portText of
        Nothing -> Just (if secure then 443 else 80)
        Just digits -> readMaybe (toString digits)
    pure (secure, host, port)
  where
    -- Split an authority into (host, "":port"|""). A @[…]@ IPv6 literal splits on the
    -- closing bracket so an inner colon is never mistaken for the port separator.
    splitAuthority :: Text -> Maybe (Text, Text)
    splitAuthority authority = case T.stripPrefix "[" authority of
        Just rest -> case T.breakOn "]" rest of
            (_, "") -> Nothing -- an opening bracket with no close: malformed
            (inner, afterBracket) -> Just (inner, T.drop 1 afterBracket)
        Nothing -> Just (T.breakOn ":" authority)

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
