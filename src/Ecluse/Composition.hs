{- HLINT ignore "Avoid restricted function" -}

{- | The composition-root wiring: turn a validated 'Config' and the
process-global credential providers into the served 'MountBinding's, failing fast
and __aggregated__ on any boot problem.

This is the __listener-free__ heart of the composition root ("Ecluse" calls it): it
holds no sockets, no network, and no real clock of its own -- the clock and the
ecosystem-to-adapter resolver are injected -- so the boot-time validation is
unit-tested without opening a listener. Its one effect is preparing each mount's rule
set ('Ecluse.Core.Rules.prepare'), which allocates per-rule engine state once at boot
(a breaker for a resilient rule; the built-in rules need none today), so binding
assembly is 'IO'; everything else stays a pure function of the validated config.

== Global providers, per-mount reference

A 'Ecluse.Core.Credential.CredentialProvider' is the service's own cloud identity,
built __once__ from the environment layer ('initCredentialProviders') and held
process-global; a mount does not carry a provider, it only __names__ which backend
it draws on (its @mtCredential@). The boot-time check is the resolution of that
reference: every distinct credential backend named across all mounts must resolve
to an __initialized__ provider, or the app halts at boot (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider"). Only the
@static@ backend has a leaf (from @ECLUSE_MIRROR_TARGET_TOKEN@); a mount naming
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
    initializedEcosystems,
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
    connectionPoolSettings,
) where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Network.HTTP.Client (ManagerSettings (managerConnCount))
import UnliftIO (tryAny)

import Ecluse.Config (
    AppConfig (..),
    Config (..),
    CredentialBackend (..),
    MirrorTarget (mtCredential, mtUrl),
    Mount (..),
    MountConfig (..),
    MountRegistries (..),
    PolicyError,
    QueueBackend (..),
    Url,
    renderPolicyError,
    unUrl,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, Secret, staticProvider)
import Ecluse.Core.Credential.CodeArtifact (CodeArtifactConfig (..), newCodeArtifactProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, prefixFor)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Queue (MemoryQueueConfig, defaultMemoryQueueConfig)
import Ecluse.Core.Queue.Sqs (SqsConfig (sqsEndpoint), SqsEndpoint (..), defaultSqsConfig)
import Ecluse.Core.Registry.Npm qualified as Npm
import Ecluse.Core.Registry.Npm.Filter qualified as NpmFilter
import Ecluse.Core.Registry.Npm.Project qualified as NpmProject
import Ecluse.Core.Registry.Npm.Request qualified as NpmRequest
import Ecluse.Core.Wire (renderWire)

import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), hostAddress, lowerCaseHosts, splitHostPort)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Cache (CacheConfig (..))
import Ecluse.Core.Server.Context (MountBinding, PackumentDeps (..), PublishDeps (..))
import Ecluse.Core.Server.Metadata qualified as Metadata
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)
import Ecluse.Core.Text (nonBlank)

{- | Apply an explicit per-host connection bound to an HTTP manager's settings.

The public and private managers call this independently after telemetry
instrumentation, so changing the pool size cannot discard the instrumented request and
response hooks.
-}
connectionPoolSettings :: Int -> ManagerSettings -> ManagerSettings
connectionPoolSettings connections settings = settings{managerConnCount = connections}

{- | The process-global credential providers, keyed by the backend they
implement. Built __once__ at the composition root from the environment layer; a
mount references one by name and never holds its own.

The keyset (see 'initializedBackends') is the boot-check's pure surface -- a mount
that names a backend absent from it has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | Build the global credential providers from the environment layer, or the
aggregated boot errors that block them. The mirror-target write provider is
selected by 'cfgCredentialProvider' (see 'planMirrorCredential'):

* @static@ -- built from @ECLUSE_MIRROR_TARGET_TOKEN@ ('cfgMirrorTargetToken') when set;
  absent, no static provider is initialized, so a mount naming @static@ fails the
  boot-time credential-reference check.
* @codeartifact@ -- the CodeArtifact inputs are resolved
  ('resolveCodeArtifactConfig'); a required input that resolves by neither an
  explicit key nor the mirror-target host is a fail-loud boot error. On success the
  generic refresh\/cache wrapper around the CodeArtifact mint leaf
  ('newCodeArtifactProvider') is built, which mints once eagerly -- so a misconfigured
  identity fails here at boot. AWS credentials are the ambient container\/task role
  (the standard chain), never an Écluse key. A mint that throws at boot (a transient
  AWS error, or a permanent one like a bad domain\/region or missing permission) is
  caught and rendered as a 'CodeArtifactMintFailed' boot error rather than escaping as
  a raw exception, so it joins the aggregated failure block.
* @gcp-artifact-registry@ -- recognised but not built in this binary, so selecting
  it is a fail-loud boot error rather than a silent fall-through.

The @static@ provider is also built whenever @ECLUSE_MIRROR_TARGET_TOKEN@ is present,
independent of the selector, so a static token never goes unused.

The 'CredentialReporters' are handed to the refreshing CodeArtifact provider so its
mint breaker and refresh outcomes record to telemetry; the static provider never
refreshes, so they do not concern it. The composition root supplies the deferred
reporters that go live once the telemetry substrate exists.
-}
initCredentialProviders :: CredentialReporters -> AppConfig -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reporters app = do
    let plans = map (\(eco, mcfg) -> (eco, mcfg, planMirrorCredential eco app mcfg)) (Map.toList (cfgMounts app))
    let errs = concat [e | (_, _, Left e) <- plans]
    if not (null errs)
        then pure (Left errs)
        else do
            let validPlans = [(eco, mcfg, mca) | (eco, mcfg, Right mca) <- plans]
            results <-
                traverse
                    ( \(eco, mcfg, mca) -> do
                        case mca of
                            Nothing ->
                                case mntMirrorTargetToken mcfg of
                                    Just token -> pure (Right (eco, Just (staticProvider AuthToken{authSecret = token, authExpiresAt = Nothing})))
                                    Nothing -> pure (Right (eco, Nothing))
                            Just caConfig -> do
                                tryAny (newCodeArtifactProvider reporters caConfig) <&> \case
                                    Left err -> Left [CodeArtifactMintFailed (toText (displayException err))]
                                    Right provider -> Right (eco, Just provider)
                    )
                    validPlans
            let (initErrs, valid) = partitionEithers results
            if not (null initErrs)
                then pure (Left (concat initErrs))
                else pure (Right (CredentialProviders (Map.fromList [(eco, p) | (eco, Just p) <- valid])))

{- | The set of ecosystems that resolved to an initialized provider -- the
pure surface the boot-time credential-reference check reasons over.
-}
initializedEcosystems :: CredentialProviders -> Set Ecosystem
initializedEcosystems (CredentialProviders ps) = Map.keysSet ps

{- | Look up the initialized provider for an ecosystem, 'Nothing' when none is
initialized (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupProvider eco (CredentialProviders ps) = Map.lookup eco ps

{- | Decide what mirror-target write provider the environment layer selects, as the
pure half of 'initCredentialProviders': 'Nothing' when the @static@ provider is
selected (its leaf is the @ECLUSE_MIRROR_TARGET_TOKEN@ already handled there), 'Just' a
resolved 'CodeArtifactConfig' when @codeartifact@ is selected, or the aggregated
boot errors that block the selection.

The @gcp-artifact-registry@ arm is recognised but not built in this binary, so it is
a fail-loud 'MirrorCredentialProviderUnavailable' boot error -- never a silent
fall-through to a different provider, mirroring how 'planMirrorQueue' treats the GCP
queue arm.
-}
planMirrorCredential :: Ecosystem -> AppConfig -> MountConfig -> Either [BootError] (Maybe CodeArtifactConfig)
planMirrorCredential eco app mcfg = case mntCredentialProvider mcfg of
    StaticCredential -> Right Nothing
    CodeArtifactCredential -> Just <$> resolveCodeArtifactConfig eco app mcfg
    AdcCredential -> Left [MirrorCredentialProviderUnavailable AdcCredential]

{- | Resolve the CodeArtifact inputs for the mirror-target token, or the aggregated
boot errors naming each input that could not be resolved.

Each required input is resolved __(a) from its explicit @MIRROR_TARGET_CODEARTIFACT_*@
key, else (b) by parsing the mirror-target URL host__ of the form
@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@ (the documented host
fallback). The region resolves explicit key → host → @AWS_REGION@: the endpoint host
encodes the domain's authoritative region, so it outranks the process-wide
@AWS_REGION@ (a cross-region deploy mints against the domain's region, not the
caller's). The mirror-target URL is the resolved one -- an unset @ECLUSE_MIRROR_TARGET@
has already folded onto the private upstream -- so a private-upstream CodeArtifact
endpoint is parsed too. The optional token-duration carries through
('cfgMirrorCodeArtifactTokenDuration').

The @{owner}@ is a 12-digit AWS account id: a resolved owner (from either source)
that is not 12 digits is a fail-loud 'CodeArtifactConfigInvalid' error, and a host
whose tail after the last hyphen is not an account id is not a CodeArtifact endpoint
at all (so it falls through to the named-key check). If a required input resolves by
__neither__ source, that is a fail-loud 'CodeArtifactConfigMissing' boot error naming
the exact key the operator must set, aggregated so one run reports every problem.
-}
resolveCodeArtifactConfig :: Ecosystem -> AppConfig -> MountConfig -> Either [BootError] CodeArtifactConfig
resolveCodeArtifactConfig eco app mcfg =
    case partitionEithers [domainE, ownerE, regionE] of
        ([], [domain, owner, region]) ->
            Right
                CodeArtifactConfig
                    { caRegion = region
                    , caDomain = domain
                    , caDomainOwner = Just owner
                    , caDurationSeconds = mntMirrorCodeArtifactTokenDuration mcfg
                    }
        (errs, _) -> Left errs
  where
    parsed :: Maybe (Text, Text, Text)
    parsed = parseCodeArtifactHost (hostAddress mirrorTargetUrl)

    mirrorTargetUrl :: Text
    mirrorTargetUrl = maybe (registryUrlText (fromMaybe (error "no pUpstream") (mntPrivateUpstream mcfg))) registryUrlText (mntMirrorTarget mcfg)

    resolve :: Text -> [Maybe Text] -> Either BootError Text
    resolve key candidates =
        let fullKey = "ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName eco) <> "__" <> key
         in maybe (Left (CodeArtifactConfigMissing fullKey)) Right (asum (map (>>= nonBlank) candidates))

    domainE = resolve "MIRROR_CODE_ARTIFACT_DOMAIN" [mntMirrorCodeArtifactDomain mcfg, fst3 <$> parsed]
    ownerE =
        resolve "MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" [mntMirrorCodeArtifactDomainOwner mcfg, snd3 <$> parsed]
            >>= validateAccountId ("ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName eco) <> "__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER")
    regionE = resolve "MIRROR_CODE_ARTIFACT_REGION" [mntMirrorCodeArtifactRegion mcfg, thd3 <$> parsed, cfgAwsRegion app]

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
isAccountId t = T.compareLength t 12 == EQ && T.all isDigit t

{- Parse a CodeArtifact npm endpoint host into its (domain, owner, region). The host
shape is @{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@; the @{owner}@ is the
12-digit account id after the __last__ hyphen of the first label, so a domain may
itself contain hyphens. 'Nothing' for any host that is not this shape -- including one
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
      An honest refusal -- never a silent fall-through to a different backend.
      -}
      QueueProviderUnavailable QueueBackend
    | {- | The SQS mirror-queue backend was selected but no AWS region was supplied
      (@AWS_REGION@), so the queue cannot be scoped to a region.
      -}
      QueueRegionMissing
    | {- | A cloud mirror-queue backend (e.g. @sqs@) was selected but no
      @ECLUSE_QUEUE_URL@ was supplied, so there is no queue to send jobs to. The
      in-memory backend does not raise this -- it has no external queue.
      -}
      QueueUrlMissing QueueBackend
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
    | {- | The eager boot-time CodeArtifact mint threw -- a transient AWS error (worth a
      retry) or a permanent one (a bad domain\/region or missing permission, to be
      fixed). Carries the rendered exception so the cause is legible and aggregated.
      -}
      CodeArtifactMintFailed Text
    | {- | A publication target was configured (@ECLUSE_PUBLICATION_TARGET@) but no
      publish-scope allow-list (@ECLUSE_PUBLISH_SCOPES@) was supplied, so the anti-shadowing
      guard would have nothing to enforce. Refused at boot rather than defaulting to an
      empty allow-list (which would deny every publish) or an open one (which would let
      a client shadow any public name).
      -}
      PublishScopesMissing Ecosystem
    | {- | A static publish credential (@ECLUSE_PUBLICATION_TARGET_TOKEN@) was configured
      without a verifiable inbound edge (@ECLUSE_AUTH_TOKEN@). Écluse would otherwise
      substitute its own standing write credential for a publishing caller who forwards
      none, so an unauthenticated request could publish within the configured scopes
      under Écluse's own identity. Refused at boot so an internal publish credential
      paired with an open edge is unrepresentable -- the write-side counterpart of the
      fail-closed read identity.
      -}
      PublishStaticCredentialNeedsEdge Ecosystem
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
            <> renderWire backend
            <> ", not initialized in this build"
    QueueProviderUnavailable backend ->
        "mirror queue provider "
            <> renderWire backend
            <> " is not available in this build"
    QueueRegionMissing ->
        "mirror queue provider "
            <> renderWire SqsQueue
            <> " requires AWS_REGION to be set"
    QueueUrlMissing backend ->
        "mirror queue provider "
            <> renderWire backend
            <> " requires ECLUSE_QUEUE_URL to be set"
    QueueEndpointMalformed url ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS / AWS_ENDPOINT_URL) is not a valid endpoint URL: " <> url
    MirrorCredentialProviderUnavailable backend ->
        "mirror-target credential provider "
            <> renderWire backend
            <> " is not available in this build"
    CodeArtifactConfigMissing key ->
        "mirror-target credential provider codeartifact requires "
            <> key
            <> " (set it explicitly, or use a CodeArtifact ECLUSE_MIRROR_TARGET it can be parsed from)"
    CodeArtifactConfigInvalid key reason ->
        "mirror-target credential provider codeartifact: " <> key <> " is invalid (" <> reason <> ")"
    CodeArtifactMintFailed detail ->
        "mirror-target credential provider codeartifact failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry; a permanent one -- bad domain/region or missing permission -- must be fixed)"
    PublishScopesMissing eco ->
        "ECLUSE_MOUNTS__" <> T.toUpper (T.pack (show eco)) <> "__PUBLICATION_TARGET is set but ECLUSE_MOUNTS__" <> T.toUpper (T.pack (show eco)) <> "__PUBLISH_SCOPES is empty: a publication target needs a publish-scope allow-list (e.g. @acme) for the anti-shadowing guard."
    PublishStaticCredentialNeedsEdge eco ->
        "ECLUSE_MOUNTS__" <> T.toUpper (T.pack (show eco)) <> "__PUBLICATION_TARGET_TOKEN is set but ECLUSE_AUTH_TOKEN is not: a static publish credential needs a verifiable inbound edge."

{- | Validate the environment layer and optional document into the served mount
bindings, or the aggregated boot errors. The composition root's single entry: it
runs 'loadConfig' (whose policy errors become 'PolicyBootError's) and then
'composeBindings', so policy, missing-adapter, and unresolved-credential failures
all surface from one call.

The ecosystem-to-adapter resolver and the wall-clock source are injected (the
composition root supplies @mountBindingFor@ and 'Data.Time.getCurrentTime'), so
this validation opens no socket. It is 'IO' only because 'composeBindings' 'prepare's
each mount's rules (allocating per-rule engine state once at boot).
-}
planMounts ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    CredentialProviders ->
    Config ->
    IO (Either [BootError] [MountBinding])
planMounts = composeBindings

{- | Turn a validated 'Config' into the served 'MountBinding's, or the aggregated
boot errors. For each mount, in ecosystem order: its credential reference must
resolve to an initialized provider, and its ecosystem must resolve to an adapter
(through the injected resolver, fed real 'PackumentDeps' so the packument route is
served rather than the @501@ stub). Errors aggregate across every mount.
-}
composeBindings ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    CredentialProviders ->
    Config ->
    IO (Either [BootError] [MountBinding])
composeBindings resolveAdapter clock providers config = do
    let pubDepsMapE = sequence $ Map.mapWithKey (\eco mcfg -> publishDepsFor eco (configApp config) mcfg limits helpMessage) (cfgMounts (configApp config))
    let (pubErrs, pubDepsMap) = case pubDepsMapE of
            Left errs -> (errs, Map.empty)
            Right m -> ([], m)

    bindingResults <- traverse (\mount -> bindingFor (join (Map.lookup (mountEcosystem mount) pubDepsMap)) mount) (Map.elems (configMounts config))
    pure $ case (pubErrs, partitionEithers bindingResults) of
        ([], ([], bindings)) -> Right bindings
        (_, (errs, _)) -> Left (pubErrs <> concat errs)
  where
    inboundToken :: Maybe Secret
    inboundToken = cfgAuthToken (configApp config)

    -- The resolved tarball-host policy for every mount, from the secure-default
    -- environment toggle: honour a cross-host dist.tarball only when explicitly
    -- opted in (and even then, never past the allowlist or the internal block).
    -- The resolved tarball-host policy for every mount, from the secure-default
    tarballHostPolicy :: Mount -> TarballHostPolicy
    tarballHostPolicy mount =
        if mntRespectUpstreamTarballHost (fromMaybe (error "no mount") (Map.lookup (mountEcosystem mount) (cfgMounts (configApp config))))
            then AnyAllowlistedHost
            else SameHostAsPackument

    -- The response-bound budget every mount enforces on its upstream fetches and
    -- decodes (security.md invariant 4), assembled from the validated environment
    -- ceilings. Carried onto each mount's deps so the data plane reads the metadata
    -- body bounded, and refuses an over-deep or version-flooded document fail-closed.
    limits :: Limits
    limits =
        Limits
            { maxBodyBytes = cfgMaxResponseBytes (configApp config)
            , maxVersionCount = cfgMaxVersionCount (configApp config)
            , maxNestingDepth = cfgMaxNestingDepth (configApp config)
            }

    -- The operator help message, derived from the environment layer like the
    -- inbound token, so every mount's denials carry it.
    helpMessage :: Maybe HelpMessage
    helpMessage = mkHelpMessage <$> cfgHelpMessage (configApp config)

    -- The global public-integrity admission floor, validated at config load, carried
    -- onto every mount's deps so the public gate refuses a below-floor version.
    minIntegrity :: MinIntegrity
    minIntegrity = cfgMinPublicIntegrity (configApp config)

    -- The global trusted-integrity admission floor (default SHA-256, loosenable below it),
    -- carried onto every mount's deps so the trusted gate drops a below-floor private
    -- version from the listing and gates the private artifact serve.
    minTrustedIntegrity :: MinTrustedIntegrity
    minTrustedIntegrity = cfgMinTrustedIntegrity (configApp config)

    -- A mount's externally-visible base URL for the dist.tarball rewrite. Absolute
    -- under ECLUSE_PUBLIC_URL when set (so a served tarball is a full URL an npm
    -- client can fetch); otherwise the relative prefix path, retained for
    -- compatibility. A trailing slash on the configured URL is dropped so the join
    -- with the leading-slash mount path yields exactly one separator.
    mountBaseUrl :: Ecosystem -> Text
    mountBaseUrl eco =
        case cfgPublicUrl (configApp config) of
            Nothing -> mountBasePath eco
            Just public -> T.dropWhileEnd (== '/') (unUrl public) <> mountBasePath eco

    {- Resolve one mount to its binding, or the boot errors that block it. Both the
    credential reference and the adapter are checked even when one already failed,
    so a mount missing both reports both in one run rather than one at a time. The
    resolved publish dependencies (shared across mounts) are passed to the adapter so
    the binding carries the first-party publish wiring. -}
    bindingFor :: Maybe PublishDeps -> Mount -> IO (Either [BootError] MountBinding)
    bindingFor pubDeps mount = do
        deps <- packumentDepsFor mount
        pure $ case (credentialError mount, resolveAdapter (mountEcosystem mount) (Just deps) pubDeps) of
            (Nothing, Just binding) -> Right binding
            (mCredErr, mBinding) ->
                Left (maybeToList mCredErr <> [MissingAdapter (mountEcosystem mount) | isNothing mBinding])

    -- The credential reference of a mount: an error when the named backend is not
    -- initialized, nothing when it resolves.
    credentialError :: Mount -> Maybe BootError
    credentialError mount =
        let backend = mtCredential (regMirrorTarget (mountRegistries mount))
         in if mountEcosystem mount `Set.member` initializedEcosystems providers
                then Nothing
                else Just (UnresolvedCredential (mountEcosystem mount) backend)

    {- Build a mount's 'PackumentDeps' from its registries, resolved rules, the
    inbound edge token, the injected clock, and the operator help message. The
    mount's externally-visible base URL drives the @dist.tarball@ rewrite: an
    __absolute__ URL under @ECLUSE_PUBLIC_URL@ (@{public}\/npm\/{pkg}\/-\/{file}@)
    when one is configured, so an @npm@ client fetches the artifact back through the
    proxy on the gated path; otherwise the relative prefix path (@\/npm@), retained
    for compatibility -- but note @npm@ cannot consume a relative @dist.tarball@ (it
    reads a leading slash as a @file:@ path), so a real install path must set
    @ECLUSE_PUBLIC_URL@ (see @mountBaseUrl@ and
    @docs\/architecture\/hosting.md@ → "URL rewriting"). -}
    packumentDepsFor :: Mount -> IO PackumentDeps
    packumentDepsFor mount = do
        -- Prepare the resolved policy into the engine's runtime rules. No effectful rule
        -- type is wired into the policy model yet, so every rule here is built-in and
        -- 'prepare' allocates no breaker; an effectful rule would carry a resilience
        -- policy (and its breaker, allocated here once).
        prepared <- prepare (mountPolicy mount)
        let regs = mountRegistries mount
        pure
            PackumentDeps
                { pdPrivateBaseUrl = registryUrlText (regPrivateUpstream regs)
                , pdPublicBaseUrl = registryUrlText (regPublicUpstream regs)
                , pdMountBaseUrl = mountBaseUrl (mountEcosystem mount)
                , pdMirrorTarget = registryUrlText (mtUrl (regMirrorTarget regs))
                , pdRules = prepared
                , pdTarballHostPolicy = tarballHostPolicy mount
                , -- The internal-range opt-in for an honoured tarball host is empty: the
                  -- composition root's secure default for the pure literal-block
                  -- defence-in-depth on the dist.tarball host gate.
                  pdAllowedInternalHosts = lowerCaseHosts mempty
                , pdLimits = limits
                , pdInboundToken = inboundToken
                , pdNow = clock
                , pdHelp = helpMessage
                , pdMinIntegrity = minIntegrity
                , pdMinTrustedIntegrity = minTrustedIntegrity
                , pdNewMetadataClient = \t p u c f1 f2 f3 l m b s -> Metadata.newNpmMetadataClient t p u c f1 f2 f3 (Npm.NpmClientConfig b m s l)
                , pdBuildArtifactRequestByFile = \_ _ t s -> NpmRequest.artifactRequestByFile t s
                , pdBuildArtifactRequestByUrl = \_ _ t s -> NpmRequest.artifactRequestByUrl t s
                , pdApplyFilter = NpmFilter.applyFilterPlan
                , pdRewriteUrls = NpmFilter.rewriteTarballUrls
                }

-- The mount's externally-visible base path, derived from its ecosystem prefix
-- (@npm@ → @\/npm@): a leading slash and the prefix segments joined, so it is the
-- relative path a client's registry endpoint maps onto.
mountBasePath :: Ecosystem -> Text
mountBasePath eco = "/" <> T.intercalate "/" (toList (prefixFor eco))

{- | Validate the first-party publish dependencies from the environment layer, shared
across the (single-ecosystem) mounts: 'Nothing' when no publication target is configured
(the publish path is off -- a @PUT \/{pkg}@ is then @405@), 'Just' when one is set and
valid, or the accumulated fail-loud publish boot errors when not -- 'PublishScopesMissing'
when a target is set without a publish-scope allow-list, and\/or
'PublishStaticCredentialNeedsEdge' when a static publish credential is set without a
verifiable inbound edge -- reported together rather than one reboot at a time. The target's
URL, the scopes, and the static fallback credential are the publish env layer; the
response bounds ('Limits') and help message are shared with the read paths and passed in.
-}
publishDepsFor :: Ecosystem -> AppConfig -> MountConfig -> Limits -> Maybe HelpMessage -> Either [BootError] (Maybe PublishDeps)
publishDepsFor eco app mcfg limits helpMessage = case mntPublicationTarget mcfg of
    Nothing -> Right Nothing
    Just url -> case bootErrors of
        [] ->
            Right
                ( Just
                    PublishDeps
                        { pubTargetUrl = registryUrlText url
                        , pubScopes = mntPublishScopes mcfg
                        , pubStaticToken = staticToken
                        , pubInboundToken = inboundToken
                        , pubLimits = limits
                        , pubHelp = helpMessage
                        , pubRelayPublish = \l m t s -> Npm.relayPublishDocument (Npm.NpmClientConfig t m s l)
                        , pubCanonicaliseName = rightToMaybe . NpmProject.projectName
                        }
                )
        errs -> Left errs
  where
    inboundToken, staticToken :: Maybe Secret
    inboundToken = cfgAuthToken app
    staticToken = mntPublicationTargetToken mcfg

    bootErrors :: [BootError]
    bootErrors = catMaybes [scopesError, edgeError]

    scopesError, edgeError :: Maybe BootError
    scopesError
        | null (mntPublishScopes mcfg) = Just (PublishScopesMissing eco)
        | otherwise = Nothing
    edgeError
        | isJust staticToken && isNothing inboundToken = Just (PublishStaticCredentialNeedsEdge eco)
        | otherwise = Nothing

{- | One ecosystem's resolved __publish__ target: the mirror-target endpoint the
mirror worker writes approved artifacts to, paired with the credential provider
that mints its bearer token.

This is the publish side of the per-ecosystem composition (the serve side is the
mount's 'PackumentDeps'). The worker's single consumer builds a registry-protocol
client from these -- the endpoint as its base URL, the provider's token as its
bearer -- so the publish client is resolved here at the composition root rather than
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
    Config ->
    Either [BootError] [PublishTarget]
planPublishTargets = composePublishTargets

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
         in case lookupProvider (mountEcosystem mount) providers of
                Just provider ->
                    Right
                        PublishTarget
                            { ptEcosystem = mountEcosystem mount
                            , ptMirrorUrl = registryUrlText (mtUrl target)
                            , ptCredentials = provider
                            }
                Nothing ->
                    Left [UnresolvedCredential (mountEcosystem mount) backend]

{- | Which mirror-queue backend the composition root will build, resolved from
config: the durable AWS @sqs@ backend (with its 'SqsConfig'), or the bounded
best-effort in-memory backend (with its 'MemoryQueueConfig'). The pure decision
'planMirrorQueue' yields; the composition root pattern-matches it to make the one
constructor call, and 'mirrorQueuePlanWarning' tells it whether a boot warning is due.
-}
data MirrorQueuePlan
    = -- | The durable AWS SQS backend, built by @Ecluse.Core.Queue.Sqs.newSqsQueue@.
      SqsBackend SqsConfig
    | {- | The bounded in-memory backend, built by
      'Ecluse.Core.Queue.newBoundedInMemoryQueue'. Non-durable and best-effort -- boot warns.
      -}
      MemoryBackend MemoryQueueConfig
    deriving stock (Eq, Show)

{- | Select the mirror-queue backend from the environment layer, yielding the
'MirrorQueuePlan' the composition root builds the queue from, or the aggregated boot
errors that block it.

This is the pure half of the queue's backend choice -- the single place that knows
which backends this binary can build. The AWS @sqs@ backend resolves to a
'SqsBackend' carrying its 'SqsConfig' (the queue URL and region, with the provider
knobs at their defaults); the composition root passes that to
@Ecluse.Core.Queue.Sqs.newSqsQueue@. The @memory@ backend resolves to a 'MemoryBackend'
carrying its depth cap, built in-process with no cloud queue (@ECLUSE_QUEUE_URL@ and
@AWS_REGION@ are not consulted for it) -- an explicit operator choice for a simple,
single-node, or air-gapped deployment, never an automatic fallback (which would
soften the fail-loud-on-misconfig posture); the composition root emits the
'memoryQueueBootWarning' on selection. The GCP @pubsub@ arm is recognised but not
built, so it is a fail-loud 'QueueProviderUnavailable' boot error rather than a
silent fall-through. @ECLUSE_QUEUE_URL@ is optional at the env layer; it is required
__here__ for @sqs@ (the jobs need a queue), so a missing one is a fail-loud
'QueueUrlMissing' boot error, and a missing @AWS_REGION@ under @sqs@ is a
'QueueRegionMissing' boot error -- the @sqs@ arm aggregates the region, queue-URL, and
endpoint failures, and the whole result is a list so it aggregates with the rest of
the boot-time validation.

When an endpoint override is configured (@AWS_ENDPOINT_URL_SQS@, else
@AWS_ENDPOINT_URL@ -- the AWS-SDK-standard variables), it is parsed into the
backend's 'SqsEndpoint' so the released image can target a local emulator
(@ministack@) or a VPC endpoint without a test-only code path; a malformed override URL is
a fail-loud 'QueueEndpointMalformed' boot error. With no override, the SQS backend
uses AWS's default endpoint and credential resolution.
-}
planMirrorQueue :: AppConfig -> Either [BootError] MirrorQueuePlan
planMirrorQueue env = case cfgQueueBackend env of
    PubSubQueue -> Left [QueueProviderUnavailable PubSubQueue]
    -- The in-memory backend needs no cloud queue: ECLUSE_QUEUE_URL and AWS_REGION are
    -- not consulted, so it can never fail on a missing one.
    MemoryQueue -> Right (MemoryBackend (defaultMemoryQueueConfig (cfgQueueMemoryMaxDepth env)))
    SqsQueue -> case (regionE, urlE, resolveSqsEndpoint env) of
        (Right region, Right url, Right endpoint) ->
            Right (SqsBackend (defaultSqsConfig (unUrl url) region){sqsEndpoint = endpoint})
        (_, _, endpointE) ->
            -- Aggregate every SQS-resolution failure (missing region, missing queue
            -- URL, malformed endpoint) so one boot reports them all at once.
            Left (lefts [void regionE, void urlE] <> fromLeft [] endpointE)
  where
    -- AWS_REGION, required to scope the SQS queue; a blank value is treated as absent.
    regionE :: Either BootError Text
    regionE = case T.strip <$> cfgAwsRegion env of
        Just region | not (T.null region) -> Right region
        _ -> Left QueueRegionMissing

    -- ECLUSE_QUEUE_URL is optional at the env layer; it is required here for SQS (the
    -- jobs need a queue to be sent to), an absent one being a fail-loud boot error.
    urlE :: Either BootError Url
    urlE = maybe (Left (QueueUrlMissing SqsQueue)) Right (cfgQueueUrl env)

{- | The loud boot warning a 'MirrorQueuePlan' warrants before its queue is built, or
'Nothing' for a durable backend that needs none. The composition root logs the
'Just' at @WarningS@ on selection, so an operator who chose the in-memory backend is
told plainly that the mirror is non-durable -- never a silent surprise.
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
        <> "ECLUSE_QUEUE_MEMORY_MAX_DEPTH to shed fewer under load."

{- Resolve the optional SQS endpoint override into an 'SqsEndpoint', or 'Nothing' for
AWS's default resolution. The AWS-SDK-standard @AWS_ENDPOINT_URL_SQS@ takes precedence
over the generic @AWS_ENDPOINT_URL@; the override URL is parsed into its TLS flag,
host, and port, and the request signing keys are taken from the standard
@AWS_ACCESS_KEY_ID@\/@AWS_SECRET_ACCESS_KEY@ (an emulator is off the ambient chain).
A malformed override URL is a fail-loud boot error. -}
resolveSqsEndpoint :: AppConfig -> Either [BootError] (Maybe SqsEndpoint)
resolveSqsEndpoint env =
    case nonBlank =<< cfgAwsEndpointUrlSqs env of
        Nothing -> Right Nothing
        Just url -> case parseEndpointUrl url of
            Just (secure, host, port) ->
                Right
                    ( Just
                        SqsEndpoint
                            { endpointSecure = secure
                            , endpointHost = host
                            , endpointPort = port
                            }
                    )
            Nothing -> Left [QueueEndpointMalformed url]

{- Parse an endpoint URL into its (TLS flag, host, port). The scheme picks the TLS
flag and the default port (443\/80) when none is given; an absent scheme or a
non-numeric port yields 'Nothing'. The @host[:port]@ authority is split by the
shared bracket-aware 'Ecluse.Core.Security.splitHostPort', so a bracketed IPv6 literal
(@[::1]:4566@) is split on its closing bracket, not on an inner colon, and the host
is returned without brackets -- the same primitive the data-plane host extractor
uses, so the two cannot drift on an authority edge case. -}
parseEndpointUrl :: Text -> Maybe (Bool, Text, Int)
parseEndpointUrl raw = do
    (secure, afterScheme) <-
        ((True,) <$> T.stripPrefix "https://" raw) <|> ((False,) <$> T.stripPrefix "http://" raw)
    let authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
    (hostText, portText) <- splitHostPort authority
    host <- nonBlank hostText
    port <- case T.stripPrefix ":" portText of
        Nothing -> Just (if secure then 443 else 80)
        Just digits -> readMaybe (toString digits)
    pure (secure, host, port)

{- | The metadata-cache tunables drawn from the validated environment layer -- its
TTL and entry bound -- so a deployment's cache settings flow from config rather than
the built-in defaults (see "Ecluse.Core.Server.Cache").
-}
cacheConfigFor :: AppConfig -> CacheConfig
cacheConfigFor env =
    CacheConfig
        { cacheTtl = cfgCacheTtl env
        , cacheMaxEntries = cfgCacheMaxEntries env
        , cacheMaxBytes = cfgCacheMaxBytes env
        }
