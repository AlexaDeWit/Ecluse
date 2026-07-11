-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

The composition root's other concerns live in the sibling modules: the boot-error
vocabulary and rendering in "Ecluse.Composition.BootError", the credential
providers and mirror-target credential selection in
"Ecluse.Composition.Credential", the mirror-queue backend selection in
"Ecluse.Composition.MirrorQueue", and the config-derived runtime sizings in
"Ecluse.Composition.Sizing".

== Fail-fast at boot

Three boot failures are aggregated into one report so a single run shows every
problem: a rule policy that does not resolve ('PolicyBootError', surfaced by
'Ecluse.Config.loadConfig'), a configured mount whose ecosystem has no adapter
wired ('MissingAdapter'), and a mount naming a credential backend with no
initialised provider ('UnresolvedCredential'). A bad configuration is thus a
loud, immediate startup failure, never a quietly mis-enforced or half-wired state
(see @docs\/architecture\/configuration.md@ → "Validation").
-}
module Ecluse.Composition (
    -- * Boot-time wiring
    planMounts,
    composeBindings,

    -- * Publish-side wiring
    PublishTarget (..),
    planPublishTargets,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Credential (CredentialProviders, initializedEcosystems, lookupProvider)
import Ecluse.Config (
    AppConfig (..),
    Config (..),
    MirrorTarget (mtCredential, mtUrl),
    Mount (..),
    MountConfig (..),
    MountRegistries (..),
    Url,
    unUrl,
 )
import Ecluse.Core.Credential (CredentialProvider, Secret)
import Ecluse.Core.Ecosystem (Ecosystem, prefixFor)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterArtifact,
    adapterFor,
    adapterMetadata,
    adapterPublish,
    artifactByFile,
    artifactByUrl,
    metadataAssemble,
    metadataNewClient,
    publishCanonicaliseName,
    publishRelay,
 )
import Ecluse.Core.Rules (RuleDeps, prepare, rdCurrentAdvisoryEtag)
import Ecluse.Core.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), tarballHostGate)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Core.Server.Context (MountBinding, PackumentDeps (..), PublishDeps (..))
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)

{- | Validate the environment layer and optional document into the served mount
bindings, or the aggregated boot errors. The composition root's single entry: it
runs 'loadConfig' (whose policy errors become 'PolicyBootError's) and then
'composeBindings', so policy, missing-adapter, and unresolved-credential failures
all surface from one call.

The ecosystem-to-adapter resolver, the wall-clock source, and the rules' boot-bound
capabilities are injected (the composition root supplies @mountBindingFor@,
'Data.Time.getCurrentTime', and each ecosystem's 'RuleDeps'), so this validation
opens no socket. The capabilities are per ecosystem because a mount's rules must
borrow /their/ ecosystem's advisory database, never a neighbour's.
It is 'IO' only because 'composeBindings' 'prepare's each mount's rules (allocating
per-rule engine state once at boot).
-}
planMounts ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    (Ecosystem -> RuleDeps) ->
    CredentialProviders ->
    Config ->
    IO (Either [BootError] [MountBinding])
planMounts = composeBindings

{- | Turn a validated 'Config' into the served 'MountBinding's, or the aggregated
boot errors. For each mount, in ecosystem order: its credential reference must
resolve to an initialised provider, and its ecosystem must resolve to an adapter
(through the injected resolver, fed real 'PackumentDeps' so the packument route is
served rather than the @501@ stub). Errors aggregate across every mount.
-}
composeBindings ::
    (Ecosystem -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    (Ecosystem -> RuleDeps) ->
    CredentialProviders ->
    Config ->
    IO (Either [BootError] [MountBinding])
composeBindings resolveAdapter clock ruleDepsFor providers config = do
    let (pubErrs, pubDepsMap) = case sequence (Map.mapWithKey (\eco mcfg -> publishDepsFor eco (adapterFor eco) app mcfg limits helpMessage) (cfgMounts app)) of
            Left errs -> (errs, Map.empty)
            Right m -> ([], m)
    -- Each resolved mount paired with its environment-layer 'MountConfig':
    -- 'Ecluse.Config.loadConfig' derives 'configMounts' from 'cfgMounts' entry for
    -- entry, so the two maps share a keyset and the pairing is total.
    let mounts = Map.elems (Map.intersectionWith (,) (configMounts config) (cfgMounts app))
    bindingResults <- traverse (\(mount, mcfg) -> bindingFor (join (Map.lookup (mountEcosystem mount) pubDepsMap)) mount mcfg) mounts
    pure $ case (pubErrs, partitionEithers bindingResults) of
        ([], ([], bindings)) -> Right bindings
        (_, (errs, _)) -> Left (pubErrs <> concat errs)
  where
    app :: AppConfig
    app = configApp config

    -- The response-bound budget every mount enforces on its upstream fetches and
    -- decodes (security.md invariant 4), assembled from the validated environment
    -- ceilings. Carried onto each mount's deps so the data plane reads the metadata
    -- body bounded, and refuses an over-deep or version-flooded document fail-closed.
    limits :: Limits
    limits =
        Limits
            { maxBodyBytes = cfgMaxResponseBytes app
            , maxVersionCount = cfgMaxVersionCount app
            , maxNestingDepth = cfgMaxNestingDepth app
            }

    -- The operator help message, derived from the environment layer like the
    -- inbound token, so every mount's denials carry it.
    helpMessage :: Maybe HelpMessage
    helpMessage = mkHelpMessage <$> cfgHelpMessage app

    {- Resolve one mount to its binding, or the boot errors that block it. Both the
    credential reference and the adapter are checked even when one already failed,
    so a mount missing both reports both in one run rather than one at a time. The
    packument-serve dependencies are projected from the mount ecosystem's registered
    adapter ('adapterFor'), so a mount whose ecosystem has none carries no deps and
    resolves to the missing-adapter error; the resolved publish dependencies (shared
    across mounts) are passed to the resolver so the binding carries the first-party
    publish wiring. -}
    bindingFor :: Maybe PublishDeps -> Mount -> MountConfig -> IO (Either [BootError] MountBinding)
    bindingFor pubDeps mount mcfg = do
        deps <- traverse (\adapter -> packumentDepsFor adapter mount mcfg) (adapterFor (mountEcosystem mount))
        pure $ case (credentialError providers mount, resolveAdapter (mountEcosystem mount) deps pubDeps) of
            (Nothing, Just binding) -> Right binding
            (mCredErr, mBinding) ->
                Left (maybeToList mCredErr <> [MissingAdapter (mountEcosystem mount) | isNothing mBinding])

    {- Build a mount's 'PackumentDeps' from its ecosystem's registered adapter, its
    registries, resolved rules, the inbound edge token, the injected clock, and the
    operator help message. The ecosystem-shaped fields (the metadata client
    constructor, the artifact request builders, the packument assembly) are the
    adapter's capability fields carried over unchanged; everything else is the
    mount's configuration. The mount's externally-visible base URL drives the
    @dist.tarball@ rewrite: an __absolute__ URL under @ECLUSE_PUBLIC_URL@
    (@{public}\/npm\/{pkg}\/-\/{file}@) when one is configured, so an @npm@ client
    fetches the artifact back through the proxy on the gated path; otherwise the
    relative prefix path (@\/npm@), retained for compatibility -- but note @npm@
    cannot consume a relative @dist.tarball@ (it reads a leading slash as a @file:@
    path), so a real install path must set @ECLUSE_PUBLIC_URL@ (see @mountBaseUrl@
    and @docs\/architecture\/hosting.md@ → "URL rewriting"). -}
    packumentDepsFor :: RegistryAdapter -> Mount -> MountConfig -> IO PackumentDeps
    packumentDepsFor adapter mount mcfg = do
        -- Prepare the resolved policy into the engine's runtime rules, closing the
        -- injected 'RuleDeps' into them; an effectful rule (AllowIfRemediatesCve)
        -- gets its resilience policy and breaker allocated here, once per mount.
        -- The same RuleDeps' non-pinning advisory-ETag reader is bridged onto the
        -- deps below, since the serve gate is where the per-request EvalContext is built.
        let ruleDeps = ruleDepsFor (mountEcosystem mount)
        prepared <- prepare ruleDeps (mountPolicy mount)
        let regs = mountRegistries mount
        pure
            PackumentDeps
                { pdPrivateBaseUrl = registryUrlText (regPrivateUpstream regs)
                , pdPublicBaseUrl = registryUrlText (regPublicUpstream regs)
                , pdMountBaseUrl = mountBaseUrl (cfgPublicUrl app) (mountEcosystem mount)
                , pdMirrorTarget = registryUrlText (mtUrl (regMirrorTarget regs))
                , pdRules = prepared
                , pdTarballHostPolicy = tarballHostPolicyFor mcfg
                , -- The operator-configured ranges extending the fixed internal-range block on
                  -- the dist.tarball host gate; the same list applies to every mount, since which
                  -- internal ranges exist on an operator's network is a deployment-wide fact.
                  pdAdditionalBlockedRanges = cfgAdditionalBlockedRanges app
                , -- The tarball-host gate's mount-constant inputs (allowlist + private and
                  -- public hosts), extracted once here so the hot artifact path parses no
                  -- URL and rebuilds no host set per request.
                  pdTarballHostGate =
                    tarballHostGate
                        (registryUrlText (regPrivateUpstream regs))
                        (registryUrlText (regPublicUpstream regs))
                        (registryUrlText (mtUrl (regMirrorTarget regs)))
                , pdLimits = limits
                , pdInboundToken = cfgAuthToken app
                , pdNow = clock
                , pdAdvisoryEtag = rdCurrentAdvisoryEtag ruleDeps
                , pdHelp = helpMessage
                , -- The global public-integrity admission floor, validated at config
                  -- load, carried onto every mount's deps so the public gate refuses
                  -- a below-floor version.
                  pdMinIntegrity = cfgMinPublicIntegrity app
                , -- The global trusted-integrity admission floor (default SHA-256,
                  -- loosenable below it), carried onto every mount's deps so the
                  -- trusted gate drops a below-floor private version from the listing
                  -- and gates the private artifact serve.
                  pdMinTrustedIntegrity = cfgMinTrustedIntegrity app
                , -- The operator's cross-upstream divergence policy (default warn),
                  -- carried onto every mount's deps so the serve path withholds a
                  -- contested version under fail-closed.
                  pdDivergencePolicy = cfgDivergencePolicy app
                , pdNewMetadataClient = metadataNewClient (adapterMetadata adapter)
                , pdBuildArtifactRequestByFile = artifactByFile (adapterArtifact adapter)
                , pdBuildArtifactRequestByUrl = artifactByUrl (adapterArtifact adapter)
                , pdAssemble = metadataAssemble (adapterMetadata adapter)
                , pdEgressUrl = mkRegistryUrl
                }

-- The resolved tarball-host policy of a mount, from the secure-default
-- environment toggle: honour a cross-host dist.tarball only when explicitly
-- opted in (and even then, never past the allowlist or the internal block).
tarballHostPolicyFor :: MountConfig -> TarballHostPolicy
tarballHostPolicyFor mcfg =
    if mntRespectUpstreamTarballHost mcfg
        then AnyAllowlistedHost
        else SameHostAsPackument

-- The credential reference of a mount: an error when the named backend is not
-- initialised, nothing when it resolves.
credentialError :: CredentialProviders -> Mount -> Maybe BootError
credentialError providers mount =
    let backend = mtCredential (regMirrorTarget (mountRegistries mount))
     in if mountEcosystem mount `Set.member` initializedEcosystems providers
            then Nothing
            else Just (UnresolvedCredential (mountEcosystem mount) backend)

-- A mount's externally-visible base URL for the dist.tarball rewrite. Absolute
-- under ECLUSE_PUBLIC_URL when set (so a served tarball is a full URL an npm
-- client can fetch); otherwise the relative prefix path, retained for
-- compatibility. A trailing slash on the configured URL is dropped so the join
-- with the leading-slash mount path yields exactly one separator.
mountBaseUrl :: Maybe Url -> Ecosystem -> Text
mountBaseUrl publicUrl eco =
    case publicUrl of
        Nothing -> mountBasePath eco
        Just public -> T.dropWhileEnd (== '/') (unUrl public) <> mountBasePath eco

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
response bounds ('Limits') and help message are shared with the read paths and passed in;
the relay and the name canonicaliser are the ecosystem's own capability, projected from
its registered adapter. A mount whose ecosystem has no adapter carries no publish deps
(its errors above still accumulate, and its boot fails on the missing adapter regardless).
-}
publishDepsFor :: Ecosystem -> Maybe RegistryAdapter -> AppConfig -> MountConfig -> Limits -> Maybe HelpMessage -> Either [BootError] (Maybe PublishDeps)
publishDepsFor eco mAdapter app mcfg limits helpMessage = case mntPublicationTarget mcfg of
    Nothing -> Right Nothing
    Just url -> case publishBootErrors eco mcfg inboundToken of
        [] ->
            Right $
                mAdapter <&> \adapter ->
                    PublishDeps
                        { pubTargetUrl = registryUrlText url
                        , pubScopes = mntPublishScopes mcfg
                        , pubStaticToken = mntPublicationTargetToken mcfg
                        , pubInboundToken = inboundToken
                        , pubLimits = limits
                        , pubHelp = helpMessage
                        , pubRelayPublish = publishRelay (adapterPublish adapter)
                        , pubCanonicaliseName = publishCanonicaliseName (adapterPublish adapter)
                        }
        errs -> Left errs
  where
    inboundToken :: Maybe Secret
    inboundToken = cfgAuthToken app

-- The accumulated fail-loud publish boot errors for a configured publication
-- target: a missing publish-scope allow-list, and a static publish credential
-- without a verifiable inbound edge, reported together.
publishBootErrors :: Ecosystem -> MountConfig -> Maybe Secret -> [BootError]
publishBootErrors eco mcfg inboundToken = catMaybes [scopesError, edgeError]
  where
    scopesError, edgeError :: Maybe BootError
    scopesError
        | null (mntPublishScopes mcfg) = Just (PublishScopesMissing eco)
        | otherwise = Nothing
    edgeError
        | isJust (mntPublicationTargetToken mcfg) && isNothing inboundToken = Just (PublishStaticCredentialNeedsEdge eco)
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
    case partitionEithers (map (publishTargetFor providers) (Map.elems (configMounts config))) of
        ([], targets) -> Right targets
        (errs, _) -> Left (concat errs)

-- One mount's publish target: its mirror-target endpoint paired with the
-- initialised write provider, or the same unresolved-credential boot error the
-- serve side reports.
publishTargetFor :: CredentialProviders -> Mount -> Either [BootError] PublishTarget
publishTargetFor providers mount =
    case lookupProvider (mountEcosystem mount) providers of
        Just provider ->
            Right
                PublishTarget
                    { ptEcosystem = mountEcosystem mount
                    , ptMirrorUrl = registryUrlText (mtUrl target)
                    , ptCredentials = provider
                    }
        Nothing ->
            Left [UnresolvedCredential (mountEcosystem mount) (mtCredential target)]
  where
    target = regMirrorTarget (mountRegistries mount)
