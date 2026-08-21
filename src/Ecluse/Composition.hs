-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition-root wiring: turn a validated 'Config' and the process-global
credential providers into the served 'MountBinding's. Any boot problem fails fast,
__aggregated__ with the rest.

This is the __listener-free__ heart of the composition root ("Ecluse" calls it). It
holds no socket, no network, and no clock of its own. The caller injects the clock
and the ecosystem-to-adapter resolver, so a unit test runs the boot-time validation
without opening a listener. Its one effect is 'Ecluse.Core.Rules.prepare' on each
mount's rule set. That allocates per-rule engine state once at boot: a breaker for a
resilient rule, though the built-in rules need none today. Binding assembly is
therefore 'IO'. Everything else stays a pure function of the validated config.

The composition root's other concerns live in the sibling modules:

* "Ecluse.Composition.BootError" holds the boot-error vocabulary and its rendering.
* "Ecluse.Composition.Credential" holds the credential providers and the
  mirror-target credential selection.
* "Ecluse.Composition.MirrorQueue" holds the mirror-queue backend selection.
* "Ecluse.Composition.Sizing" holds the config-derived runtime sizings.

== Fail-fast at boot

One report aggregates three boot failures, so a single run shows every problem:

* A rule policy does not resolve ('PolicyBootError', surfaced by
  'Ecluse.Config.loadConfig').
* A configured mount's ecosystem has no adapter wired ('MissingAdapter').
* A mount has no initialised mirror-write provider ('UnresolvedCredential').

A bad configuration is a loud, immediate startup failure, never a quietly
mis-enforced or half-wired state (see
@docs\/architecture\/configuration.md@ → "Validation").
-}
module Ecluse.Composition (
    -- * Boot-time wiring
    planMounts,
    composeBindings,
    validateComposition,

    -- * Publish-side wiring
    PublishBudget (..),
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
    EgressSettings (..),
    IntegritySettings (..),
    MirrorTarget (mtUrl),
    Mount (..),
    MountConfig (..),
    MountRegistries (..),
    ServerSettings (..),
    Url,
    regMirrorTarget,
    regPrivateUpstream,
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
    artifactHosts,
    metadataAssemble,
    metadataNewClient,
    metadataSerialise,
    publishCanonicaliseName,
    publishDeclaredNames,
    publishRelay,
 )
import Ecluse.Core.Rules (RuleDeps, prepare, rdCurrentAdvisoryEtag)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission)
import Ecluse.Core.Server.Context (MountBinding, PackumentDeps (..), PublishDeps (..))
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite), mountUpstreams)

{- | Validate the environment layer and optional document into the served mount
bindings, or the aggregated boot errors. This is the composition root's single
entry. It runs 'loadConfig' (whose policy errors become 'PolicyBootError's) and then
'composeBindings', so policy, missing-adapter, and unresolved-credential failures
all surface from one call.

The caller injects the ecosystem-to-adapter resolver, the wall-clock source, and the
rules' boot-bound capabilities: the composition root supplies @mountBindingFor@,
'Data.Time.getCurrentTime', and each ecosystem's 'RuleDeps'. This validation
therefore opens no socket. The capabilities are per ecosystem because a mount's
rules must borrow /their/ ecosystem's advisory database, never a neighbour's. It is
'IO' only because 'composeBindings' 'prepare's each mount's rules, which allocates
per-rule engine state once at boot.
-}
planMounts ::
    (Ecosystem -> PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    (Ecosystem -> RuleDeps) ->
    CredentialProviders ->
    Limits ->
    Maybe PublishBudget ->
    Config ->
    IO (Either [BootError] [MountBinding])
planMounts = composeBindings

{- | The publish-side byte discipline every publishing mount runs under: the
process-wide aggregate byte-admission and the per-request cap (the chunked-body
weight). The composition root builds it from the memory plan's publish tenant.
Present exactly when a publication target is configured. The tenant and the target
derive from the same predicate, so a publishing mount without a budget is
unrepresentable at the root.
-}
data PublishBudget = PublishBudget
    { pbBodyBudget :: ByteAdmission
    , pbMaxRequestBytes :: Int
    }

{- | Turn a validated 'Config' into the served 'MountBinding's, or the aggregated
boot errors. Each mount, in ecosystem order, must resolve its credential reference
to an initialised provider and its ecosystem to an adapter. The injected resolver
does the second, and the mount's 'PackumentDeps' are built from it. Errors aggregate
across every mount.

The 'Limits' arrive resolved: the byte cap from the memory plan
("Ecluse.Composition.MemoryPlan"), married to the pinned structural counts. Every
mount's deps carry them, so the data plane reads each metadata body bounded
(security.md invariant 4).
-}
composeBindings ::
    (Ecosystem -> PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding) ->
    IO UTCTime ->
    (Ecosystem -> RuleDeps) ->
    CredentialProviders ->
    Limits ->
    Maybe PublishBudget ->
    Config ->
    IO (Either [BootError] [MountBinding])
composeBindings resolveAdapter clock ruleDepsFor providers limits publishBudget config = do
    -- The pure structural refusals (missing adapters, publish policy) come from
    -- 'validateComposition'. That is the function @ecluse check-config@ runs too,
    -- so the checker and the boot cannot drift on what is refused.
    let structuralErrs = validateComposition config
        pubDepsMap = Map.mapWithKey (\eco mcfg -> publishDepsFor (adapterFor eco) app mcfg limits publishBudget helpMessage) (cfgMounts app)
    -- 'Ecluse.Config.loadConfig' derives 'configMounts' from 'cfgMounts' entry for
    -- entry, so the two maps share a keyset and this pairing is total.
    let mounts = Map.elems (Map.intersectionWith (,) (configMounts config) (cfgMounts app))
    bindingResults <- traverse (\(mount, mcfg) -> bindingFor (join (Map.lookup (mountEcosystem mount) pubDepsMap)) mount mcfg) mounts
    pure $ case (structuralErrs, partitionEithers bindingResults) of
        ([], ([], bindings)) -> Right bindings
        (_, (errs, _)) -> Left (structuralErrs <> concat errs)
  where
    app :: AppConfig
    app = configApp config

    -- The operator help message, derived from the environment layer like the
    -- inbound token, so every mount's denials carry it.
    helpMessage :: Maybe HelpMessage
    helpMessage = mkHelpMessage <$> srvHelpMessage (cfgServer app)

    {- Resolve one mount to its binding, or the boot errors that block it. It checks
    both the credential reference and the adapter even when one has already failed,
    so a mount missing both reports both in one run. The packument-serve dependencies
    come from the mount ecosystem's registered adapter ('adapterFor'). A mount whose
    ecosystem has none is the missing-adapter error, never a half-wired
    mount. The resolved publish dependencies, shared across mounts, go to the
    resolver, so the binding carries the first-party publish wiring. -}
    bindingFor :: Maybe PublishDeps -> Mount -> MountConfig -> IO (Either [BootError] MountBinding)
    bindingFor pubDeps mount mcfg =
        case adapterFor eco of
            -- No adapter for this ecosystem: nothing to build deps from, and no
            -- mount to bind. 'validateComposition' already reported the missing
            -- adapter, so only the credential reference is still this mount's to check.
            Nothing -> pure (Left (maybeToList (credentialError providers mount)))
            Just adapter -> do
                deps <- packumentDepsFor adapter mount mcfg
                pure $ case (credentialError providers mount, resolveAdapter eco deps pubDeps) of
                    (Nothing, Just binding) -> Right binding
                    (mCredErr, mBinding) ->
                        Left (maybeToList mCredErr <> [MissingAdapter eco | isNothing mBinding])
      where
        eco = mountEcosystem mount

    {- Build a mount's 'PackumentDeps' from its ecosystem's registered adapter, its
    registries, resolved rules, the inbound edge token, the injected clock, and the
    operator help message. The ecosystem-shaped fields (the metadata client
    constructor, the artifact request builders, the packument assembly) carry over the
    adapter's capability fields unchanged. Everything else is the mount's configuration.

    The mount's externally-visible base URL drives the @dist.tarball@ rewrite. With
    @ECLUSE_SERVER__PUBLIC_URL@ configured it is an __absolute__ URL
    (@{public}\/npm\/{pkg}\/-\/{file}@), so an @npm@ client fetches the artifact back
    through the proxy on the gated path. Otherwise it is the relative prefix path
    (@\/npm@), retained for compatibility. An @npm@ client cannot consume a relative
    @dist.tarball@: it reads a leading slash as a @file:@ path. A real install path
    must therefore set @ECLUSE_SERVER__PUBLIC_URL@ (see @mountBaseUrl@ and
    @docs\/architecture\/web-layer.md@ → "Multi-ecosystem mounts"). -}
    packumentDepsFor :: RegistryAdapter -> Mount -> MountConfig -> IO PackumentDeps
    packumentDepsFor adapter mount mcfg = do
        -- Preparing the resolved policy closes the injected 'RuleDeps' into the
        -- engine's runtime rules. An effectful rule (AllowIfRemediatesCve) gets its
        -- resilience policy and breaker allocated here, once per mount. The deps
        -- below bridge the same RuleDeps' non-pinning advisory-ETag reader, because
        -- the serve gate builds the per-request EvalContext.
        let ruleDeps = ruleDepsFor (mountEcosystem mount)
        prepared <- prepare ruleDeps (mountPolicy mount)
        let regs = mountRegistries mount
        pure
            PackumentDeps
                { -- The leading argument is the adapter's declared artifact hosts: the
                  -- ecosystem's own same-host equivalence for the tarball gate.
                  pdUpstreams =
                    mountUpstreams
                        (artifactHosts (adapterArtifact adapter))
                        (registryUrlText <$> regPrivateUpstream regs)
                        (registryUrlText (regPublicUpstream regs))
                        (maybe NoMirrorWrite (MirrorOnAdmit . registryUrlText . mtUrl) (regMirrorTarget regs))
                , pdMountBaseUrl = mountBaseUrl (srvPublicUrl (cfgServer app)) (mountEcosystem mount)
                , pdRules = prepared
                , -- The operator-configured ranges extending the fixed internal-range block
                  -- on the dist.tarball host gate. The same list applies to every mount,
                  -- because a network's internal ranges are a deployment-wide fact.
                  pdAdditionalBlockedRanges = egrAdditionalBlockedRanges (cfgEgress app)
                , pdLimits = limits
                , pdInboundToken = srvAuthToken (cfgServer app)
                , pdNow = clock
                , pdAdvisoryEtag = rdCurrentAdvisoryEtag ruleDeps
                , pdHelp = helpMessage
                , -- The global public-integrity admission floor, validated at config
                  -- load, carried onto every mount's deps so the public gate refuses
                  -- a below-floor version.
                  pdMinIntegrity = intMinPublic (cfgIntegrity app)
                , -- The trusted-integrity admission floor: the global default
                  -- (SHA-256, loosenable below it), refined per mount so a legacy
                  -- registry's loosening never leaks onto a neighbouring mount.
                  pdMinTrustedIntegrity = fromMaybe (intMinTrusted (cfgIntegrity app)) (mntMinTrustedIntegrity mcfg)
                , -- The cross-upstream divergence policy: the global default
                  -- (warn), refined per mount for the same reason.
                  pdDivergencePolicy = fromMaybe (intDivergencePolicy (cfgIntegrity app)) (mntDivergencePolicy mcfg)
                , pdNewMetadataClient = metadataNewClient (adapterMetadata adapter)
                , pdBuildArtifactRequestByFile = artifactByFile (adapterArtifact adapter)
                , pdBuildArtifactRequestByUrl = artifactByUrl (adapterArtifact adapter)
                , pdAssemble = metadataAssemble (adapterMetadata adapter)
                , pdSerialise = metadataSerialise (adapterMetadata adapter)
                , pdEgressUrl = mkRegistryUrl
                }

-- The credential reference of a mount: an error when a mirrored mount's write
-- backend is not initialised, nothing when it resolves. A serve-only mount never
-- writes, so it references no provider and can never fail here.
credentialError :: CredentialProviders -> Mount -> Maybe BootError
credentialError providers mount = case regMirrorTarget (mountRegistries mount) of
    Nothing -> Nothing
    Just _ ->
        if mountEcosystem mount `Set.member` initializedEcosystems providers
            then Nothing
            else Just (UnresolvedCredential (mountEcosystem mount))

-- A mount's externally-visible base URL for the dist.tarball rewrite. Absolute
-- under ECLUSE_SERVER__PUBLIC_URL when set, so a served tarball is a full URL an npm
-- client can fetch. Otherwise the relative prefix path, retained for compatibility.
-- The join drops a trailing slash on the configured URL, so it yields exactly one
-- separator against the leading-slash mount path.
mountBaseUrl :: Maybe Url -> Ecosystem -> Text
mountBaseUrl publicUrl eco =
    case publicUrl of
        Nothing -> mountBasePath eco
        Just public -> T.dropWhileEnd (== '/') (unUrl public) <> mountBasePath eco

-- The mount's externally-visible base path, derived from its ecosystem prefix
-- (@npm@ → @\/npm@): a leading slash and the prefix segments joined. This is the
-- relative path a client's registry endpoint maps onto.
mountBasePath :: Ecosystem -> Text
mountBasePath eco = "/" <> T.intercalate "/" (toList (prefixFor eco))

{- | The pure structural validation a boot enforces beyond 'Ecluse.Config.loadConfig'.
It refuses a served mount whose ecosystem has no registered adapter
('MissingAdapter'). It also checks the publish policy of every configured
publication target ('PublishAllowMissing', 'PublishStaticCredentialNeedsEdge').
The @ecluse check-config@ command shares it, so the checker can never pass a
configuration the proxy refuses.

Pure and side-effect-free: it initialises no provider and mints no credential. The
mirrored-mount credential expectations are already structural on 'Config' itself,
because each mirrored mount carries the credential 'Ecluse.Config.loadConfig'
derived from its target. Only the provider-initialisation check
('UnresolvedCredential') stays with 'composeBindings', which consumes this same
function for everything else.
-}
validateComposition :: Config -> [BootError]
validateComposition config = missingAdapters <> publishPolicyErrors
  where
    app = configApp config
    missingAdapters =
        [MissingAdapter eco | eco <- Map.keys (configMounts config), isNothing (adapterFor eco)]
    publishPolicyErrors =
        concat
            [ publishBootErrors eco mcfg (srvAuthToken (cfgServer app))
            | (eco, mcfg) <- Map.toAscList (cfgMounts app)
            , isJust (mntPublicationTarget mcfg)
            ]

{- | Build the first-party publish dependencies from the environment layer, shared
across the (single-ecosystem) mounts. 'Nothing' when no publication target is
configured, so the publish path is off and a @PUT \/{pkg}@ answers @405@. Also
'Nothing' when the ecosystem has no adapter, in which case the boot fails on the
missing adapter regardless.

Refusing on the publish policy is 'validateComposition''s job. Construction here
assumes it, and only an error-free compose consumes the result. The target's URL,
the scopes, and the static fallback credential are the publish env layer. The read
paths share the response bounds ('Limits') and the help message, and the caller
passes both in. The relay, the name canonicaliser, and the declared-name extractor
are the ecosystem's own capability, projected from its registered adapter.
-}
publishDepsFor :: Maybe RegistryAdapter -> AppConfig -> MountConfig -> Limits -> Maybe PublishBudget -> Maybe HelpMessage -> Maybe PublishDeps
publishDepsFor mAdapter app mcfg limits publishBudget helpMessage = do
    url <- mntPublicationTarget mcfg
    adapter <- mAdapter
    budget <- publishBudget
    pure
        PublishDeps
            { pubTargetUrl = registryUrlText url
            , pubScopes = mntPublishAllow mcfg
            , pubStaticToken = mntPublicationTargetToken mcfg
            , pubInboundToken = inboundToken
            , pubLimits = limits
            , pubBodyBudget = pbBodyBudget budget
            , pubMaxRequestBytes = pbMaxRequestBytes budget
            , pubHelp = helpMessage
            , pubRelayPublish = publishRelay (adapterPublish adapter)
            , pubCanonicaliseName = publishCanonicaliseName (adapterPublish adapter)
            , pubDeclaredNames = publishDeclaredNames (adapterPublish adapter)
            }
  where
    inboundToken :: Maybe Secret
    inboundToken = srvAuthToken (cfgServer app)

-- Two fail-loud boot errors for a configured publication target, reported together:
-- a missing publish-scope allow-list, and a static publish credential without a
-- verifiable inbound edge.
publishBootErrors :: Ecosystem -> MountConfig -> Maybe Secret -> [BootError]
publishBootErrors eco mcfg inboundToken = catMaybes [scopesError, edgeError]
  where
    scopesError, edgeError :: Maybe BootError
    scopesError
        | null (mntPublishAllow mcfg) = Just (PublishAllowMissing eco)
        | otherwise = Nothing
    edgeError
        | isJust (mntPublicationTargetToken mcfg) && isNothing inboundToken = Just (PublishStaticCredentialNeedsEdge eco)
        | otherwise = Nothing

{- | One ecosystem's resolved __publish__ target: the mirror-target endpoint the
mirror worker writes approved artifacts to, paired with the credential provider
that mints its bearer token.

This is the publish side of the per-ecosystem composition (the serve side is the
mount's 'PackumentDeps'). The worker's single consumer builds a registry-protocol
client from these, taking the endpoint as its base URL and the provider's token as
its bearer. The publish client resolves here at the composition root rather than
per request.
-}
data PublishTarget = PublishTarget
    { ptEcosystem :: Ecosystem
    -- ^ The ecosystem this publish target serves.
    , ptMirrorUrl :: Text
    -- ^ The mirror-target endpoint the worker publishes approved artifacts to.
    , ptCredentials :: CredentialProvider
    -- ^ The provider minting the mirror-target write token.
    }

{- | Resolve each configured mount to its publish target, or the aggregated boot
errors. This is the publish side of 'planMounts'. It validates the same config and
resolves each mount's mirror-target endpoint and write credential, so the
composition root can build the worker's publish client.

An unresolved credential reference raises the same fail-loud boot error
'composeBindings' reports for the serve side. The two surfaces therefore never
disagree on what is wired.
-}
planPublishTargets ::
    CredentialProviders ->
    Config ->
    Either [BootError] [PublishTarget]
planPublishTargets = composePublishTargets

-- Resolve every mirrored mount's publish target from a validated config,
-- aggregating an unresolved-credential error per mount (the same check
-- 'composeBindings' applies). A serve-only mount publishes nothing and
-- contributes no target.
composePublishTargets ::
    CredentialProviders ->
    Config ->
    Either [BootError] [PublishTarget]
composePublishTargets providers config =
    case partitionEithers (mapMaybe (publishTargetFor providers) (Map.elems (configMounts config))) of
        ([], targets) -> Right targets
        (errs, _) -> Left (concat errs)

-- One mirrored mount's publish target: its mirror-target endpoint paired with the
-- initialised write provider, or the same unresolved-credential boot error the
-- serve side reports. 'Nothing' for a serve-only mount (no write, no target).
publishTargetFor :: CredentialProviders -> Mount -> Maybe (Either [BootError] PublishTarget)
publishTargetFor providers mount = do
    target <- regMirrorTarget (mountRegistries mount)
    pure $ case lookupProvider (mountEcosystem mount) providers of
        Just provider ->
            Right
                PublishTarget
                    { ptEcosystem = mountEcosystem mount
                    , ptMirrorUrl = registryUrlText (mtUrl target)
                    , ptCredentials = provider
                    }
        Nothing ->
            Left [UnresolvedCredential (mountEcosystem mount)]
