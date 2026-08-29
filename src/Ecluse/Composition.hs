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
import Ecluse.Core.Registry.Npm.Publish (npmPublishAllowed)
import Ecluse.Core.Rules (RuleDeps, prepare, rdCurrentAdvisoryEtag)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission)
import Ecluse.Core.Server.Context (MountBinding, PackumentDeps (..), PublishDeps (..))
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite), mountUpstreams)
import Ecluse.Core.Text (stripTrailingSlash)

{- | The composition root's single entry to the served mount bindings, or every boot error
at once. The caller injects every capability, so this opens no socket.
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

{- | The publish-side byte discipline: the process-wide aggregate admission and the
per-request cap. It exists exactly when a publication target is configured.
-}
data PublishBudget = PublishBudget
    { pbBodyBudget :: ByteAdmission
    , pbMaxRequestBytes :: Int
    }

{- | Turn a validated 'Config' into the served 'MountBinding's, or the boot errors aggregated
across every mount. The 'Limits' arrive resolved, so every metadata read is bounded.
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
    -- 'Ecluse.Composition.Plan.resolveBootPlan' runs 'validateComposition' first on both
    -- entry points. This call keeps the structural errors in reach of a caller without a plan.
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

    {- It checks the credential reference and the adapter even when one has already
    failed, so a mount missing both reports both in one run. -}
    bindingFor :: Maybe PublishDeps -> Mount -> MountConfig -> IO (Either [BootError] MountBinding)
    bindingFor pubDeps mount mcfg =
        case adapterFor eco of
            -- 'validateComposition' already reported the missing adapter, so only the
            -- credential reference is still this mount's to check.
            Nothing -> pure (Left (maybeToList (credentialError providers mount)))
            Just adapter -> do
                deps <- packumentDepsFor adapter mount mcfg
                pure $ case (credentialError providers mount, resolveAdapter eco deps pubDeps) of
                    (Nothing, Just binding) -> Right binding
                    (mCredErr, mBinding) ->
                        Left (maybeToList mCredErr <> [MissingAdapter eco | isNothing mBinding])
      where
        eco = mountEcosystem mount

    {- The ecosystem-shaped fields carry over the adapter's capabilities unchanged, and
    the rest is the mount's configuration. @mountBaseUrl@ owns the @dist.tarball@ base. -}
    packumentDepsFor :: RegistryAdapter -> Mount -> MountConfig -> IO PackumentDeps
    packumentDepsFor adapter mount mcfg = do
        -- 'prepare' allocates an effectful rule's resilience policy and breaker once per mount.
        -- The deps below bridge that same 'RuleDeps' non-pinning advisory-ETag reader.
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

-- A serve-only mount never writes, so it references no provider and can never fail here.
credentialError :: CredentialProviders -> Mount -> Maybe BootError
credentialError providers mount = case regMirrorTarget (mountRegistries mount) of
    Nothing -> Nothing
    Just _ ->
        if mountEcosystem mount `Set.member` initializedEcosystems providers
            then Nothing
            else Just (UnresolvedCredential (mountEcosystem mount))

-- Absolute under ECLUSE_SERVER__PUBLIC_URL when set, otherwise the relative prefix path.
-- A real install path must set it: an npm client reads a leading slash as a @file:@ path.
mountBaseUrl :: Maybe Url -> Ecosystem -> Text
mountBaseUrl publicUrl eco =
    case publicUrl of
        Nothing -> mountBasePath eco
        Just public -> stripTrailingSlash (unUrl public) <> mountBasePath eco

-- The relative path a client's registry endpoint maps onto (npm becomes /npm).
mountBasePath :: Ecosystem -> Text
mountBasePath eco = "/" <> T.intercalate "/" (toList (prefixFor eco))

{- | The pure structural validation a boot enforces beyond 'Ecluse.Config.loadConfig'. It
leaves provider initialisation ('UnresolvedCredential') to 'composeBindings'.
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

{- | Build the first-party publish dependencies, shared across the mounts. 'Nothing' with no
publication target configured, so the publish path is off and a @PUT \/{pkg}@ answers @405@.
-}
publishDepsFor :: Maybe RegistryAdapter -> AppConfig -> MountConfig -> Limits -> Maybe PublishBudget -> Maybe HelpMessage -> Maybe PublishDeps
publishDepsFor mAdapter app mcfg limits publishBudget helpMessage = do
    url <- mntPublicationTarget mcfg
    adapter <- mAdapter
    budget <- publishBudget
    pure
        PublishDeps
            { pubTargetUrl = registryUrlText url
            , pubAllowed = npmPublishAllowed (mntPublishAllow mcfg)
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

-- The caller applies this only to a mount with a configured publication target.
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

{- | One ecosystem's resolved publish target: the endpoint the worker writes approved
artifacts to, and the provider that mints its bearer token. Resolved once, not per request.
-}
data PublishTarget = PublishTarget
    { ptEcosystem :: Ecosystem
    -- ^ The ecosystem this publish target serves.
    , ptMirrorUrl :: Text
    -- ^ The mirror-target endpoint the worker publishes approved artifacts to.
    , ptCredentials :: CredentialProvider
    -- ^ The provider minting the mirror-target write token.
    }

{- | Resolve each configured mount to its publish target, or the aggregated boot errors. An
unresolved credential raises the same error 'composeBindings' reports for the serve side.
-}
planPublishTargets ::
    CredentialProviders ->
    Config ->
    Either [BootError] [PublishTarget]
planPublishTargets = composePublishTargets

composePublishTargets ::
    CredentialProviders ->
    Config ->
    Either [BootError] [PublishTarget]
composePublishTargets providers config =
    case partitionEithers (mapMaybe (publishTargetFor providers) (Map.elems (configMounts config))) of
        ([], targets) -> Right targets
        (errs, _) -> Left (concat errs)

-- 'Nothing' for a serve-only mount, which writes nothing and has no target. A
-- mirrored mount without an initialised provider is the unresolved-credential error.
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
