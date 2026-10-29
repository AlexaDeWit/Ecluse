-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bind validated mounts, source views and publication targets without origin discovery.
Live wiring prepares rule state and the credentials required by each role.
-}
module Ecluse.Composition (
    -- * The environment-dependent wiring
    ResolveAdapter,
    WiringPorts (..),
    BootWiring (..),
    resolveBootWiring,

    -- * Boot-time wiring
    planMounts,

    -- * The first-party privilege
    firstPartyName,

    -- * Publish-side wiring
    PublishBudget (..),
    PublishTarget (..),
    planPublishTargets,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Credential (
    BuildCredentials,
    CredentialProviders,
    initCredentialProviders,
    initializedEcosystems,
    lookupProvider,
    noCredentialProviders,
 )
import Ecluse.Composition.Endpoints (publicationReadUrl, publicationTargetUrl)
import Ecluse.Composition.MirrorRole (MirrorMintPlan (MintMirrorWrite, SkipMirrorWrite))
import Ecluse.Composition.Validate (
    ValidatedPlan (vpMounts, vpPublications, vpSettings),
    VettedMount (vmAdapter, vmConfig, vmEcosystem, vmMount),
    VettedPublication (vpubFirstParty, vpubMaxRequestBytes, vpubStaticCredential, vpubTarget),
 )
import Ecluse.Config (
    AppConfig (..),
    EgressSettings (..),
    FirstParty (..),
    IntegritySettings (..),
    MirrorTarget (mtBackend, mtUploadUrl, mtUrl, mtUsername),
    Mount (..),
    MountConfig (..),
    MountIntegrity (..),
    MountRegistries (..),
    ServerSettings (..),
    StoreTag,
    Url,
    regMirrorTarget,
    regPrivateUpstream,
    sbTag,
    unUrl,
 )
import Ecluse.Core.Credential (CredentialProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo), prefixFor)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterArtifact,
    adapterInstallEvidence,
    adapterMetadata,
    adapterProjectName,
    adapterPublish,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts))
import Ecluse.Core.Registry.Cargo.FirstParty (cargoFirstPartyName)
import Ecluse.Core.Registry.Npm.Publish (npmPublishAllowed)
import Ecluse.Core.Registry.PyPI.FirstParty (pypiFirstPartyName)
import Ecluse.Core.Registry.RubyGems.FirstParty (rubyGemsFirstPartyName)
import Ecluse.Core.Rules (RuleDeps, prepare, rdCurrentAdvisoryEtag, rdSourceReporter)
import Ecluse.Core.Rules.Outage (SourceReporter (noteAdmission))
import Ecluse.Core.Security (Limits, maxPublishRequestBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission)
import Ecluse.Core.Server.Context (MountBinding, PackumentDeps (..), PublishDeps (..), ReadSourceSelection (..))
import Ecluse.Core.Server.Response (HelpMessage, mkHelpMessage)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite), mountUpstreams)
import Ecluse.Core.Text (stripTrailingSlash)

-- | Resolve an ecosystem's deps to its mount, 'Nothing' without an adapter. "Ecluse.Service" implements it.
type ResolveAdapter = Ecosystem -> PackumentDeps -> Maybe PackumentDeps -> Maybe PublishDeps -> Maybe MountBinding

-- | The capabilities the wiring is built through, injected so a unit test assembles it with no listener.
data WiringPorts = WiringPorts
    { wpReporters :: Ecosystem -> StoreTag -> CredentialReporters
    -- ^ Reporters keyed by the shared credential's canonical ecosystem and store label.
    , wpBuildCredentials :: BuildCredentials
    -- ^ How this boot builds the mirror-write providers, for the roles that mint them.
    , wpResolveAdapter :: ResolveAdapter
    -- ^ The ecosystem-to-binding resolver, 'Nothing' for an ecosystem this build ships no adapter for.
    , wpClock :: IO UTCTime
    -- ^ The clock every mount's serve path reads.
    , wpRuleDeps :: Ecosystem -> RuleDeps
    -- ^ One ecosystem's rule capabilities, including its advisory-database lookup.
    , wpDeadlines :: Deadlines
    -- ^ The process's sweeper, which bounds native install-evidence inspection.
    }

-- | What only a live environment settles: the served mounts and the worker's publish targets.
data BootWiring = BootWiring
    { bwBindings :: [MountBinding]
    -- ^ The resolved mounts. A worker-only role builds them for their rules, and serves none.
    , bwPublishTargets :: [PublishTarget]
    -- ^ One target per mirrored mount, with the provider that mints its write token. A role that writes nothing plans none.
    }

-- | Resolve serving mounts and worker targets from the same credential providers.
resolveBootWiring :: WiringPorts -> MirrorMintPlan -> Limits -> Maybe PublishBudget -> ValidatedPlan -> IO (Either [BootError] BootWiring)
resolveBootWiring ports mintPlan limits publishBudget plan = do
    providersE <- mintedProviders
    case providersE of
        Left errs -> pure (Left errs)
        Right providers -> do
            bindingsE <- planMounts (wpResolveAdapter ports) (wpClock ports) (wpRuleDeps ports) mintPlan providers (wpDeadlines ports) limits publishBudget plan
            -- The serve side and the publish side read the same providers independently, so
            -- 'Validation' reports both rather than the mounts' refusals alone.
            pure . validationToEither $
                BootWiring
                    <$> eitherToValidation bindingsE
                    <*> eitherToValidation (planPublishTargets mintPlan providers plan)
  where
    -- A minting role mints once, eagerly, and before either group above consumes the providers,
    -- so a misconfigured identity fails at boot rather than on the first write.
    mintedProviders :: IO (Either [BootError] CredentialProviders)
    mintedProviders = case mintPlan of
        SkipMirrorWrite -> pure (Right noCredentialProviders)
        MintMirrorWrite -> initCredentialProviders (wpBuildCredentials ports) (wpReporters ports) (map vmMount (vpMounts plan))

-- | The publish byte discipline, aggregate admission and per-request cap, present exactly when a mount publishes.
data PublishBudget = PublishBudget
    { pbBodyBudget :: ByteAdmission
    , pbMaxRequestBytes :: Int
    }

-- | Resolve mounts with their configured authority and publication budget.
planMounts :: ResolveAdapter -> IO UTCTime -> (Ecosystem -> RuleDeps) -> MirrorMintPlan -> CredentialProviders -> Deadlines -> Limits -> Maybe PublishBudget -> ValidatedPlan -> IO (Either [BootError] [MountBinding])
planMounts resolveAdapter clock ruleDepsFor mintPlan providers deadlines limits publishBudget plan = do
    bindingResults <- traverse bindingFor (vpMounts plan)
    pure $ case partitionEithers bindingResults of
        ([], bindings) -> Right bindings
        (errs, _) -> Left (concat errs)
  where
    app :: AppConfig
    app = vpSettings plan

    ctx :: WiringContext
    ctx =
        WiringContext
            { wcApp = app
            , wcDeadlines = deadlines
            , wcLimits = limits
            , wcClock = clock
            , wcRuleDeps = ruleDepsFor
            , wcPublishBudget = publishBudget
            , -- Derived from the environment layer like the inbound token, and resolved once, so
              -- every mount's denials carry the same message.
              wcHelp = mkHelpMessage <$> srvHelpMessage (cfgServer app)
            }

    {- The plan cleared the adapter, so only the credential reference and the injected resolver
    are still this mount's to check, and it reports both in one run. -}
    bindingFor :: VettedMount -> IO (Either [BootError] MountBinding)
    bindingFor vetted = do
        deps <- packumentDepsFor ctx (vmAdapter vetted) (vmMount vetted) (vmConfig vetted)
        pure $ case (credentialError mintPlan providers (vmMount vetted), resolveAdapter eco (primaryView vetted deps) (privateView vetted deps) (mountPublishDeps ctx plan vetted)) of
            (Nothing, Just binding) -> Right binding
            (mCredErr, mBinding) ->
                Left (maybeToList mCredErr <> [MissingAdapter eco | isNothing mBinding])
      where
        eco = vmEcosystem vetted

{- The deployment-wide inputs every mount's deps are built from. Each is resolved once for the
whole pass, so two mounts cannot be wired against different values of one of them. -}
data WiringContext = WiringContext
    { wcApp :: AppConfig
    , wcDeadlines :: Deadlines
    , wcLimits :: Limits
    , wcClock :: IO UTCTime
    , wcRuleDeps :: Ecosystem -> RuleDeps
    , wcPublishBudget :: Maybe PublishBudget
    , wcHelp :: Maybe HelpMessage
    }

primaryView :: VettedMount -> PackumentDeps -> PackumentDeps
primaryView vetted deps
    | vmEcosystem vetted == Cargo =
        deps
            { pdReadSourceSelection = ManagedMirrorAndPublic
            , pdUpstreams =
                mountUpstreams
                    (artifactHosts (pdArtifact deps))
                    (mtUrl <$> regMirrorTarget regs)
                    (regPublicUpstream regs)
                    (maybe NoMirrorWrite (MirrorOnAdmit . mtUrl) (regMirrorTarget regs))
            }
    | otherwise = deps
  where
    regs = mountRegistries (vmMount vetted)

privateView :: VettedMount -> PackumentDeps -> Maybe PackumentDeps
privateView vetted deps
    | vmEcosystem vetted == Cargo =
        Just
            deps
                { pdReadSourceSelection = PrivateOnly
                , pdMountBaseUrl = pdMountBaseUrl deps <> "/private"
                , pdMirrorState = Nothing
                , pdUpstreams =
                    mountUpstreams
                        (artifactHosts (pdArtifact deps))
                        (regPrivateUpstream regs)
                        (regPublicUpstream regs)
                        NoMirrorWrite
                }
    | otherwise = Nothing
  where
    regs = mountRegistries (vmMount vetted)

-- A mount the pass cleared no publication for leaves @PUT \/{pkg}@ answering @405@.
mountPublishDeps :: WiringContext -> ValidatedPlan -> VettedMount -> Maybe PublishDeps
mountPublishDeps ctx plan vetted =
    Map.lookup (vmEcosystem vetted) (vpPublications plan)
        >>= publishDepsFor (vmAdapter vetted) (wcApp ctx) (wcLimits ctx) (wcPublishBudget ctx) (wcHelp ctx)

-- The ecosystem-shaped fields are the adapter's own records, carried whole, and the rest is the
-- mount's configuration. @pdMountBaseUrl@ owns the @dist.tarball@ base.
packumentDepsFor :: WiringContext -> RegistryAdapter -> Mount -> MountConfig -> IO PackumentDeps
packumentDepsFor ctx adapter mount mcfg = do
    -- 'prepare' allocates an effectful rule's resilience policy and breaker once per mount.
    -- 'pdAdvisoryEtag' reads the current ETag through those same 'RuleDeps', so no generation is pinned at boot.
    let ruleDeps = wcRuleDeps ctx (mountEcosystem mount)
    prepared <- prepare ruleDeps (mountPolicy mount)
    installEvidence <- traverse (\build -> build (wcDeadlines ctx) (wcLimits ctx)) (adapterInstallEvidence adapter)
    let regs = mountRegistries mount
        app = wcApp ctx
    pure
        PackumentDeps
            { pdUpstreams =
                mountUpstreams
                    (artifactHosts (adapterArtifact adapter))
                    (regPrivateUpstream regs)
                    (regPublicUpstream regs)
                    (maybe NoMirrorWrite (MirrorOnAdmit . mtUrl) (regMirrorTarget regs))
            , -- Deny by default: a mount that declares no namespaces owns no first-party name.
              pdFirstParty = maybe (const False) firstPartyName (mntFirstParty mcfg)
            , pdMountBaseUrl = mountBaseUrl (srvPublicUrl (cfgServer app)) (mountEcosystem mount)
            , pdInstallEvidence = installEvidence
            , pdRules = prepared
            , -- Operator ranges extending the fixed internal-range block on the @dist.tarball@ host
              -- gate. One list for every mount: internal ranges are a deployment-wide fact.
              pdAdditionalBlockedRanges = egrAdditionalBlockedRanges (cfgEgress app)
            , pdLimits = wcLimits ctx
            , pdInboundToken = srvAuthToken (cfgServer app)
            , pdNow = wcClock ctx
            , pdAdvisoryEtag = rdCurrentAdvisoryEtag ruleDeps
            , pdNoteAdmission = noteAdmission (rdSourceReporter ruleDeps)
            , pdHelp = wcHelp ctx
            , pdMinIntegrity = intMinPublic (cfgIntegrity app)
            , -- Refined per mount, so a legacy registry's loosening below the global default
              -- never leaks onto a neighbouring mount.
              pdMinTrustedIntegrity = fromMaybe (intMinTrusted (cfgIntegrity app)) (miMinTrusted (mntIntegrity mcfg))
            , pdMetadata = adapterMetadata adapter
            , pdArtifact = adapterArtifact adapter
            , pdEgressUrl = mkRegistryUrl
            , pdReadSourceSelection = MergePrivateAndPublic
            , pdMirrorState = Nothing
            , pdMetadataCompression = srvMetadataGzip (cfgServer app)
            }

-- A role that mints nothing references nothing, and a mount with no mirror target never writes,
-- so neither can fail here.
credentialError :: MirrorMintPlan -> CredentialProviders -> Mount -> Maybe BootError
credentialError mintPlan providers mount = case (mintPlan, regMirrorTarget (mountRegistries mount)) of
    (SkipMirrorWrite, _) -> Nothing
    (MintMirrorWrite, Nothing) -> Nothing
    (MintMirrorWrite, Just _) ->
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

-- | A cleared publication's dependencies. 'Nothing' without the publish budget the memory plan allocates.
publishDepsFor :: RegistryAdapter -> AppConfig -> Limits -> Maybe PublishBudget -> Maybe HelpMessage -> VettedPublication -> Maybe PublishDeps
publishDepsFor adapter app limits publishBudget helpMessage publication = do
    budget <- publishBudget
    -- An ecosystem this build writes nothing for carries no publish capability, so the mount's
    -- publish route stays opt-out and answers its documented refusal.
    publish <- adapterPublish adapter
    pure
        PublishDeps
            { pubReadUrl = publicationReadUrl (vpubTarget publication)
            , pubTargetUrl = publicationTargetUrl (vpubTarget publication)
            , pubAllowed = firstPartyName (vpubFirstParty publication)
            , pubStaticCredential = vpubStaticCredential publication
            , pubInboundToken = srvAuthToken (cfgServer app)
            , pubMaxRequestBytes = fromMaybe (pbMaxRequestBytes budget) (vpubMaxRequestBytes publication)
            , pubLimits = limits{maxPublishRequestBytes = fromMaybe (pbMaxRequestBytes budget) (vpubMaxRequestBytes publication)}
            , pubBodyBudget = pbBodyBudget budget
            , pubHelp = helpMessage
            , pubProjectName = adapterProjectName adapter
            , pubAdapter = publish
            }

-- | Whether this deployment owns a name: the one predicate every consumer of the privilege reads.
firstPartyName :: FirstParty -> PackageName -> Bool
firstPartyName = \case
    FirstPartyNpmScopes scopes -> npmPublishAllowed (toList scopes)
    FirstPartyPyPI entries -> pypiFirstPartyName entries
    FirstPartyCargo entries -> cargoFirstPartyName entries
    FirstPartyRubyGems entries -> rubyGemsFirstPartyName entries

-- | One ecosystem's publish target, resolved once: the worker's write endpoint and its token provider.
data PublishTarget = PublishTarget
    { ptEcosystem :: Ecosystem
    -- ^ The ecosystem this publish target serves.
    , ptMirrorUrl :: RegistryUrl
    -- ^ The mirror-target endpoint the worker publishes approved artifacts to.
    , ptUploadUrl :: RegistryUrl
    -- ^ The exact native upload endpoint, independently configured from the read root.
    , ptBackendTag :: StoreTag
    -- ^ The declared backend selects native service credential presentation.
    , ptUsername :: Maybe Text
    -- ^ The backend credential convention retained beside the refreshing secret provider.
    , ptCredentials :: CredentialProvider
    -- ^ The provider minting the mirror-target write token.
    }

-- | Each cleared mount's publish target, or the aggregated boot errors. A role that mints no write credential plans none.
planPublishTargets ::
    MirrorMintPlan ->
    CredentialProviders ->
    ValidatedPlan ->
    Either [BootError] [PublishTarget]
planPublishTargets mintPlan providers plan = case mintPlan of
    SkipMirrorWrite -> Right []
    MintMirrorWrite ->
        case partitionEithers (mapMaybe (publishTargetFor providers . vmMount) (vpMounts plan)) of
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
                    , ptMirrorUrl = mtUrl target
                    , ptUploadUrl = mtUploadUrl target
                    , ptBackendTag = sbTag (mtBackend target)
                    , ptUsername = mtUsername target
                    , ptCredentials = provider
                    }
        Nothing ->
            Left [UnresolvedCredential (mountEcosystem mount)]
