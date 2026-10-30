-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pure boot pass accumulates refusals and advisories for each role.
The composition root builds only from 'ValidatedPlan'. Unvetted settings remain on 'vpSettings'.
-}
module Ecluse.Composition.Validate (
    -- * The validate phase
    ValidatedPlan (vpMounts, vpPublications, vpMirrorStores, vpPrivateCaches, vpProgressFloor, vpSettings),
    vetBoot,
    vetBootWith,
    vetMirrorCapabilities,

    -- * What it clears
    VettedMount (vmEcosystem, vmAdapter, vmMount, vmConfig),
    VettedPublication (vpubTarget, vpubFirstParty, vpubStaticCredential, vpubMaxRequestBytes),
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    Advisory (DredgerQuotaOverrideUnmatched, RemediationSkipsWithoutStore),
    BootError (
        AdvisoryDenyWithoutStore,
        DredgerChunkPauseBeneathFloor,
        DredgerQuotaScopeConflict,
        FirstPartyMissing,
        FirstPartyWithoutPrivateUpstream,
        MinProgressBytesNotPositive,
        MirrorStateRequired,
        MirrorTargetWithoutPublish,
        MissingAdapter,
        ProgressWindowNotBelowServeCap,
        ProgressWindowNotPositive,
        PublicationTargetWithoutPublish,
        PublishStaticCredentialNeedsEdge,
        RuleBlockedOnMount,
        UploadConfigurationRequired,
        UploadUrlRequired
    ),
 )
import Ecluse.Composition.Endpoints (
    PublicationTarget,
    VettedEndpoints (vePublicationTargets),
    vetEndpoints,
 )
import Ecluse.Composition.Maintenance.Plan (ClearedBackend, ResolveMaintenanceAdapter, overrideKey, vetPrivateCaches, vetStoreBackends)
import Ecluse.Composition.Types (BootRole, MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly), pipelineRoleOf)
import Ecluse.Composition.Vet (Severity (Advise, Ignore, Refuse), Vet, byStoreRole, decided, rule)
import Ecluse.Config (
    AppConfig (cfgDredger, cfgLimits, cfgMirrorState, cfgMounts, cfgServer),
    Config (configApp, configMounts),
    DredgerSettings (drgChunkPause, drgQuotaOverrides),
    FirstParty,
    LimitsSettings (limMinProgressBytes, limProgressWindow),
    MirrorEndpoint (meUploadUrl, meUsername),
    MirrorTarget (mtUrl),
    Mount,
    MountConfig (mntFirstParty, mntMaxRequestBytes, mntMirrorTarget, mntPrivateUpstream, mntPublicationTarget),
    PrivateEndpoint (preTarget),
    PublicationEndpoint (peTarget, peToken, peUploadUrl, peUsername),
    QuotaOverride (qoQuotas, qoScope, qoWeights),
    ServerSettings (srvAuthToken),
    StoreBackend,
    StoreTag (TagCodeArtifact),
    Target (tgtTag, tgtUrl),
    advisorySourced,
    meTarget,
    mountAdvisoryDenials,
    mountPolicy,
    mountRegistries,
    regMirrorTarget,
    storeTagName,
 )
import Ecluse.Core.Credential (ClientCredential (ClientCredential), Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor, adapterPublish, adapterRuleBlocks)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterPublish (publishIdentity, publishMirrorRequirements, publishUploadEndpoint),
    MirrorRequirements (mirrorStateRequirement),
    MirrorStateRequirement (DurableVersionState),
    UploadEndpointRequirement (ExplicitUploadEndpoint),
    UploadIdentityRequirement (NamedIdentity),
 )
import Ecluse.Core.Registry.Sweep.Types (minimumChunkPause)
import Ecluse.Core.Rules.Types (
    AllowIfRemediatesCveParams (aircOnUnavailable),
    FailureAlignment (FailNoDecision),
    PrecededRule (prRule),
    Rule (AllowIfRemediatesCve),
    ruleType,
 )
import Ecluse.Core.Security (
    ProgressFloor,
    ProgressFloorError (MinBytesNotPositive, WindowNotBelowServeCap, WindowNotPositive),
    mkProgressFloor,
    serveCapSeconds,
 )
import Ecluse.Core.Security.Egress (registryUrlText)

-- | What the pure boot pass cleared: the mounts a role may serve, the endpoints it may use, and the settings no rule vets.
data ValidatedPlan = ValidatedPlan
    { vpMounts :: [VettedMount]
    -- ^ Every active mount, in ascending ecosystem order, with the adapter that serves it.
    , vpPublications :: Map Ecosystem VettedPublication
    -- ^ Each mount's cleared publish path, absent where the mount declares no target.
    , vpMirrorStores :: Map Ecosystem ClearedBackend
    -- ^ The backend for each store a sweep may delete from. Only @ecluse dredger@'s pass clears one.
    , vpPrivateCaches :: Map Ecosystem (Maybe StoreBackend, ClearedBackend)
    -- ^ Private caches cleared for this role with their own credential plans.
    , vpProgressFloor :: ProgressFloor
    -- ^ The upstream progress floor the configured window and byte count make under the serve-path cap.
    , vpSettings :: AppConfig
    -- ^ The settings no rule vets. Its mounts are the raw declarations, and 'vpMounts' holds the vetted ones.
    }

-- | One active mount, paired with the adapter this build ships for its ecosystem.
data VettedMount = VettedMount
    { vmEcosystem :: Ecosystem
    , vmAdapter :: RegistryAdapter
    , vmMount :: Mount
    , vmConfig :: MountConfig
    }

-- | A mount's cleared publish path: the endpoint, the first-party namespaces, and the static credential the edge gate covers.
data VettedPublication = VettedPublication
    { vpubTarget :: PublicationTarget
    , vpubFirstParty :: FirstParty
    , vpubStaticCredential :: Maybe ClientCredential
    , vpubMaxRequestBytes :: Maybe Int
    }

-- | Require only capabilities this process uses. Store maintenance never opens mirror state or uploads packages.
vetMirrorCapabilities :: BootRole -> AppConfig -> Vet ()
vetMirrorCapabilities role app = decided $ case missing of
    [] -> Right ()
    errors -> Left errors
  where
    pipeline = pipelineRoleOf role
    mirrorWrites = pipeline `elem` [Just ServeAndMirror, Just MirrorOnly]
    clientWrites = pipeline `elem` [Just ServeAndMirror, Just ServeOnly]
    missing = concatMap missingOnMount (Map.toList (cfgMounts app))
    missingOnMount (eco, mount) = case adapterFor eco >>= adapterPublish of
        Nothing -> []
        Just publish ->
            maybe [] (mirrorNeeds eco publish) (mntMirrorTarget mount)
                <> maybe [] (publicationNeeds eco publish) (mntPublicationTarget mount)
    mirrorNeeds eco publish endpoint =
        [MirrorStateRequired eco | mirrorStateRequirement (publishMirrorRequirements publish) == DurableVersionState, isJust pipeline, isNothing (cfgMirrorState app)]
            <> [uploadUrlRequired eco "mirrorTarget" (meTarget endpoint) | separateUpload publish, mirrorWrites, isNothing (meUploadUrl endpoint)]
            <> [usernameRequired eco "mirrorTarget" (meTarget endpoint) | publishIdentity publish == NamedIdentity, mirrorWrites, tgtTag (meTarget endpoint) /= TagCodeArtifact, isNothing (meUsername endpoint)]
    publicationNeeds eco publish endpoint =
        [uploadUrlRequired eco "publicationTarget" (peTarget endpoint) | separateUpload publish, clientWrites, isNothing (peUploadUrl endpoint)]
            <> [usernameRequired eco "publicationTarget" (peTarget endpoint) | publishIdentity publish == NamedIdentity, clientWrites, isJust (peToken endpoint), tgtTag (peTarget endpoint) /= TagCodeArtifact, isNothing (peUsername endpoint)]
    separateUpload publish = publishUploadEndpoint publish == ExplicitUploadEndpoint
    keyPath endpoint target key = endpoint <> "." <> storeTagName (tgtTag target) <> "." <> key
    uploadUrlRequired eco endpoint target = UploadUrlRequired eco (keyPath endpoint target "uploadUrl")
    usernameRequired eco endpoint target = UploadConfigurationRequired eco (keyPath endpoint target "username")

-- | Accumulate every pure refusal and advisory for one role before constructing its plan.
vetBoot :: Config -> Vet ValidatedPlan
vetBoot = vetBootWith adapterFor

-- | Vet every adapter-dependent capability through one lookup, including absent capabilities.
vetBootWith :: ResolveMaintenanceAdapter -> Config -> Vet ValidatedPlan
vetBootWith resolveAdapter config =
    assemble
        <$> vetMounts resolveAdapter config
        <*> vetPublishPolicy app
        <*> vetEndpoints (cfgMounts app)
        <*> vetStoreBackends resolveAdapter (configMounts config)
        <*> vetPrivateCaches resolveAdapter (cfgMounts app) (configMounts config)
        <*> vetProgressFloor app
        <* vetSweepPacing app
        <* vetAdvisoryStore config
        <* vetRemediationStore config
        <* vetRuleBlocks resolveAdapter config
        <* vetQuotaOverrides config
        <* vetQuotaScopes app
  where
    app = configApp config

    assemble mounts policies endpoints backends caches progress =
        ValidatedPlan
            { vpMounts = mounts
            , vpPublications = Map.intersectionWith cleared (vePublicationTargets endpoints) policies
            , vpMirrorStores = backends
            , vpPrivateCaches = caches
            , vpProgressFloor = progress
            , vpSettings = app
            }

    cleared target (firstParty, credential, requestBytes) = VettedPublication target firstParty credential requestBytes

-- Every role dials upstreams, so every role refuses a floor the window and byte count cannot make.
vetProgressFloor :: AppConfig -> Vet ProgressFloor
vetProgressFloor app =
    decided (first (map refusal . toList) (mkProgressFloor (fromIntegral serveCapSeconds) (fromIntegral window) minBytes))
  where
    window = limProgressWindow (cfgLimits app)
    minBytes = limMinProgressBytes (cfgLimits app)
    refusal = \case
        WindowNotPositive -> ProgressWindowNotPositive window
        WindowNotBelowServeCap -> ProgressWindowNotBelowServeCap window serveCapSeconds
        MinBytesNotPositive -> MinProgressBytesNotPositive minBytes

{- The floor under the sweep's own pace. Deletion is permanent, so both store roles refuse a
pause that would sweep faster than an operator can stop it, and every other role reads none. -}
vetSweepPacing :: AppConfig -> Vet ()
vetSweepPacing app = rule severity beneathFloor (drgChunkPause (cfgDredger app))
  where
    severity = byStoreRole (Refuse (`DredgerChunkPauseBeneathFloor` minimumChunkPause)) Ignore

    beneathFloor configured = configured <$ guard (configured < minimumChunkPause)

-- A rule that refuses without advisory evidence can never decide without a store, so every role
-- refuses the pairing rather than serve a mount that refuses everything.
vetAdvisoryStore :: Config -> Vet ()
vetAdvisoryStore config =
    traverse_ (rule (const (Refuse (uncurry AdvisoryDenyWithoutStore))) denyingWithoutStore) mounts
  where
    stored = advisorySourced (configApp config)
    mounts = Map.toAscList (configMounts config)

    denyingWithoutStore (eco, mount)
        | stored = Nothing
        | otherwise = (,) eco <$> nonEmpty (mountAdvisoryDenials mount)

-- A skipping remediation rule boots without a store, but admits no fix and logs every admission's skip.
vetRemediationStore :: Config -> Vet ()
vetRemediationStore config = traverse_ (rule (const (Advise RemediationSkipsWithoutStore)) skippingWithoutStore) mounts
  where
    stored = advisorySourced (configApp config)
    mounts = Map.toAscList (configMounts config)

    skippingWithoutStore (eco, mount) = eco <$ guard (not stored && any (skips . prRule) (mountPolicy mount))
    skips = \case
        AllowIfRemediatesCve params -> aircOnUnavailable params == FailNoDecision
        _ -> False

-- A blocked declaration refuses every role, even when another rule outranks it.
vetRuleBlocks :: ResolveMaintenanceAdapter -> Config -> Vet ()
vetRuleBlocks resolveAdapter config = traverse_ vetMountRules (activeMounts config)
  where
    vetMountRules (eco, (mount, _)) =
        traverse_ (rule (const (Refuse (uncurry (RuleBlockedOnMount eco)))) blocked) (mountPolicy mount)
      where
        blocked preceded = do
            adapter <- resolveAdapter eco
            let ty = ruleType (prRule preceded)
            reason <- Map.lookup ty (adapterRuleBlocks adapter)
            pure (ty, reason)

{- A declared capacity that matches no store paces nothing. It advises rather than refuses,
because an endpoint renamed under a running Dredger would otherwise stop the role outright. -}
vetQuotaOverrides :: Config -> Vet ()
vetQuotaOverrides config = traverse_ (rule severity unmatched) declaredKeys
  where
    severity = byStoreRole (Advise DredgerQuotaOverrideUnmatched) Ignore
    declaredKeys = Map.keys (drgQuotaOverrides (cfgDredger (configApp config)))
    unmatched key = key <$ guard (overrideKey key `notElem` storeKeys)
    storeKeys = map overrideKey (declaredStoreUrls config)

{- One capacity pool takes one definition. Two entries that name the same scope and describe it
differently refuse, because whichever the boot read last would silently set the other's rate. -}
vetQuotaScopes :: AppConfig -> Vet ()
vetQuotaScopes app = traverse_ (rule severity conflicting) (pairsBy declaredScope entries)
  where
    severity = byStoreRole (Refuse (\(scope, first', second') -> DredgerQuotaScopeConflict scope first' second')) Ignore
    entries = Map.toAscList (drgQuotaOverrides (cfgDredger app))
    declaredScope (_, override) = qoScope override
    conflicting (scope, (leftKey, left), (rightKey, right))
        | describes left == describes right = Nothing
        | otherwise = Just (scope, leftKey, rightKey)
    describes override = (qoQuotas override, qoWeights override)

-- Every pair of entries sharing one declared key, so a rule reads two definitions at once.
pairsBy :: (Ord key) => (entry -> Maybe key) -> [entry] -> [(key, entry, entry)]
pairsBy keyOf entries =
    [ (key, left, right)
    | (key, grouped) <- Map.toAscList (Map.fromListWith (<>) [(key, [entry]) | entry <- entries, Just key <- [keyOf entry]])
    , (left, right) <- adjacent (reverse grouped)
    ]
  where
    adjacent values = zip values (drop 1 values)

-- Every store URL a mount declares as a sweep target: its mirror target and its private cache.
declaredStoreUrls :: Config -> [Text]
declaredStoreUrls config =
    [registryUrlText (mtUrl target) | mount <- Map.elems (configMounts config), Just target <- [regMirrorTarget (mountRegistries mount)]]
        <> [registryUrlText (tgtUrl (preTarget endpoint)) | mcfg <- Map.elems (cfgMounts (configApp config)), Just endpoint <- [mntPrivateUpstream mcfg]]

vetMounts :: ResolveMaintenanceAdapter -> Config -> Vet [VettedMount]
vetMounts resolveAdapter config = catMaybes <$> traverse (vetMount resolveAdapter) (activeMounts config)

{- 'Ecluse.Config.loadConfig' derives 'configMounts' from 'cfgMounts' entry for entry, so the two
maps share a keyset and this pairing is total. -}
activeMounts :: Config -> [(Ecosystem, (Mount, MountConfig))]
activeMounts config =
    Map.toAscList (Map.intersectionWith (,) (configMounts config) (cfgMounts (configApp config)))

-- 'Nothing' only where the rule refused, and a refused pass yields no plan to carry it into.
vetMount :: ResolveMaintenanceAdapter -> (Ecosystem, (Mount, MountConfig)) -> Vet (Maybe VettedMount)
vetMount resolveAdapter (eco, (mount, mcfg)) =
    vetted
        <$ rule (const (Refuse MissingAdapter)) unservedEcosystem eco
        <* rule (const (Refuse MirrorTargetWithoutPublish)) (declaredWithoutPublish mirrors) eco
        <* rule (const (Refuse PublicationTargetWithoutPublish)) (declaredWithoutPublish publishes) eco
        <* rule (const (Refuse FirstPartyWithoutPrivateUpstream)) firstPartyWithoutPrivateUpstream (eco, mcfg)
  where
    vetted = resolveAdapter eco <&> \adapter -> VettedMount eco adapter mount mcfg

    unservedEcosystem e
        | isNothing (resolveAdapter e) = Just e
        | otherwise = Nothing

    mirrors = isJust (regMirrorTarget (mountRegistries mount))
    publishes = isJust (mntPublicationTarget mcfg)

    declaredWithoutPublish declared e = do
        guard declared
        adapter <- resolveAdapter e
        guard (isNothing (adapterPublish adapter))
        pure e

firstPartyWithoutPrivateUpstream :: (Ecosystem, MountConfig) -> Maybe Ecosystem
firstPartyWithoutPrivateUpstream (eco, mcfg) =
    eco <$ guard (isJust (mntFirstParty mcfg) && isNothing (mntPrivateUpstream mcfg))

{- The two couplings a declared publication target carries: the first-party namespaces the
guard enforces, and the inbound edge a static publish credential needs. -}
vetPublishPolicy :: AppConfig -> Vet (Map Ecosystem (FirstParty, Maybe ClientCredential, Maybe Int))
vetPublishPolicy app =
    Map.fromList . catMaybes <$> traverse (vetPublication (srvAuthToken (cfgServer app))) publishingMounts
  where
    publishingMounts =
        [ (eco, mcfg)
        | (eco, mcfg) <- Map.toAscList (cfgMounts app)
        , isJust (mntPublicationTarget mcfg)
        ]

vetPublication :: Maybe Secret -> (Ecosystem, MountConfig) -> Vet (Maybe (Ecosystem, (FirstParty, Maybe ClientCredential, Maybe Int)))
vetPublication inboundToken subject@(eco, mcfg) =
    cleared
        <$ rule (const (Refuse FirstPartyMissing)) firstPartyMissing subject
        <* rule (const (Refuse (uncurry PublishStaticCredentialNeedsEdge))) (staticWithoutEdge inboundToken) subject
  where
    cleared = mntFirstParty mcfg <&> \firstParty -> (eco, (firstParty, publicationCredential mcfg, mntMaxRequestBytes mcfg))

publicationCredential :: MountConfig -> Maybe ClientCredential
publicationCredential mount = do
    endpoint <- mntPublicationTarget mount
    token <- peToken endpoint
    let username = if tgtTag (peTarget endpoint) == TagCodeArtifact then Just "aws" else peUsername endpoint
    pure (ClientCredential username token)

publicationToken :: MountConfig -> Maybe Secret
publicationToken mcfg = peToken =<< mntPublicationTarget mcfg

firstPartyMissing :: (Ecosystem, MountConfig) -> Maybe Ecosystem
firstPartyMissing (eco, mcfg)
    | isNothing (mntFirstParty mcfg) = Just eco
    | otherwise = Nothing

{- Any unauthenticated client could otherwise publish within scope under Écluse's own credential.
The tag rides along, because the credential's key path nests below it. -}
staticWithoutEdge :: Maybe Secret -> (Ecosystem, MountConfig) -> Maybe (Ecosystem, StoreTag)
staticWithoutEdge inboundToken (eco, mcfg) = do
    guard (isJust (publicationToken mcfg) && isNothing inboundToken)
    endpoint <- mntPublicationTarget mcfg
    pure (eco, tgtTag (peTarget endpoint))
