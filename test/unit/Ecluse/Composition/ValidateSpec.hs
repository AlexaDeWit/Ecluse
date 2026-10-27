-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Check native policies and endpoint capabilities for each boot role.
module Ecluse.Composition.ValidateSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (
    Advisory (RemediationSkipsWithoutStore),
    BootError (
        AdvisoryDenyWithoutStore,
        DredgerChunkPauseBeneathFloor,
        FirstPartyMissing,
        FirstPartyWithoutPrivateUpstream,
        MinProgressBytesNotPositive,
        MirrorStateRequired,
        MirrorTargetOnMountEndpoint,
        MirrorTargetWithoutPublish,
        ProgressWindowNotBelowServeCap,
        ProgressWindowNotPositive,
        PublicationTargetOnMountEndpoint,
        PublicationTargetOnPublicUpstream,
        PublicationTargetWithoutPublish,
        PublishStaticCredentialNeedsEdge,
        RuleBlockedOnMount,
        StoreMaintenanceUnavailable,
        StoreTagConflict,
        UploadConfigurationRequired
    ),
    StoreMaintenanceReason (NoControlPlane, NoProtocolMaintenance, PrivateCacheUnavailable),
 )
import Ecluse.Composition.Endpoints (publicationTargetUrl)
import Ecluse.Composition.Support (
    cargoCodeArtifactEnv,
    cargoDeprecationRefusal,
    cargoGateEnv,
    cargoMirrorEnv,
    cargoScopeRefusal,
    clearedUrl,
    codeArtifactDomain,
    codeArtifactEnvVars,
    codeArtifactMirrorUrl,
    expectConfig,
    mountDocFor,
    noMaintenanceBackend,
    overrideEnv,
    publicGateEnv,
    pypiMirrorEnv,
    rubyInstallRefusal,
    rubyScopeRefusal,
    staticEnvVars,
    withDredgeablePrivate,
    withObservablePrivate,
    withRubyInstallBlock,
    withoutPrivateUpstreamUrl,
 )
import Ecluse.Composition.Types (BootRole (..), MirrorRole (..), RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter), registryRoleOf)
import Ecluse.Composition.Validate (
    ValidatedPlan (vpMirrorStores, vpMounts, vpPrivateCaches, vpProgressFloor, vpPublications, vpSettings),
    VettedMount (vmEcosystem, vmMount),
    VettedPublication (vpubFirstParty, vpubStaticCredential, vpubTarget),
    vetBoot,
    vetBootWith,
    vetMirrorCapabilities,
 )
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    AppConfig (cfgMirrorState, cfgMounts),
    Config (configApp),
    FirstParty (FirstPartyNpmScopes),
    MirrorEndpoint (meUploadUrl, meUsername),
    MountConfig (mntMirrorTarget, mntPublicationTarget),
    PublicationEndpoint (peUsername),
    StoreTag (TagRegistry),
    mountPolicy,
 )
import Ecluse.Core.Credential (ClientCredential (credSecret), unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterPublish), adapterFor)
import Ecluse.Core.Registry.Sweep.Types (minimumChunkPause)
import Ecluse.Core.Rules.Types (PrecededRule (prRule), RuleType (AllowScopeType, DenyInstallTimeExecutionType), ruleName)
import Ecluse.Core.Security (mkProgressFloor, serveCapSeconds)
import Ecluse.Core.Security.Egress (registryUrlText)

-- | Check accumulated refusals and the plans cleared for each registry role.
spec :: Spec
spec = do
    mirrorCapabilitiesSpec
    publicationCapabilitiesSpec
    cargoPublicationCapabilitiesSpec
    ruleBlocksSpec
    cargoRuleBlocksSpec
    cargoMaintenanceCapabilitiesSpec
    clearedSpec
    refusalSpec
    advisoryStoreSpec
    firstPartyAuthoritySpec
    privatePublicationSpec

mirrorCapabilitiesSpec :: Spec
mirrorCapabilitiesSpec = describe "role-specific mirror capabilities" $ do
    it "requires durable state on a producer without requiring a writer endpoint" $ do
        config <- expectConfig pypiMirrorEnv Nothing
        let app = withoutWriteSettings PyPI (configApp config)
        capabilityResult (BootMirrorPipeline ServeOnly) app `shouldBe` Right ()
        capabilityResult (BootMirrorPipeline ServeOnly) app{cfgMirrorState = Nothing} `shouldBe` Left [MirrorStateRequired PyPI]
    it "requires native write settings in embedded and standalone workers" $ do
        config <- expectConfig pypiMirrorEnv Nothing
        let app = (withoutWriteSettings PyPI (configApp config)){cfgMirrorState = Nothing}
        for_ [ServeAndMirror, MirrorOnly] $ \role ->
            capabilityResult (BootMirrorPipeline role) app
                `shouldBe` Left
                    [ MirrorStateRequired PyPI
                    , UploadConfigurationRequired PyPI "mirrorTarget.registry.uploadUrl"
                    , UploadConfigurationRequired PyPI "mirrorTarget.registry.username"
                    ]
    it "does not add mirror-state or writer dependencies to Dredger and Pilot" $ do
        config <- expectConfig pypiMirrorEnv Nothing
        let app = (withoutWriteSettings PyPI (configApp config)){cfgMirrorState = Nothing}
        for_ [BootStorePruner, BootStorePreview, BootWithoutPipeline] $ \role -> capabilityResult role app `shouldBe` Right ()
    it "requires durable Cargo state in every mirror pipeline role" $ do
        config <- expectConfig cargoMirrorEnv Nothing
        let app = (configApp config){cfgMirrorState = Nothing}
        for_ [ServeOnly, ServeAndMirror, MirrorOnly] $ \role ->
            capabilityResult (BootMirrorPipeline role) app `shouldBe` Left [MirrorStateRequired Cargo]
    it "requires only the native Cargo upload setting in both writer roles" $ do
        config <- expectConfig cargoMirrorEnv Nothing
        let app = withoutWriteSettings Cargo (configApp config)
            upload = UploadConfigurationRequired Cargo "mirrorTarget.registry.uploadUrl"
        for_ [ServeAndMirror, MirrorOnly] $ \role -> do
            capabilityResult (BootMirrorPipeline role) app `shouldBe` Left [upload]
            capabilityResult (BootMirrorPipeline role) app{cfgMirrorState = Nothing} `shouldBe` Left [MirrorStateRequired Cargo, upload]
        capabilityResult (BootMirrorPipeline ServeOnly) app `shouldBe` Right ()
    it "clears token-only Cargo mirror settings without a username" $ do
        config <- expectConfig cargoMirrorEnv Nothing
        (Map.lookup Cargo (cfgMounts (configApp config)) >>= mntMirrorTarget >>= meUsername) `shouldBe` Nothing
        for_ [ServeOnly, ServeAndMirror, MirrorOnly] $ \role ->
            capabilityResult (BootMirrorPipeline role) (configApp config) `shouldBe` Right ()
    it "requires no native state for an unmirrored Cargo producer" $ do
        config <- expectConfig cargoGateEnv Nothing
        capabilityResult (BootMirrorPipeline ServeOnly) (configApp config) `shouldBe` Right ()
    it "adds no Cargo mirror state or upload setting to maintenance and Pilot" $ do
        config <- expectConfig cargoMirrorEnv Nothing
        let app = (withoutWriteSettings Cargo (configApp config)){cfgMirrorState = Nothing}
        for_ [BootStorePruner, BootStorePreview, BootWithoutPipeline] $ \role ->
            capabilityResult role app `shouldBe` Right ()
  where
    withoutWriteSettings eco app = app{cfgMounts = Map.adjust stripWriter eco (cfgMounts app)}
    stripWriter mount = mount{mntMirrorTarget = fmap (\endpoint -> endpoint{meUploadUrl = Nothing, meUsername = Nothing}) (mntMirrorTarget mount)}
    capabilityResult role = snd . runVet (registryRoleOf role) . vetMirrorCapabilities role

publicationCapabilitiesSpec :: Spec
publicationCapabilitiesSpec = describe "PyPI publication credentials by boot role" $ do
    for_ [BootMirrorPipeline ServeAndMirror, BootMirrorPipeline ServeOnly] $ \role ->
        describe (show role) $ do
            it "refuses a static token without an explicit username" $ do
                config <- expectConfig tokenEnv Nothing
                publicationResult role config `shouldBe` Left [UploadConfigurationRequired PyPI "publicationTarget.registry.username"]
            it "accepts a static token with an explicit username" $ do
                config <- expectConfig (overrideEnv (publicationKey <> "USERNAME") "publisher" tokenEnv) Nothing
                publicationResult role config `shouldBe` Right [PyPI]
            it "accepts anonymous publication without a token or username" $ do
                config <- expectConfig publicationEnv Nothing
                publicationResult role config `shouldBe` Right [PyPI]
    for_ [BootMirrorPipeline MirrorOnly, BootStorePruner, BootStorePreview, BootWithoutPipeline] $ \role ->
        it ("does not require a publication username under " <> show role) $ do
            config <- expectConfig tokenEnv Nothing
            publicationResult role config `shouldBe` Right [PyPI]
  where
    publicationKey = "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__"
    publicationEnv =
        [ ("ECLUSE_SERVER__AUTH_TOKEN", "edge-token")
        , ("ECLUSE_MOUNTS__PYPI__ENABLED", "true")
        , ("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", "acme-*")
        , ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/pypi/")
        , (publicationKey <> "URL", "https://publish.example.test/legacy/")
        ]
            <> publicGateEnv
    tokenEnv = overrideEnv (publicationKey <> "TOKEN") "publish-write-token" publicationEnv

publicationResult :: BootRole -> Config -> Either [BootError] [Ecosystem]
publicationResult role config =
    fmap (Map.keys . vpPublications) . snd . runVet (registryRoleOf role) $
        vetBoot config <* vetMirrorCapabilities role (configApp config)

cargoPublicationCapabilitiesSpec :: Spec
cargoPublicationCapabilitiesSpec = describe "Cargo publication API by boot role" $ do
    for_ [BootMirrorPipeline ServeAndMirror, BootMirrorPipeline ServeOnly] $ \role -> do
        it ("requires the native Cargo API root under " <> show role) $ do
            config <- expectConfig publicationEnv Nothing
            publicationResult role config `shouldBe` Left [UploadConfigurationRequired Cargo "publicationTarget.registry.uploadUrl"]
        it ("clears a Cargo token without a username under " <> show role) $ do
            config <- expectConfig (overrideEnv (publicationKey <> "TOKEN") "publisher-token" completeEnv) Nothing
            (Map.lookup Cargo (cfgMounts (configApp config)) >>= mntPublicationTarget >>= peUsername) `shouldBe` Nothing
            publicationResult role config `shouldBe` Right [Cargo]
        it ("clears anonymous Cargo publication with an explicit API under " <> show role) $ do
            config <- expectConfig completeEnv Nothing
            publicationResult role config `shouldBe` Right [Cargo]
    for_ [BootMirrorPipeline MirrorOnly, BootStorePruner, BootStorePreview, BootWithoutPipeline] $ \role ->
        it ("adds no unused Cargo publication API requirement under " <> show role) $ do
            config <- expectConfig (overrideEnv (publicationKey <> "TOKEN") "publisher-token" publicationEnv) Nothing
            publicationResult role config `shouldBe` Right [Cargo]
    it "keeps a Cargo publication endpoint colocated with private reads valid for writers" $ do
        config <- expectConfig (overrideEnv (publicationKey <> "URL") "https://private.example.test/" (overrideEnv (publicationKey <> "UPLOAD_URL") "https://private.example.test/api/" completeEnv)) Nothing
        publicationResult (BootMirrorPipeline ServeAndMirror) config `shouldBe` Right [Cargo]
  where
    publicationKey = "ECLUSE_MOUNTS__CARGO__PUBLICATION_TARGET__REGISTRY__"
    publicationEnv =
        cargoGateEnv
            <> [ ("ECLUSE_SERVER__AUTH_TOKEN", "edge-token")
               , ("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", "acme-*")
               , ("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/")
               , (publicationKey <> "URL", "https://publish.example.test/")
               ]
    completeEnv = overrideEnv (publicationKey <> "UPLOAD_URL") "https://publish.example.test/api/" publicationEnv

cargoRuleBlocksSpec :: Spec
cargoRuleBlocksSpec = describe "resolved Cargo rule blocks" $
    for_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        it "clears the shipped policy for the Cargo mount alone" $ do
            plan <- expectVetted role cargoGateEnv
            map vmEcosystem (vpMounts plan) `shouldBe` [Cargo]
            map (ruleName . prRule) (concatMap (mountPolicy . vmMount) (vpMounts plan)) `shouldBe` ["AllowIfOlderThan"]
        for_ [(scopeRule, cargoScopeRefusal), (deprecatedRule, cargoDeprecationRefusal)] $ \(rules, refusal) ->
            it ("refuses the shared " <> show refusal) $
                refusalsFor role (overrideEnv "ECLUSE_RULES" rules cargoGateEnv) `shouldReturn` [refusal]
        it "refuses aliased Cargo blocks below a higher allow with an admission-only deny" $ do
            errors <- refusalsFor role (overrideEnv "ECLUSE_MOUNTS__CARGO__RULES" "{\"alias-scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\",\"precedence\":1},\"alias-deprecated\":{\"type\":\"DenyIfDeprecated\",\"precedence\":1,\"appliesTo\":[\"admission\"]},\"pin\":{\"type\":\"AllowByIdentity\",\"identity\":\"fixture@1.0.0\",\"precedence\":999}}" cargoGateEnv)
            errors `shouldMatchList` [cargoScopeRefusal, cargoDeprecationRefusal]
        it "accumulates duplicate Cargo blocks and an independent advisory refusal" $ do
            let shared = "{\"scope-a\":{\"type\":\"AllowScope\",\"scope\":\"acme\"},\"scope-b\":{\"type\":\"AllowScope\",\"scope\":\"acme\"},\"deprecated\":{\"type\":\"DenyIfDeprecated\"}}"
                cve = "{\"cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8}}"
            errors <- refusalsFor role (overrideEnv "ECLUSE_MOUNTS__CARGO__RULES" cve (overrideEnv "ECLUSE_RULES" shared cargoGateEnv))
            errors `shouldMatchList` [cargoScopeRefusal, cargoScopeRefusal, cargoDeprecationRefusal, AdvisoryDenyWithoutStore Cargo ("DenyIfCve" :| [])]
        it "reports independent Ruby and Cargo scope blocks" $ do
            errors <- refusalsFor role (overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" (overrideEnv "ECLUSE_RULES" scopeRule cargoGateEnv))
            errors `shouldMatchList` [rubyScopeRefusal, cargoScopeRefusal]
        it "removes both blocked types through matching Cargo rule keys" $ do
            let env = overrideEnv "ECLUSE_MOUNTS__CARGO__RULES" "{\"scope\":{\"enabled\":false},\"deprecated\":{\"enabled\":false}}" (overrideEnv "ECLUSE_RULES" bothRules cargoGateEnv)
            plan <- expectVetted role env
            map (ruleName . prRule) (concatMap (mountPolicy . vmMount) (vpMounts plan)) `shouldBe` ["AllowIfOlderThan"]
        it "keeps npm mount policy out of the Cargo policy" $ do
            plan <- expectVetted role (overrideEnv "ECLUSE_MOUNTS__NPM__RULES" bothRules (overrideEnv "ECLUSE_MOUNTS__NPM__ENABLED" "true" cargoGateEnv))
            map vmEcosystem (vpMounts plan) `shouldBe` [Npm, Cargo]
            let cargoPolicies = [mountPolicy (vmMount mount) | mount <- vpMounts plan, vmEcosystem mount == Cargo]
            map (map (ruleName . prRule)) cargoPolicies `shouldBe` [["AllowIfOlderThan"]]
        it "ignores blocked declarations on a disabled Cargo mount" $ do
            let env = overrideEnv "ECLUSE_MOUNTS__NPM__ENABLED" "true" (overrideEnv "ECLUSE_MOUNTS__CARGO__ENABLED" "false" (overrideEnv "ECLUSE_RULES" bothRules cargoGateEnv))
            plan <- expectVetted role env
            map vmEcosystem (vpMounts plan) `shouldBe` [Npm]
  where
    scopeRule = "{\"scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}"
    deprecatedRule = "{\"deprecated\":{\"type\":\"DenyIfDeprecated\"}}"
    bothRules = "{\"scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"},\"deprecated\":{\"type\":\"DenyIfDeprecated\"}}"

cargoMaintenanceCapabilitiesSpec :: Spec
cargoMaintenanceCapabilitiesSpec = describe "Cargo maintenance capabilities" $ do
    for_ [MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        it "refuses generic Cargo inventory for both mirror and private cache" $ do
            errors <- refusalsFor role cargoMirrorEnv
            errors `shouldMatchList` [StoreMaintenanceUnavailable Cargo (NoControlPlane TagRegistry), StoreMaintenanceUnavailable Cargo (PrivateCacheUnavailable "registry has no inventory control plane")]
        it "refuses protocol maintenance without native physical deletion" $ do
            let document = mountDocFor "cargo" ["\"privateUpstream\":{\"codeArtifact\":{\"url\":\"" <> codeArtifactDomain <> "/cargo/internal/\"}}", "\"mirrorTarget\":{\"verdaccio\":{\"url\":\"https://target.example.test/\",\"token\":\"t\",\"permitDeletion\":true}}"]
            config <- expectConfig cargoGateEnv (Just document)
            leftToMaybe (vetted role config) `shouldBe` Just [StoreMaintenanceUnavailable Cargo NoProtocolMaintenance]
        it "refuses an incompatible Cargo private cache while retaining a supported mirror" $ do
            let document = mountDocFor "cargo" ["\"privateUpstream\":{\"verdaccio\":{\"url\":\"https://private.example.test/\",\"token\":\"t\",\"permitDeletion\":true}}", "\"mirrorTarget\":{\"codeArtifact\":{\"url\":\"" <> codeArtifactDomain <> "/cargo/mirror/\"}}"]
            config <- expectConfig cargoGateEnv (Just document)
            leftToMaybe (vetted role config) `shouldBe` Just [StoreMaintenanceUnavailable Cargo NoProtocolMaintenance]
        it "clears native CodeArtifact mirror and private-cache maintenance" $ do
            plan <- expectVetted role cargoCodeArtifactEnv
            Map.keys (vpMirrorStores plan) `shouldBe` [Cargo]
            Map.keys (vpPrivateCaches plan) `shouldBe` [Cargo]
    it "retains generic Cargo registry use in every pipeline role" $ do
        config <- expectConfig cargoMirrorEnv Nothing
        for_ [ServeOnly, ServeAndMirror, MirrorOnly] $ \role ->
            publicationResult (BootMirrorPipeline role) config `shouldBe` Right []

ruleBlocksSpec :: Spec
ruleBlocksSpec = describe "vetBoot -- ecosystem rule blocks" $ do
    for_ [([Npm], publicGateEnv), ([PyPI], overrideEnv "ECLUSE_MOUNTS__NPM__ENABLED" "false" bothMounts), ([Npm, PyPI], bothMounts)] $ \(ecosystems, env) ->
        it ("clears shipped defaults for " <> show ecosystems) $ do
            plan <- expectVetted MirrorWriter env
            map vmEcosystem (vpMounts plan) `shouldBe` ecosystems
            for_ (vpMounts plan) $ \mount ->
                map (ruleName . prRule) (mountPolicy (vmMount mount))
                    `shouldMatchList` (["AllowIfOlderThan"] <> ["DenyInstallTimeExecution" | vmEcosystem mount == Npm])

    for_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        it "refuses a shared install-code deny" $
            refusalsFor role (overrideEnv "ECLUSE_RULES" installRule bothMounts)
                `shouldReturn` [installBlock]
        it "refuses a shared scope allow" $
            refusalsFor role (overrideEnv "ECLUSE_RULES" scopeRule bothMounts)
                `shouldReturn` [scopeBlock]
        it "refuses an aliased mount rule even below a higher allow and at admission only" $
            refusalsFor
                role
                ( overrideEnv
                    "ECLUSE_MOUNTS__PYPI__RULES"
                    "{\"another-name\":{\"type\":\"DenyInstallTimeExecution\",\"precedence\":1,\"appliesTo\":[\"admission\"]},\"pin\":{\"type\":\"AllowByIdentity\",\"identity\":\"flask\",\"precedence\":999}}"
                    bothMounts
                )
                `shouldReturn` [installBlock]
        it "accumulates every entry beside an independent advisory-store refusal" $ do
            let shared = "{\"code-a\":{\"type\":\"DenyInstallTimeExecution\"},\"code-b\":{\"type\":\"DenyInstallTimeExecution\"},\"scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}"
                npmDeny = "{\"cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8}}"
            errors <- refusalsFor role (overrideEnv "ECLUSE_MOUNTS__NPM__RULES" npmDeny (overrideEnv "ECLUSE_RULES" shared bothMounts))
            errors `shouldMatchList` [AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| []), installBlock, installBlock, scopeBlock]
        it "reports independent Ruby and PyPI installation blocks" $ do
            errors <- refusalsFor role (overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" (overrideEnv "ECLUSE_RULES" installRule bothMounts))
            errors `shouldMatchList` [rubyInstallRefusal, installBlock]
        it "clears the same-key suppression in the PyPI patch" $
            refusalsFor
                role
                ( overrideEnv
                    "ECLUSE_MOUNTS__PYPI__RULES"
                    "{\"custom-code\":{\"enabled\":false},\"custom-scope\":{\"enabled\":false}}"
                    (overrideEnv "ECLUSE_RULES" "{\"custom-code\":{\"type\":\"DenyInstallTimeExecution\"},\"custom-scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}" bothMounts)
                )
                `shouldReturn` []
        it "clears both types when they live under npm alone" $
            refusalsFor
                role
                ( overrideEnv
                    "ECLUSE_MOUNTS__NPM__RULES"
                    "{\"custom-code\":{\"type\":\"DenyInstallTimeExecution\"},\"custom-scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}"
                    bothMounts
                )
                `shouldReturn` []
        it "ignores a disabled PyPI mount's blocked declaration" $
            refusalsFor role (overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "false" (overrideEnv "ECLUSE_RULES" installRule bothMounts))
                `shouldReturn` []
        it "preserves suppression of the shipped npm default" $
            refusalsFor role (overrideEnv "ECLUSE_MOUNTS__NPM__RULES" "{\"deny-scripts\":{\"enabled\":false}}" bothMounts)
                `shouldReturn` []
  where
    bothMounts = overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" publicGateEnv
    installRule = "{\"custom-code\":{\"type\":\"DenyInstallTimeExecution\"}}"
    scopeRule = "{\"custom-scope\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}"
    installBlock = RuleBlockedOnMount PyPI DenyInstallTimeExecutionType "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release."
    scopeBlock = RuleBlockedOnMount PyPI AllowScopeType "A PyPI project name has no scope, so this rule can never match one."

advisoryStoreSpec :: Spec
advisoryStoreSpec = describe "vetBoot -- an advisory deny with no advisory store" $ do
    forM_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role ->
        it ("refuses under " <> show role) $
            refusalsFor role (denyingOn "DenyIfCve" (withObservablePrivate codeArtifactEnvVars))
                >>= (`shouldSatisfy` elem (AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| [])))

    for_ ["deny", "skip"] $ \alignment ->
        it ("refuses a manual MAL rule without a source under " <> alignment) $
            refusalsFor MirrorWriter (denyingWith "DenyIfKnownMalicious" alignment staticEnvVars)
                `shouldReturn` [AdvisoryDenyWithoutStore Npm ("DenyIfKnownMalicious" :| [])]

    it "refuses a forced named MAL default without a source" $
        refusalsFor MirrorWriter (overrideEnv "ECLUSE_RULES" "{\"deny-known-malicious\":{\"enabled\":true}}" staticEnvVars)
            `shouldReturn` [AdvisoryDenyWithoutStore Npm ("DenyIfKnownMalicious" :| [])]

    it "names the EPSS deny on its own terms" $
        refusalsFor MirrorWriter (denyingOn "DenyIfEpss" staticEnvVars)
            `shouldReturn` [AdvisoryDenyWithoutStore Npm ("DenyIfEpss" :| [])]

    it "refuses a deny that fails open, because skip still cannot decide without a database" $
        refusalsFor MirrorWriter (denyingWith "DenyIfCve" "skip" staticEnvVars)
            `shouldReturn` [AdvisoryDenyWithoutStore Npm ("DenyIfCve" :| [])]

    it "clears the source-free shipped policy, whose advisory rules no source activates" $
        refusalsFor MirrorWriter staticEnvVars `shouldReturn` []

    it "clears an advisory deny once a store is configured" $
        refusalsFor MirrorWriter (withAdvisoryStore (denyingOn "DenyIfCve" staticEnvVars)) `shouldReturn` []

    it "clears the shipped advisory rules a store activates" $
        refusalsFor MirrorWriter (withAdvisoryStore staticEnvVars) `shouldReturn` []

    forM_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role -> do
        let roleEnv = withObservablePrivate codeArtifactEnvVars
        it ("refuses the shipped remediation rule activated without a source under " <> show role) $
            refusalsFor role (overrideEnv "ECLUSE_RULES" "{\"remediation-fast-track\":{\"enabled\":true}}" roleEnv)
                >>= (`shouldSatisfy` elem (AdvisoryDenyWithoutStore Npm ("AllowIfRemediatesCve" :| [])))
        it ("refuses a named remediation rule that denies without a source under " <> show role) $
            refusalsFor role (denyingOn "AllowIfRemediatesCve" roleEnv)
                >>= (`shouldSatisfy` elem (AdvisoryDenyWithoutStore Npm ("AllowIfRemediatesCve" :| [])))
        it ("advises on, and raises no store refusal for, a remediation rule that skips without a source under " <> show role) $ do
            config <- expectConfig (denyingWith "AllowIfRemediatesCve" "skip" roleEnv) Nothing
            let (advisories, outcome) = runVet role (vetBoot config)
            advisories `shouldSatisfy` elem (RemediationSkipsWithoutStore Npm)
            fromLeft [] outcome `shouldSatisfy` not . any isStoreRefusal

    it "boots a remediation rule that skips without a source" $ do
        config <- expectConfig (denyingWith "AllowIfRemediatesCve" "skip" staticEnvVars) Nothing
        void (snd (runVet MirrorWriter (vetBoot config))) `shouldBe` Right ()

    it "advises nothing for a skipping remediation rule once a store is configured" $ do
        config <- expectConfig (withAdvisoryStore (denyingWith "AllowIfRemediatesCve" "skip" staticEnvVars)) Nothing
        fst (runVet MirrorWriter (vetBoot config)) `shouldSatisfy` notElem (RemediationSkipsWithoutStore Npm)

privatePublicationSpec :: Spec
privatePublicationSpec = describe "vetBoot private upstream and publication target collision" $ do
    let collision = PublicationTargetOnMountEndpoint Npm Npm "privateUpstream" "https://private.example.test"

    forM_ [MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        it "returns no plan despite a usable maintenance backend" $ do
            config <- expectConfig (withObservablePrivate (publishingAtTag "VERDACCIO" "https://private.example.test" codeArtifactEnvVars)) Nothing
            let (advisories, outcome) = runVet role (vetBoot config)
            advisories `shouldBe` []
            fmap (Map.keys . vpMirrorStores) outcome `shouldBe` Left [collision]

        it "accumulates the collision and the unavailable maintenance backend" $
            refusalsFor role (withDredgeablePrivate (publishingAtTag "VERDACCIO" "https://private.example.test" staticEnvVars))
                `shouldReturn` [collision, noMaintenanceBackend]

        it "clears a usable store when publication is separate" $ do
            plan <- expectVetted role (withObservablePrivate (publishingAtTag "VERDACCIO" "https://publish.example.test" codeArtifactEnvVars))
            fmap clearedUrl (Map.lookup Npm (vpMirrorStores plan)) `shouldBe` Just codeArtifactMirrorUrl

    it "clears the same-mount publication for proxy and mirror writers" $ do
        plan <- expectVetted MirrorWriter (withObservablePrivate (publishingAtTag "VERDACCIO" "https://private.example.test" codeArtifactEnvVars))
        fmap (registryUrlText . publicationTargetUrl . vpubTarget) (Map.lookup Npm (vpPublications plan))
            `shouldBe` Just "https://private.example.test"

    it "retains the separate refusal for conflicting tags at the same publication URL" $ do
        config <- expectConfig (withObservablePrivate (publishingAt "https://private.example.test" codeArtifactEnvVars)) Nothing
        let conflict = StoreTagConflict Npm "privateUpstream.verdaccio" Npm "publicationTarget.registry" "https://private.example.test"
        void (snd (runVet MirrorPreviewer (vetBoot config))) `shouldBe` Left [collision, conflict]
        void (snd (runVet MirrorWriter (vetBoot config))) `shouldBe` Left [conflict]

firstPartyAuthoritySpec :: Spec
firstPartyAuthoritySpec = describe "first-party authority" $
    forM_ [(Npm, "NPM", "@acme"), (PyPI, "PYPI", "acme-tools,acme-*")] $ \(eco, envName, namespaces) ->
        forM_ [MirrorWriter, MirrorPruner] $ \role ->
            describe (show eco <> " " <> show role) $ do
                let key suffix = "ECLUSE_MOUNTS__" <> envName <> "__" <> suffix
                    publicEnv =
                        [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
                        , (key "ENABLED", "true")
                        ]
                    ownedEnv = overrideEnv (key "FIRST_PARTY") namespaces publicEnv
                it "refuses first-party names without a private upstream" $
                    refusalsFor role ownedEnv `shouldReturn` [FirstPartyWithoutPrivateUpstream eco]
                it "clears first-party names with a private upstream" $
                    refusalsFor role (overrideEnv (key "PRIVATE_UPSTREAM__REGISTRY__URL") "https://private.example.test" ownedEnv)
                        `shouldReturn` []
                it "clears a public-only mount without first-party names" $
                    refusalsFor role publicEnv `shouldReturn` []
                it "ignores a disabled mount's first-party declaration" $ do
                    plan <- expectVetted role (overrideEnv (key "ENABLED") "false" ownedEnv)
                    map vmEcosystem (vpMounts plan) `shouldBe` []

clearedSpec :: Spec
clearedSpec = describe "vetBoot -- what a cleared configuration reifies" $ do
    it "clears the active mounts and leaves the raw settings on the plan beside them" $ do
        plan <- expectVetted MirrorWriter staticEnvVars
        map vmEcosystem (vpMounts plan) `shouldBe` [Npm]
        Map.keys (cfgMounts (vpSettings plan)) `shouldBe` [Npm]

    it "clears a declared publication target with its namespaces and its static credential" $ do
        plan <- expectVetted MirrorWriter (overrideEnv "ECLUSE_SERVER__AUTH_TOKEN" "edge-token" staticPublishEnv)
        case Map.lookup Npm (vpPublications plan) of
            Nothing -> expectationFailure "expected the publishing mount to clear a publication"
            Just publication -> do
                registryUrlText (publicationTargetUrl (vpubTarget publication))
                    `shouldBe` "https://publish.example.test"
                vpubFirstParty publication `shouldBe` FirstPartyNpmScopes (pure (mkScope "acme"))
                fmap (unSecret . credSecret) (vpubStaticCredential publication) `shouldBe` Just "publish-write-token"

    it "clears no publication for a mount that declares no target, so PUT stays a 405" $ do
        plan <- expectVetted MirrorWriter staticEnvVars
        Map.keys (vpPublications plan) `shouldBe` []

    it "clears the deleting role the backend for a mirror store no other endpoint holds" $ do
        plan <- expectVetted MirrorPruner codeArtifactEnvVars
        fmap clearedUrl (Map.lookup Npm (vpMirrorStores plan)) `shouldBe` Just codeArtifactMirrorUrl

    it "clears a backend for every mount that declares a mirror target" $ do
        plan <- expectVetted MirrorPruner codeArtifactEnvVars
        Map.keys (vpMirrorStores plan) `shouldBe` mirroringMounts (vpSettings plan)

    it "clears a writing role no store at all, because no writing role sweeps one" $ do
        plan <- expectVetted MirrorWriter codeArtifactEnvVars
        Map.keys (vpMirrorStores plan) `shouldBe` []

    it "clears the progress floor the configured window and byte count make under the serve-path cap" $ do
        plan <- expectVetted MirrorWriter (withProgressFloor "5" "4096" staticEnvVars)
        Right (vpProgressFloor plan) `shouldBe` mkProgressFloor (fromIntegral serveCapSeconds) 5 4096

    it "clears a writing role a mirror target this build cannot sweep, and says nothing of it" $ do
        config <- expectConfig staticEnvVars Nothing
        let (advisories, outcome) = runVet MirrorWriter (vetBoot config)
        advisories `shouldBe` []
        fmap (Map.keys . vpMirrorStores) outcome `shouldBe` Right []

refusalSpec :: Spec
refusalSpec = describe "vetBoot -- the refusals its groups earn" $ do
    it "refuses an active Ruby installation rule the adapter cannot support" $
        refusalsFor MirrorWriter (withRubyInstallBlock staticEnvVars)
            `shouldReturn` [rubyInstallRefusal]

    forM_
        [ ("mirror", MirrorTargetWithoutPublish PyPI, pypiMirrorEnv)
        ,
            ( "publication"
            , PublicationTargetWithoutPublish PyPI
            , overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__FIRST_PARTY" "acme-*" $
                    overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" "https://private.example.test/pypi/" $
                        overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL" "https://publish.example.test/pypi/" staticEnvVars
            )
        ]
        $ \(target, refusal, env) -> describe (target <> " publisher capability") $ do
            it "refuses the target when its adapter has no publisher" $ do
                config <- expectConfig env Nothing
                let withoutPublisher PyPI = (\adapter -> adapter{adapterPublish = Nothing}) <$> adapterFor PyPI
                    withoutPublisher eco = adapterFor eco
                void (snd (runVet MirrorWriter (vetBootWith withoutPublisher config))) `shouldBe` Left [refusal]
            it "clears the target with the real PyPI publisher" $
                refusalsFor MirrorWriter env `shouldReturn` []

    it "refuses a publication target that leaves the anti-shadowing guard nothing to enforce" $
        refusalsFor MirrorWriter (withoutFirstParty publishingEnv)
            `shouldReturn` [FirstPartyMissing Npm]

    it "refuses a static publish credential with no verifiable inbound edge" $
        refusalsFor MirrorWriter staticPublishEnv
            `shouldReturn` [PublishStaticCredentialNeedsEdge Npm TagRegistry]

    it "refuses the deleting role a mirror target this build has no maintenance backend for" $
        refusalsFor MirrorPruner (withDredgeablePrivate staticEnvVars) `shouldReturn` [noMaintenanceBackend]

    it "refuses the deleting role a collision on a target it has a backend for" $
        refusalsFor MirrorPruner (overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" codeArtifactMirrorUrl (withoutPrivateUpstreamUrl codeArtifactEnvVars))
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm Npm "privateUpstream" codeArtifactMirrorUrl]

    it "refuses the deleting role a chunk pause beneath the sweep's floor" $
        refusalsFor MirrorPruner (beneathThePauseFloor codeArtifactEnvVars)
            `shouldReturn` [DredgerChunkPauseBeneathFloor 1 minimumChunkPause]

    forM_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role -> describe ("the progress floor under " <> show role) $ do
        it "refuses a window that is not below the serve-path cap" $
            forM_ [serveCapSeconds, serveCapSeconds + 1] $ \window ->
                refusalsFor role (withProgressFloor (show window) "1048576" codeArtifactEnvVars)
                    `shouldReturn` [ProgressWindowNotBelowServeCap window serveCapSeconds]

        it "reports a zero window and a zero byte count together" $
            refusalsFor role (withProgressFloor "0" "0" codeArtifactEnvVars)
                `shouldReturn` [ProgressWindowNotPositive 0, MinProgressBytesNotPositive 0]

        it "refuses a negative window and a negative byte count" $
            refusalsFor role (withProgressFloor "-5" "-1" codeArtifactEnvVars)
                `shouldReturn` [ProgressWindowNotPositive (-5), MinProgressBytesNotPositive (-1)]

    it "clears a writing role that same pause, because no writing role sweeps" $
        refusalsFor MirrorWriter (beneathThePauseFloor codeArtifactEnvVars) `shouldReturn` []

    it "reports the mount refusal beside the maintenance refusal from one deleting-role run" $
        refusalsFor MirrorPruner (withRubyInstallBlock (withDredgeablePrivate staticEnvVars))
            `shouldReturn` [noMaintenanceBackend, rubyInstallRefusal]

    it "reports a refusal from each of the writing groups in one run" $ do
        let envVars =
                withRubyInstallBlock $
                    withoutFirstParty (overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL" "https://public.example.test/npm/" staticEnvVars)
        refusalsFor MirrorWriter envVars
            `shouldReturn` [ FirstPartyMissing Npm
                           , PublicationTargetOnPublicUpstream Npm Npm "https://public.example.test/npm/"
                           , rubyInstallRefusal
                           ]

publishingEnv :: [(String, String)]
publishingEnv = publishingAt "https://publish.example.test" staticEnvVars

publishingAt :: String -> [(String, String)] -> [(String, String)]
publishingAt = publishingAtTag "REGISTRY"

publishingAtTag :: String -> String -> [(String, String)] -> [(String, String)]
publishingAtTag tag url env =
    overrideEnv "ECLUSE_MOUNTS__NPM__FIRST_PARTY" "@acme" $
        overrideEnv ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__" <> tag <> "__URL") url env

staticPublishEnv :: [(String, String)]
staticPublishEnv = overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__TOKEN" "publish-write-token" publishingEnv

withProgressFloor :: String -> String -> [(String, String)] -> [(String, String)]
withProgressFloor window minBytes =
    overrideEnv "ECLUSE_LIMITS__MIN_PROGRESS_BYTES" minBytes . overrideEnv "ECLUSE_LIMITS__PROGRESS_WINDOW" window

beneathThePauseFloor :: [(String, String)] -> [(String, String)]
beneathThePauseFloor = overrideEnv "ECLUSE_DREDGER__CHUNK_PAUSE" "1"

withoutFirstParty :: [(String, String)] -> [(String, String)]
withoutFirstParty = filter ((/= "ECLUSE_MOUNTS__NPM__FIRST_PARTY") . fst)

mirroringMounts :: AppConfig -> [Ecosystem]
mirroringMounts app =
    [eco | (eco, mcfg) <- Map.toAscList (cfgMounts app), isJust (mntMirrorTarget mcfg)]

expectVetted :: RegistryRole -> [(String, String)] -> IO ValidatedPlan
expectVetted role envVars = do
    config <- expectConfig envVars Nothing
    either (\errs -> fail ("boot vetting refused: " <> show errs)) pure (vetted role config)

refusalsFor :: RegistryRole -> [(String, String)] -> IO [BootError]
refusalsFor role envVars = fromLeft [] . vetted role <$> expectConfig envVars Nothing

withAdvisoryStore :: [(String, String)] -> [(String, String)]
withAdvisoryStore = overrideEnv "ECLUSE_ADVISORIES__URL" "s3://advisories"

denyingOn :: String -> [(String, String)] -> [(String, String)]
denyingOn ruleType = denyingWith ruleType "deny"

denyingWith :: String -> String -> [(String, String)] -> [(String, String)]
denyingWith ruleType alignment = overrideEnv "ECLUSE_RULES" rules
  where
    rules = "{\"gate\":{\"type\":\"" <> ruleType <> "\",\"onUnavailable\":\"" <> alignment <> "\"" <> threshold <> "}}"
    threshold = case ruleType of
        "DenyIfKnownMalicious" -> ""
        "DenyIfEpss" -> ",\"minEpss\":0.5"
        "AllowIfRemediatesCve" -> ",\"remainingAdvisories\":\"differential\""
        _ -> ",\"minCvss\":8"

isStoreRefusal :: BootError -> Bool
isStoreRefusal = \case
    AdvisoryDenyWithoutStore{} -> True
    _ -> False

vetted :: RegistryRole -> Config -> Either [BootError] ValidatedPlan
vetted role config = snd (runVet role (vetBoot config))
