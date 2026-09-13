-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.ValidateSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (
    BootError (
        DredgerChunkPauseBeneathFloor,
        FirstPartyMissing,
        FirstPartyWithoutPrivateUpstream,
        MirrorTargetOnMountEndpoint,
        MirrorTargetWithoutPublish,
        MissingAdapter,
        PublicationTargetOnMountEndpoint,
        PublicationTargetOnPublicUpstream,
        PublicationTargetWithoutPublish,
        PublishStaticCredentialNeedsEdge
    ),
 )
import Ecluse.Composition.Endpoints (publicationTargetUrl)
import Ecluse.Composition.Support (
    clearedRepository,
    codeArtifactEnvVars,
    codeArtifactMirrorUrl,
    expectConfig,
    noMaintenanceBackend,
    overrideEnv,
    staticEnvVars,
    withObservablePrivate,
    withoutPrivateUpstreamUrl,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Validate (
    ValidatedPlan (vpMirrorStores, vpMounts, vpPublications, vpSettings),
    VettedMount (vmEcosystem),
    VettedPublication (vpubFirstParty, vpubStaticToken, vpubTarget),
    vetBoot,
 )
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    AppConfig (cfgMounts),
    Config,
    FirstParty (FirstPartyNpmScopes),
    MountConfig (mntMirrorTarget),
    StoreTag (TagRegistry),
 )
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Sweep.Types (minimumChunkPause)
import Ecluse.Core.Security.Egress (registryUrlText)

-- | Check accumulated refusals and the plans cleared for each registry role.
spec :: Spec
spec = do
    clearedSpec
    refusalSpec
    firstPartyAuthoritySpec
    privatePublicationSpec

privatePublicationSpec :: Spec
privatePublicationSpec = describe "vetBoot private upstream and publication target collision" $ do
    let collision = PublicationTargetOnMountEndpoint Npm Npm "privateUpstream" "https://private.example.test"

    forM_ [MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        it "returns no plan despite a usable maintenance backend" $ do
            config <- expectConfig (withObservablePrivate (publishingAt "https://private.example.test" codeArtifactEnvVars)) Nothing
            let (advisories, outcome) = runVet role (vetBoot config)
            advisories `shouldBe` []
            fmap (Map.keys . vpMirrorStores) outcome `shouldBe` Left [collision]

        it "accumulates the collision and the unavailable maintenance backend" $
            refusalsFor role (withObservablePrivate (publishingAt "https://private.example.test" staticEnvVars))
                `shouldReturn` [collision, noMaintenanceBackend]

        it "clears a usable store when publication is separate" $ do
            plan <- expectVetted role (withObservablePrivate (publishingAt "https://publish.example.test" codeArtifactEnvVars))
            fmap clearedRepository (Map.lookup Npm (vpMirrorStores plan)) `shouldBe` Just (Just "mirror")

    it "clears the same-mount publication for proxy and mirror writers" $ do
        plan <- expectVetted MirrorWriter (withObservablePrivate (publishingAt "https://private.example.test" codeArtifactEnvVars))
        fmap (registryUrlText . publicationTargetUrl . vpubTarget) (Map.lookup Npm (vpPublications plan))
            `shouldBe` Just "https://private.example.test"

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
                fmap unSecret (vpubStaticToken publication) `shouldBe` Just "publish-write-token"

    it "clears no publication for a mount that declares no target, so PUT stays a 405" $ do
        plan <- expectVetted MirrorWriter staticEnvVars
        Map.keys (vpPublications plan) `shouldBe` []

    it "clears the deleting role the backend for a mirror store no other endpoint holds" $ do
        plan <- expectVetted MirrorPruner codeArtifactEnvVars
        fmap repositoryOf (Map.lookup Npm (vpMirrorStores plan)) `shouldBe` Just "mirror"

    it "clears a backend for every mount that declares a mirror target" $ do
        plan <- expectVetted MirrorPruner codeArtifactEnvVars
        Map.keys (vpMirrorStores plan) `shouldBe` mirroringMounts (vpSettings plan)

    it "clears a writing role no store at all, because no writing role sweeps one" $ do
        plan <- expectVetted MirrorWriter codeArtifactEnvVars
        Map.keys (vpMirrorStores plan) `shouldBe` []

    it "clears a writing role a mirror target this build cannot sweep, and says nothing of it" $ do
        config <- expectConfig staticEnvVars Nothing
        let (advisories, outcome) = runVet MirrorWriter (vetBoot config)
        advisories `shouldBe` []
        fmap (Map.keys . vpMirrorStores) outcome `shouldBe` Right []
  where
    repositoryOf = fromMaybe "<not a CodeArtifact store>" . clearedRepository

refusalSpec :: Spec
refusalSpec = describe "vetBoot -- the refusals its groups earn" $ do
    it "refuses a mount whose ecosystem this build ships no adapter for" $
        refusalsFor MirrorWriter (overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" staticEnvVars)
            `shouldReturn` [MissingAdapter RubyGems]

    it "refuses a mirror target on a mount whose ecosystem this build writes nothing for" $
        refusalsFor
            MirrorWriter
            ( overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" "https://private.example.test/pypi/" $
                    overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL" "https://mirror.example.test/pypi/" $
                        overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__TOKEN" "t" staticEnvVars
            )
            `shouldReturn` [MirrorTargetWithoutPublish PyPI]

    it "refuses a publication target on a mount whose ecosystem this build writes nothing for" $
        refusalsFor
            MirrorWriter
            ( overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__FIRST_PARTY" "acme-*" $
                    overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" "https://private.example.test/pypi/" $
                        overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL" "https://publish.example.test/pypi/" staticEnvVars
            )
            `shouldReturn` [PublicationTargetWithoutPublish PyPI]

    it "refuses a publication target that leaves the anti-shadowing guard nothing to enforce" $
        refusalsFor MirrorWriter (withoutFirstParty publishingEnv)
            `shouldReturn` [FirstPartyMissing Npm]

    it "refuses a static publish credential with no verifiable inbound edge" $
        refusalsFor MirrorWriter staticPublishEnv
            `shouldReturn` [PublishStaticCredentialNeedsEdge Npm TagRegistry]

    it "refuses the deleting role a mirror target this build has no maintenance backend for" $
        refusalsFor MirrorPruner staticEnvVars `shouldReturn` [noMaintenanceBackend]

    it "refuses the deleting role a collision on a target it has a backend for" $
        refusalsFor MirrorPruner (overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" codeArtifactMirrorUrl (withoutPrivateUpstreamUrl codeArtifactEnvVars))
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm Npm "privateUpstream" codeArtifactMirrorUrl]

    it "refuses the deleting role a chunk pause beneath the sweep's floor" $
        refusalsFor MirrorPruner (beneathThePauseFloor codeArtifactEnvVars)
            `shouldReturn` [DredgerChunkPauseBeneathFloor 1 minimumChunkPause]

    it "clears a writing role that same pause, because no writing role sweeps" $
        refusalsFor MirrorWriter (beneathThePauseFloor codeArtifactEnvVars) `shouldReturn` []

    it "reports the mount refusal beside the maintenance refusal from one deleting-role run" $
        refusalsFor MirrorPruner (overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" staticEnvVars)
            `shouldReturn` [MissingAdapter RubyGems, noMaintenanceBackend]

    it "reports a refusal from each of the writing groups in one run" $ do
        let envVars =
                overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true" $
                    withoutFirstParty (overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL" "https://public.example.test/npm/" staticEnvVars)
        refusalsFor MirrorWriter envVars
            `shouldReturn` [ MissingAdapter RubyGems
                           , FirstPartyMissing Npm
                           , PublicationTargetOnPublicUpstream Npm Npm "https://public.example.test/npm/"
                           ]

publishingEnv :: [(String, String)]
publishingEnv = publishingAt "https://publish.example.test" staticEnvVars

publishingAt :: String -> [(String, String)] -> [(String, String)]
publishingAt url env =
    overrideEnv "ECLUSE_MOUNTS__NPM__FIRST_PARTY" "@acme" $
        overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL" url env

staticPublishEnv :: [(String, String)]
staticPublishEnv = overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__TOKEN" "publish-write-token" publishingEnv

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

vetted :: RegistryRole -> Config -> Either [BootError] ValidatedPlan
vetted role config = snd (runVet role (vetBoot config))
