-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Contracts for role-specific backend vetting without constructing store handles.
module Ecluse.Composition.Maintenance.PlanSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (Advisory, BootError (StoreMaintenanceUnavailable), StoreMaintenanceReason (DeletionNotPermitted, NoProtocolMaintenance, PrivateCacheUnavailable), renderBootError)
import Ecluse.Composition.Maintenance.Plan (ClearedBackend (cbAlphabet), vetPrivateCaches, vetStoreBackends)
import Ecluse.Composition.Maintenance.Support (mountsFor, noAdapter, privateCaches, refusalsOf, unaddressablePrivateUpstream, verdaccioEnv, vetted, withoutMaintenance, withoutPrivateAuthority)
import Ecluse.Composition.Support (clearedUrl, codeArtifactEnvVars, codeArtifactMirrorUrl, expectConfig, noMaintenanceBackend, privateInventoryRefusal, staticEnvVars, withObservablePrivate, withoutMirrorTargetToken, withoutMirrorTargetUrl, withoutPrivateUpstreamUrl)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp, configMounts), ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol), MountConfig (mntPrivateUpstream), StoreBackend, StoreTag (TagVerdaccio), sbControl)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance, adapterPublish), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceVersionDelete))
import Ecluse.Core.Registry.Maintenance.NameSpace (noNameAlphabet)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (casRepository)

spec :: Spec
spec = do
    boundarySpec
    passSpec
    protocolSpec
    previewCachesSpec

boundarySpec :: Spec
boundarySpec = describe "the cleared backend boundary" $ do
    it "issues a mirror store's backend only where the deleting role's pass cleared it" $ do
        withheld <- mountsFor (verdaccioEnv "false")
        void (snd (vetted MirrorPruner withheld))
            `shouldBe` Left [StoreMaintenanceUnavailable Npm (DeletionNotPermitted TagVerdaccio)]
        consenting <- mountsFor (verdaccioEnv "true")
        fmap Map.keys (snd (vetted MirrorPruner consenting)) `shouldBe` Right [Npm]

    it "issues a private cache's backend only where the cache's own pass cleared it" $ do
        refused <- expectConfig staticEnvVars Nothing
        void (privateCaches adapterFor MirrorPruner refused) `shouldBe` Left [privateInventoryRefusal]
        cleared <- expectConfig codeArtifactEnvVars Nothing
        fmap Map.keys (privateCaches adapterFor MirrorPruner cleared) `shouldBe` Right [Npm]

passSpec :: Spec
passSpec = describe "vetStoreBackends" $ do
    it "clears the deleting role the backend for each mirror target this build can sweep" $ do
        mounts <- mountsFor codeArtifactEnvVars
        let (advisories, outcome) = vetted MirrorPruner mounts
        advisories `shouldBe` []
        fmap (map clearedUrl . Map.elems) outcome `shouldBe` Right [codeArtifactMirrorUrl]
        fmap Map.keys outcome `shouldBe` Right [Npm]

    it "refuses the deleting role a mirror target this build cannot sweep, naming the key" $ do
        mounts <- mountsFor staticEnvVars
        case vetted MirrorPruner mounts of
            ([], Left [err]) -> do
                err `shouldBe` noMaintenanceBackend
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"
            other -> expectationFailure ("expected the one maintenance refusal, got: " <> show (refusalsOf other))

    it "clears a writing role no backend, and neither refuses nor advises on a target it cannot sweep" $ do
        -- Only the Dredger deletes, so only its pass reads the rule. The checker still names
        -- the Dredger's refusal for this configuration, so an operator learns of it once.
        mounts <- mountsFor staticEnvVars
        clearsNothing (vetted MirrorWriter mounts) `shouldBe` True

    it "clears nothing and refuses nothing for a mount that declares no mirror target" $ do
        mounts <- mountsFor (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars))
        clearsNothing (vetted MirrorPruner mounts) `shouldBe` True

    it "clears a vendor store with no alphabet when this build ships the ecosystem no adapter" $ do
        -- Unreachable through a real boot, which refuses that mount as MissingAdapter first. The
        -- store still clears, walked as one bucket, rather than the pass inventing an alphabet.
        mounts <- mountsFor codeArtifactEnvVars
        fmap (map cbAlphabet . Map.elems) (snd (runVet MirrorPruner (vetStoreBackends noAdapter mounts)))
            `shouldBe` Right [noNameAlphabet]

protocolSpec :: Spec
protocolSpec = describe "vetStoreBackends -- a store swept through the ecosystem protocol" $ do
    for_
        [ ("an adapter", noAdapter)
        , ("version deletion", fmap (\adapter -> adapter{adapterMaintenance = (adapterMaintenance adapter){maintenanceVersionDelete = Nothing}}) . adapterFor)
        , ("publication", fmap (\adapter -> adapter{adapterPublish = Nothing}) . adapterFor)
        ]
        $ \(missing, resolveAdapter) ->
            it ("refuses a consenting protocol store without " <> missing) $ do
                mounts <- mountsFor (verdaccioEnv "true")
                void (snd (runVet MirrorPruner (vetStoreBackends resolveAdapter mounts)))
                    `shouldBe` Left [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "refuses the deleting role a Verdaccio target carrying no deletion consent" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        refusalsOf (vetted MirrorPruner mounts)
            `shouldBe` Just [StoreMaintenanceUnavailable Npm (DeletionNotPermitted TagVerdaccio)]

    it "names the consent key an operator must set in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        renderedRefusals (vetted MirrorPruner mounts)
            `shouldSatisfy` any (T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION")

    it "refuses the preview role a target this build reaches no control plane for" $ do
        -- Everything but the consent key still stands, so a preview of a store this build cannot
        -- sweep refuses exactly as the deleting role's own boot does.
        mounts <- mountsFor (verdaccioEnv "true")
        refusalsOf (runVet MirrorPreviewer (vetStoreBackends withoutMaintenance mounts))
            `shouldBe` Just [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "refuses the deleting role an ecosystem whose protocol spells no delete" $ do
        -- The rule must not turn on which ecosystem the mount names, so the adapter is
        -- injected and the refusal is the rule's own rather than the registry's.
        mounts <- mountsFor (verdaccioEnv "true")
        refusalsOf (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldBe` Just [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "names the protocol in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        renderedRefusals (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldSatisfy` any (T.isInfixOf "npm protocol carries no package listing or version delete")

    it "boots every writing role on a configuration the Dredger refuses for either reason" $ do
        withheld <- mountsFor (verdaccioEnv "false")
        clearsNothing (vetted MirrorWriter withheld) `shouldBe` True
        consenting <- mountsFor (verdaccioEnv "true")
        clearsNothing (runVet MirrorWriter (vetStoreBackends withoutMaintenance consenting)) `shouldBe` True

previewCachesSpec :: Spec
previewCachesSpec = describe "vetPrivateCaches" $ do
    it "refuses a generic registry without an inventory backend" $ do
        config <- expectConfig staticEnvVars Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
        void result `shouldBe` Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable "registry has no inventory control plane")]

    it "accumulates private inventory refusals across mounts" $ do
        let env =
                [ ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/pypi/")
                , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__URL", "https://test-111122223333.d.codeartifact.us-east-1.amazonaws.com/pypi/mirror/")
                , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__UPLOAD_URL", "https://test-111122223333.d.codeartifact.us-east-1.amazonaws.com/pypi/mirror/")
                ]
                    <> [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/")]
                    <> withoutPrivateUpstreamUrl codeArtifactEnvVars
        config <- expectConfig env Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
            expected = [StoreMaintenanceUnavailable eco (PrivateCacheUnavailable "registry has no inventory control plane") | eco <- [Npm, PyPI]]
        void result `shouldBe` Left expected

    it "refuses a private protocol backend with no listing capability" $ do
        config <- expectConfig (withObservablePrivate (withoutPrivateAuthority codeArtifactEnvVars)) Nothing
        let result = privateCaches withoutMaintenance MirrorPreviewer config
        void result `shouldBe` Left [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "clears the deleting role an independently consenting private store" $ do
        config <- expectConfig codeArtifactEnvVars Nothing
        let result = privateCaches adapterFor MirrorPruner config
        fmap Map.null result `shouldBe` Right False

    for_ ["TOKEN", "PERMIT_DELETION"] $ \missing ->
        it ("refuses private deletion without its own " <> missing) $ do
            let key = "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__" <> missing
                env = filter ((/= key) . fst) codeArtifactEnvVars
            config <- expectConfig env Nothing
            let result = privateCaches adapterFor MirrorPruner config
            case result of
                Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable detail)] -> detail `shouldSatisfy` T.isInfixOf (toText key)
                other -> expectationFailure ("expected the private authority refusal, got " <> show (void other))

    it "adds no private observation when the mount has no mirror target" $ do
        config <- expectConfig (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars)) Nothing
        let result = privateCaches adapterFor MirrorPreviewer config
        fmap Map.null result `shouldBe` Right True

    it "refuses a private CodeArtifact endpoint that addresses no repository of this mount's own" $ do
        -- The loader refuses this endpoint now ('vetPrivateRepository'), so the pass is driven
        -- over a mount config built here rather than one a configuration could carry.
        config <- expectConfig codeArtifactEnvVars Nothing
        let unaddressable = Map.map (\mcfg -> mcfg{mntPrivateUpstream = Just unaddressablePrivateUpstream}) (cfgMounts (configApp config))
            result = snd (runVet MirrorPreviewer (vetPrivateCaches adapterFor unaddressable (configMounts config)))
        case result of
            Left [StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable detail)] ->
                detail `shouldSatisfy` T.isInfixOf "CodeArtifactRepositoryMissing"
            other -> expectationFailure ("expected the private cache refusal, got " <> show (void other))

    it "clears the private CodeArtifact cache on the repository its own endpoint addresses" $ do
        let env =
                ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", retainedEndpoint)
                    : withoutPrivateUpstreamUrl codeArtifactEnvVars
        config <- expectConfig env Nothing
        case privateCaches adapterFor MirrorPreviewer config of
            Right caches -> case Map.elems caches of
                [(backend, cleared)] -> do
                    clearedUrl cleared `shouldBe` retainedEndpoint
                    privateRepository backend `shouldBe` Just "retained"
                _ -> expectationFailure "expected one cleared private CodeArtifact cache"
            Left errors -> expectationFailure (show errors)

-- A pass that logged nothing and cleared no store: what every writing role's pass looks like.
clearsNothing :: ([Advisory], Either [BootError] (Map Ecosystem ClearedBackend)) -> Bool
clearsNothing = \case
    ([], Right cleared) -> Map.null cleared
    _ -> False

renderedRefusals :: ([Advisory], Either [BootError] a) -> [Text]
renderedRefusals = maybe [] (map renderBootError) . refusalsOf

-- The repository a cleared cache's own backend declaration addresses, absent on any other arm.
privateRepository :: Maybe StoreBackend -> Maybe Text
privateRepository backend = case sbControl <$> backend of
    Just (ControlCodeArtifact store) -> Just (casRepository store)
    Just ControlNone -> Nothing
    Just (ControlProtocol _ _) -> Nothing
    Nothing -> Nothing

-- | A private CodeArtifact cache on its own repository, distinct from the mirror target's.
retainedEndpoint :: (IsString s) => s
retainedEndpoint = "https://cache-999900001111.d.codeartifact.us-west-2.amazonaws.com/npm/retained/"
