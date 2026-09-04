-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.CredentialSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.Credential (
    codeArtifactIdentityGroups,
    initializedEcosystems,
    lookupProvider,
    mirrorBackends,
    providerLabel,
 )
import Ecluse.Composition.Support (
    expectConfig,
    expectProviders,
    staticEnvVars,
 )
import Ecluse.Config (Config (configMounts), StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio), sbTag)
import Ecluse.Core.Credential (authSecret, currentToken, unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Telemetry.Metrics (Label (LProvider), renderLabel)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))

spec :: Spec
spec = do
    credentialProvidersSpec
    providerLabelSpec
    identityGroupsSpec

credentialProvidersSpec :: Spec
credentialProvidersSpec = describe "initCredentialProviders" $ do
    it "realises a static provider for a non-CodeArtifact mirror target with a token" $ do
        config <- expectConfig staticEnvVars Nothing
        providers <- expectProviders config
        initializedEcosystems providers `shouldBe` fromList [Npm]

    it "yields the configured static token through the initialized provider" $ do
        config <- expectConfig staticEnvVars Nothing
        providers <- expectProviders config
        case lookupProvider Npm providers of
            Nothing -> expectationFailure "expected an initialized static provider"
            Just provider -> do
                tok <- currentToken provider
                unSecret (authSecret tok) `shouldBe` "mirror-write-token"

{- | The label every credential signal carries. It derives from the mount's own declared tag, so a
second minting store cannot report under the first one's name.
-}
providerLabelSpec :: Spec
providerLabelSpec = describe "providerLabel (the label a store's credential records under)" $ do
    it "spells every store tag as the configuration spells it" $
        map (renderLabel . LProvider . providerLabel) [TagRegistry, TagCodeArtifact, TagVerdaccio]
            `shouldBe` [("provider", "registry"), ("provider", "codeArtifact"), ("provider", "verdaccio")]

    it "labels two mounts declaring different stores under their own tags" $ do
        config <- expectConfig twoStoreEnvVars Nothing
        sortOn fst (map labelledBackend (mirrorBackends (Map.elems (configMounts config))))
            `shouldBe` [(Npm, "registry"), (PyPI, "verdaccio")]
  where
    labelledBackend (eco, backend) = (eco, snd (renderLabel (LProvider (providerLabel (sbTag backend)))))

{- | The domain-scoped sharing decision, pinned on its pure surface. CodeArtifact mints per domain,
so coinciding identities share a group, and a differing domain or duration keeps its own.
-}
identityGroupsSpec :: Spec
identityGroupsSpec = describe "codeArtifactIdentityGroups (per-domain provider sharing)" $ do
    it "groups ecosystems whose resolved CodeArtifact identities coincide" $ do
        let shared = caConfig "shared-domain" Nothing
        map (fmap (second sortNE)) (codeArtifactIdentityGroups [(Npm, TagCodeArtifact, shared), (PyPI, TagCodeArtifact, shared)])
            `shouldBe` [(shared, (TagCodeArtifact, Npm :| [PyPI]))]

    it "keeps distinct domains on distinct providers" $ do
        let a = caConfig "domain-a" Nothing
            b = caConfig "domain-b" Nothing
        sortOn fst (map (fmap (second sortNE)) (codeArtifactIdentityGroups [(Npm, TagCodeArtifact, a), (PyPI, TagCodeArtifact, b)]))
            `shouldBe` sortOn fst [(a, (TagCodeArtifact, Npm :| [])), (b, (TagCodeArtifact, PyPI :| []))]

    it "keeps a differing requested token duration on its own provider" $ do
        -- Same domain, different requested credential: the duration is a mint
        -- parameter, so the identities do not coincide.
        let short = caConfig "shared-domain" (Just 900)
            long = caConfig "shared-domain" (Just 3600)
        length (codeArtifactIdentityGroups [(Npm, TagCodeArtifact, short), (PyPI, TagCodeArtifact, long)]) `shouldBe` 2

{- | 'staticEnvVars' with a pypi mount mirroring to a Verdaccio store, so one load carries two
mounts declared under two different tags.
-}
twoStoreEnvVars :: [(String, String)]
twoStoreEnvVars =
    [ ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", "https://pypi-private.example.test")
    , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__VERDACCIO__URL", "https://pypi-mirror.example.test")
    , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__VERDACCIO__TOKEN", "pypi-write-token")
    ]
        <> staticEnvVars

sortNE :: NonEmpty Ecosystem -> NonEmpty Ecosystem
sortNE = NE.sort

caConfig :: Text -> Maybe Natural -> CodeArtifactConfig
caConfig domain duration =
    CodeArtifactConfig
        { caRegion = "us-east-1"
        , caDomain = domain
        , caDomainOwner = Just "111122223333"
        , caDurationSeconds = duration
        }
