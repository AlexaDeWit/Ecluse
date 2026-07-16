-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.CredentialSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Test.Hspec

import Ecluse.Composition.Credential (
    codeArtifactIdentityGroups,
    initializedEcosystems,
    lookupProvider,
 )
import Ecluse.Composition.Support (
    expectConfig,
    expectProviders,
    staticEnvVars,
 )
import Ecluse.Core.Credential (authSecret, currentToken, unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))

spec :: Spec
spec = do
    credentialProvidersSpec
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

{- | The domain-scoped sharing decision, pinned on its pure surface: a CodeArtifact
token is minted per domain, so mounts whose resolved identities coincide share one
provider group (one boot mint, one refresh schedule, one breaker), while a
differing identity -- another domain, or another requested duration -- keeps its
own.
-}
identityGroupsSpec :: Spec
identityGroupsSpec = describe "codeArtifactIdentityGroups (per-domain provider sharing)" $ do
    it "groups ecosystems whose resolved CodeArtifact identities coincide" $ do
        let shared = caConfig "shared-domain" Nothing
        map (fmap sortNE) (codeArtifactIdentityGroups [(Npm, shared), (PyPI, shared)])
            `shouldBe` [(shared, Npm :| [PyPI])]

    it "keeps distinct domains on distinct providers" $ do
        let a = caConfig "domain-a" Nothing
            b = caConfig "domain-b" Nothing
        sortOn fst (map (fmap sortNE) (codeArtifactIdentityGroups [(Npm, a), (PyPI, b)]))
            `shouldBe` sortOn fst [(a, Npm :| []), (b, PyPI :| [])]

    it "keeps a differing requested token duration on its own provider" $ do
        -- Same domain, different requested credential: the duration is a mint
        -- parameter, so the identities do not coincide.
        let short = caConfig "shared-domain" (Just 900)
            long = caConfig "shared-domain" (Just 3600)
        length (codeArtifactIdentityGroups [(Npm, short), (PyPI, long)]) `shouldBe` 2

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
