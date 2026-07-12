-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.CredentialSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Credential (
    codeArtifactIdentityGroups,
    initCredentialProviders,
    initializedEcosystems,
    lookupProvider,
    parseCodeArtifactHost,
    planMirrorCredential,
    resolveCodeArtifactConfig,
 )
import Ecluse.Composition.Support (
    expectEnv,
    expectProviders,
    noTokenEnvVars,
    overrideEnv,
    staticEnvVars,
    withoutMirrorTargetUrl,
 )
import Ecluse.Config (AppConfig (cfgMounts), CredentialBackend (..))
import Ecluse.Core.Credential (authSecret, currentToken, unSecret)
import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))

spec :: Spec
spec = do
    credentialProvidersSpec
    identityGroupsSpec
    mirrorCredentialSpec
    parseCodeArtifactHostSpec

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

credentialProvidersSpec :: Spec
credentialProvidersSpec = describe "initCredentialProviders" $ do
    it "initializes the static provider from ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN when set" $ do
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        initializedEcosystems providers `shouldBe` fromList [Npm]

    it "yields the configured static token through the initialized provider" $ do
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        case lookupProvider Npm providers of
            Nothing -> expectationFailure "expected an initialized static provider"
            Just provider -> do
                tok <- currentToken provider
                unSecret (authSecret tok) `shouldBe` "mirror-write-token"

    it "initializes no provider when no static token is supplied" $ do
        env <- expectEnv noTokenEnvVars
        providers <- expectProviders env
        initializedEcosystems providers `shouldBe` mempty
        -- 'CredentialProvider' has no 'Eq'/'Show' (it is a record of an IO
        -- function), so resolution is asserted through 'isJust' on a 'Bool'.
        isJust (lookupProvider Npm providers) `shouldBe` False

    it "refuses to build when the gcp-artifact-registry provider is selected (not built)" $ do
        -- 'CredentialProviders' has no 'Show', so the Left is extracted to compare.
        env <- expectEnv (overrideEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "gcp-artifact-registry" staticEnvVars)
        result <- initCredentialProviders noCredentialReporters env
        leftToMaybe result `shouldBe` Just [MirrorCredentialProviderUnavailable GcpArtifactRegistryCredential]

    it "refuses to build when codeartifact is selected but its domain cannot be resolved" $ do
        -- A non-CodeArtifact ECLUSE_MOUNTS__NPM__MIRROR_TARGET and no explicit keys: domain and owner
        -- resolve by neither route, so both are named in the aggregated boot failure.
        env <- expectEnv (("AWS_REGION", "us-east-1") : overrideEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "codeartifact" staticEnvVars)
        result <- initCredentialProviders noCredentialReporters env
        leftToMaybe result
            `shouldBe` Just
                [ CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN"
                , CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER"
                ]

mirrorCredentialSpec :: Spec
mirrorCredentialSpec = describe "planMirrorCredential / resolveCodeArtifactConfig" $ do
    it "selects the static provider (no CodeArtifact config) by default" $ do
        env <- expectEnv staticEnvVars
        planMirrorCredential Npm env (cfgMounts env Map.! Npm) `shouldBe` Right Nothing

    it "refuses the gcp-artifact-registry provider as not built" $ do
        env <- expectEnv (overrideEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "gcp-artifact-registry" staticEnvVars)
        planMirrorCredential Npm env (cfgMounts env Map.! Npm) `shouldBe` Left [MirrorCredentialProviderUnavailable GcpArtifactRegistryCredential]

    it "resolves CodeArtifact inputs from the explicit MIRROR_TARGET_CODEARTIFACT_* keys" $ do
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "my-domain")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "111122223333")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "eu-west-1")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "1800")
                    : staticEnvVars
                )
        case resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm) of
            Right cfg -> do
                caDomain cfg `shouldBe` "my-domain"
                caDomainOwner cfg `shouldBe` Just "111122223333"
                caRegion cfg `shouldBe` "eu-west-1"
                caDurationSeconds cfg `shouldBe` Just 1800
            Left errs -> expectationFailure ("expected a resolved CodeArtifact config, got " <> show errs)

    it "parses the domain, owner, and region from a CodeArtifact ECLUSE_MOUNTS__NPM__MIRROR_TARGET host" $ do
        -- (b) the URL-parse fallback: a real CodeArtifact npm endpoint host, with a
        -- hyphenated domain so the 12-digit owner after the LAST hyphen is recovered.
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        case resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm) of
            Right cfg -> do
                caDomain cfg `shouldBe` "my-domain"
                caDomainOwner cfg `shouldBe` Just "111122223333"
                caRegion cfg `shouldBe` "us-west-2"
            Left errs -> expectationFailure ("expected the host to parse, got " <> show errs)

    it "ranks the host-encoded region above AWS_REGION (mints against the domain's region)" $ do
        -- N3: the endpoint host encodes the domain's authoritative region, so a
        -- cross-region deploy (us-west-2 URL, AWS_REGION=us-east-1) mints in us-west-2.
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/")
                    : ("AWS_REGION", "us-east-1")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        (caRegion <$> rightToMaybe (resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm))) `shouldBe` Just "us-west-2"

    it "falls the region back to AWS_REGION only when neither explicit key nor host supplies it" $ do
        -- A non-CodeArtifact mirror URL (no host region) and no explicit region: the
        -- process-wide AWS_REGION is the last resort.
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "d")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "111122223333")
                    : ("AWS_REGION", "ap-south-1")
                    : staticEnvVars
                )
        (caRegion <$> rightToMaybe (resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm))) `shouldBe` Just "ap-south-1"

    it "fails loud, naming the owner key, on a hyphen-bearing non-CodeArtifact host" $ do
        -- B1: a host whose tail after the last hyphen is not a 12-digit account id is
        -- not a CodeArtifact endpoint, so its owner never mis-parses (e.g. owner "b").
        -- With domain + region supplied explicitly, only the owner is unresolved.
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://a-b.d.codeartifact.us-east-1.amazonaws.com/npm/r/")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "a")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-east-1")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm) `shouldBe` Left [CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER"]

    it "fails loud on an explicit owner with less than 12 digits" $ do
        -- B1: a malformed explicit owner must be rejected, not sail through.
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "d")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "123")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-east-1")
                    : staticEnvVars
                )
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm)
            `shouldBe` Left [CodeArtifactConfigInvalid "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id"]

    it "fails loud on an explicit owner with more than 12 digits" $ do
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "d")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "1111222233334")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-east-1")
                    : staticEnvVars
                )
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm)
            `shouldBe` Left [CodeArtifactConfigInvalid "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id"]

    it "fails loud on an explicit owner with 12 characters but containing non-digits" $ do
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "d")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "11112222333a")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-east-1")
                    : staticEnvVars
                )
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm)
            `shouldBe` Left [CodeArtifactConfigInvalid "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id"]

    it "fails to parse a CodeArtifact host with an invalid account ID (containing non-digits), falling back to missing keys" $ do
        env <-
            expectEnv
                ( ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "codeartifact")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://my-domain-11112222333a.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "my-domain")
                    : ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-west-2")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm)
            `shouldBe` Left [CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER"]

    it "fails loud, naming each unresolved input, when neither key nor host supplies it" $ do
        -- A non-CodeArtifact mirror URL and no explicit keys / AWS_REGION: domain,
        -- owner, and region are all unresolved and all named in one aggregated failure.
        env <- expectEnv (overrideEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "codeartifact" staticEnvVars)
        resolveCodeArtifactConfig Npm env (cfgMounts env Map.! Npm)
            `shouldBe` Left
                [ CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN"
                , CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER"
                , CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION"
                ]

parseCodeArtifactHostSpec :: Spec
parseCodeArtifactHostSpec = describe "parseCodeArtifactHost" $ do
    it "parses a valid CodeArtifact host into domain, owner, and region" $ do
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com"
            `shouldBe` Just ("my-domain", "111122223333", "us-west-2")

    it "parses a valid CodeArtifact host with hyphens in the domain" $ do
        parseCodeArtifactHost "my-company-domain-111122223333.d.codeartifact.eu-central-1.amazonaws.com"
            `shouldBe` Just ("my-company-domain", "111122223333", "eu-central-1")

    it "returns Nothing if the host does not contain .d.codeartifact." $ do
        parseCodeArtifactHost "example.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-111122223333.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing

    it "returns Nothing if the host contains .d.codeartifact. multiple times" $ do
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.foo.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing

    it "returns Nothing if there is no hyphen separating domain and owner" $ do
        -- The parsing logic relies on finding the last hyphen in domainOwner.
        -- If there's no hyphen, T.breakOnEnd returns ("", "mydomain111122223333"),
        -- then T.dropEnd 1 "" is "", which fails the `nonBlank` check.
        parseCodeArtifactHost "mydomain111122223333.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing

    it "returns Nothing if the host is missing the .amazonaws.com suffix" $ do
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2" `shouldBe` Nothing

    it "returns Nothing if the owner is not exactly a 12-digit AWS account id" $ do
        parseCodeArtifactHost "my-domain-11112222333.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-1111222233334.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-11112222333a.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-owner.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
