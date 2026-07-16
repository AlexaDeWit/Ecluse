-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.MirrorCredentialSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.MirrorCredential (parseCodeArtifactHost, resolveMirrorCredential)
import Ecluse.Config.Types (ConfigError (..), MirrorCredential (..), renderConfigError)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Test.Package (unsafeRegistryUrl)

spec :: Spec
spec = do
    resolveMirrorCredentialSpec
    parseCodeArtifactHostSpec

-- The CodeArtifact endpoint used across the derivation cases: a hyphenated domain so
-- the 12-digit owner after the LAST hyphen is recovered.
codeArtifactTarget :: Text
codeArtifactTarget = "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/"

resolveMirrorCredentialSpec :: Spec
resolveMirrorCredentialSpec = describe "resolveMirrorCredential (the target dictates the credential)" $ do
    it "derives the CodeArtifact identity from a CodeArtifact target host" $ do
        resolveMirrorCredential Npm (unsafeRegistryUrl codeArtifactTarget) Nothing (Just 1800)
            `shouldBe` Right
                ( MirrorCodeArtifact
                    CodeArtifactConfig
                        { caRegion = "us-west-2"
                        , caDomain = "my-domain"
                        , caDomainOwner = Just "111122223333"
                        , caDurationSeconds = Just 1800
                        }
                )

    it "derives a static credential for a non-CodeArtifact target with a token" $ do
        resolveMirrorCredential Npm (unsafeRegistryUrl "https://mirror.example.test/") (Just (mkSecret "write-token")) Nothing
            `shouldBe` Right (MirrorStatic (mkSecret "write-token"))

    it "rejects a non-CodeArtifact target with no static write token, naming the key" $ do
        case resolveMirrorCredential Npm (unsafeRegistryUrl "https://mirror.example.test/") Nothing Nothing of
            Left err@(MirrorCredentialTokenMissing Npm) ->
                renderConfigError err `shouldSatisfy` T.isInfixOf "MIRROR_TARGET_TOKEN"
            other -> expectationFailure ("expected MirrorCredentialTokenMissing Npm, got " <> show other)

    it "rejects a CodeArtifact target that also carries a static token (the two must not contend)" $ do
        -- The #808 regression: a CodeArtifact-scoped mint identity can never be paired
        -- with an operator-supplied bearer, and a CodeArtifact endpoint's token is
        -- always minted -- so supplying both is a loud conflict, not a silent choice.
        case resolveMirrorCredential Npm (unsafeRegistryUrl codeArtifactTarget) (Just (mkSecret "stray")) (Just 1800) of
            Left err@(MirrorCredentialConflict Npm) ->
                renderConfigError err `shouldSatisfy` T.isInfixOf "MIRROR_TARGET_TOKEN"
            other -> expectationFailure ("expected MirrorCredentialConflict Npm, got " <> show other)

    it "never mints a CodeArtifact token for a non-CodeArtifact endpoint (#808)" $ do
        -- The core invariant, stated directly: no input can produce a MirrorCodeArtifact
        -- whose identity was not parsed from the very target host it will be sent to.
        let nonCodeArtifact = unsafeRegistryUrl "https://mirror.example.test/"
        resolveMirrorCredential Npm nonCodeArtifact (Just (mkSecret "t")) (Just 1800)
            `shouldSatisfy` \case
                Right (MirrorCodeArtifact _) -> False
                _ -> True

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
