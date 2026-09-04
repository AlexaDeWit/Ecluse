-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.TargetSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.Target (
    CodeArtifactAbsence (NoFormatFor, NotRepositoryEndpoint),
    ControlPlane (ControlCodeArtifact, ControlNone),
    MintPlan (MintCodeArtifact, MintStatic),
    StoreBackend (sbControl, sbMint, sbTag),
    StoreTag (TagCodeArtifact, TagRegistry),
    parseCodeArtifactHost,
    resolveStoreBackend,
 )
import Ecluse.Config.Types (ConfigError (..), renderConfigError)
import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatToken)
import Ecluse.Test.Package (unsafeRegistryUrl)

spec :: Spec
spec = do
    mintSpec
    controlSpec
    parseCodeArtifactHostSpec

-- The CodeArtifact endpoint used across the derivation cases: a hyphenated domain so
-- the 12-digit owner after the LAST hyphen is recovered.
codeArtifactTarget :: Text
codeArtifactTarget = "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/"

mintSpec :: Spec
mintSpec = describe "resolveStoreBackend (the target dictates the credential)" $ do
    it "derives the CodeArtifact identity from a CodeArtifact target host" $ do
        backend <- resolvedAt Npm codeArtifactTarget Nothing (Just 1800)
        sbTag backend `shouldBe` TagCodeArtifact
        sbMint backend
            `shouldBe` MintCodeArtifact
                CodeArtifactConfig
                    { caRegion = "us-west-2"
                    , caDomain = "my-domain"
                    , caDomainOwner = Just "111122223333"
                    , caDurationSeconds = Just 1800
                    }

    it "derives a static credential for a non-CodeArtifact target with a token" $ do
        backend <- resolvedAt Npm nonCodeArtifactTarget (Just (mkSecret "write-token")) Nothing
        sbTag backend `shouldBe` TagRegistry
        sbMint backend `shouldBe` MintStatic (mkSecret "write-token")

    it "rejects a non-CodeArtifact target with no static write token, naming the key" $ do
        case resolveStoreBackend Npm (unsafeRegistryUrl nonCodeArtifactTarget) Nothing Nothing of
            Left err@(MirrorCredentialTokenMissing Npm) ->
                renderConfigError err `shouldSatisfy` T.isInfixOf "MIRROR_TARGET_TOKEN"
            other -> expectationFailure ("expected MirrorCredentialTokenMissing Npm, got " <> show other)

    it "rejects a CodeArtifact target that also carries a static token (the two must not contend)" $ do
        -- A CodeArtifact endpoint's token is always minted, so an operator-supplied bearer beside
        -- it is a loud conflict, never a silent choice.
        case resolveStoreBackend Npm (unsafeRegistryUrl codeArtifactTarget) (Just (mkSecret "stray")) (Just 1800) of
            Left err@(MirrorCredentialConflict Npm) ->
                renderConfigError err `shouldSatisfy` T.isInfixOf "MIRROR_TARGET_TOKEN"
            other -> expectationFailure ("expected MirrorCredentialConflict Npm, got " <> show other)

    it "never mints a CodeArtifact token for a non-CodeArtifact endpoint (#808)" $ do
        -- The core invariant: no input produces a MintCodeArtifact whose identity
        -- was not parsed from the target host that receives it.
        resolveStoreBackend Npm (unsafeRegistryUrl nonCodeArtifactTarget) (Just (mkSecret "t")) (Just 1800)
            `shouldSatisfy` \case
                Right backend -> case sbMint backend of
                    MintCodeArtifact _ -> False
                    MintStatic _ -> True
                Left _ -> False

{- The control plane the same value carries: which store the Dredger may delete from, and why a
target reaches none. A CodeArtifact path that addresses no repository stays the Dredger's refusal
alone, so every role that never deletes still boots on it. -}
controlSpec :: Spec
controlSpec = describe "resolveStoreBackend (the control plane the target reaches)" $ do
    it "reads the domain, its owner, the region, and the repository from the endpoint" $ do
        store <- storeAt Npm npmEndpoint
        casDomain store `shouldBe` "acme"
        casDomainOwner store `shouldBe` "111122223333"
        casRegion store `shouldBe` "eu-west-1"
        casRepository store `shouldBe` "mirror"
        formatToken (casFormat store) `shouldBe` "npm"

    it "keeps a domain that carries its own hyphens" $ do
        store <- storeAt Npm hyphenatedEndpoint
        casDomain store `shouldBe` "acme-eu-team"

    it "reads a pypi mount from the pypi endpoint of the same repository" $ do
        store <- storeAt PyPI pypiEndpoint
        casRepository store `shouldBe` "mirror"
        formatToken (casFormat store) `shouldBe` "pypi"

    it "reaches no control plane for a host no backend in this build recognises" $ do
        backend <- resolvedAt Npm verdaccioEndpoint staticToken Nothing
        sbTag backend `shouldBe` TagRegistry
        sbControl backend `shouldBe` ControlNone

    it "names the tag rather than the ecosystem's format for an unrecognised host" $ do
        -- A RubyGems mirror on Verdaccio reaches no control plane for its tag, not for a
        -- CodeArtifact format it was never going to need.
        backend <- resolvedAt RubyGems verdaccioGemsEndpoint staticToken Nothing
        sbTag backend `shouldBe` TagRegistry
        sbControl backend `shouldBe` ControlNone

    it "carries the absence for an ecosystem CodeArtifact has no format for" $
        controlAt RubyGems npmEndpoint `shouldReturn` Left (NoFormatFor RubyGems)

    it "carries the absence for a mount pointed at another format's endpoint of the same repository" $
        controlAt Npm pypiEndpoint `shouldReturn` Left (NotRepositoryEndpoint "npm")

    it "carries the absence for an endpoint that names no repository" $
        controlAt Npm "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/"
            `shouldReturn` Left (NotRepositoryEndpoint "npm")

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
        parseCodeArtifactHost "mydomain111122223333.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing

    it "returns Nothing if the host is missing the .amazonaws.com suffix" $ do
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2" `shouldBe` Nothing

    it "returns Nothing if the owner is not exactly a 12-digit AWS account id" $ do
        parseCodeArtifactHost "my-domain-11112222333.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-1111222233334.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-11112222333a.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing
        parseCodeArtifactHost "my-domain-owner.d.codeartifact.us-west-2.amazonaws.com" `shouldBe` Nothing

-- Resolve one target, failing the test on a load-time refusal.
resolvedAt :: Ecosystem -> Text -> Maybe Secret -> Maybe Natural -> IO StoreBackend
resolvedAt eco url mToken mDuration =
    either (fail . toString . renderConfigError) pure $
        resolveStoreBackend eco (unsafeRegistryUrl url) mToken mDuration

-- The CodeArtifact control plane a target resolves to, or the absence it carries instead.
controlAt :: Ecosystem -> Text -> IO (Either CodeArtifactAbsence CodeArtifactStore)
controlAt eco url = do
    backend <- resolvedAt eco url Nothing Nothing
    case sbControl backend of
        ControlCodeArtifact addressed -> pure addressed
        ControlNone -> fail "expected a CodeArtifact control plane"

-- The addressed store, failing the test on the absence.
storeAt :: Ecosystem -> Text -> IO CodeArtifactStore
storeAt eco url = controlAt eco url >>= either (fail . show) pure

staticToken :: Maybe Secret
staticToken = Just (mkSecret "write-token")

nonCodeArtifactTarget :: (IsString s) => s
nonCodeArtifactTarget = "https://mirror.example.test/"

verdaccioEndpoint :: (IsString s) => s
verdaccioEndpoint = "https://verdaccio.example.test/npm/mirror/"

verdaccioGemsEndpoint :: (IsString s) => s
verdaccioGemsEndpoint = "https://verdaccio.example.test/gems/mirror/"

npmEndpoint :: (IsString s) => s
npmEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"

pypiEndpoint :: (IsString s) => s
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

hyphenatedEndpoint :: (IsString s) => s
hyphenatedEndpoint = "https://acme-eu-team-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"
