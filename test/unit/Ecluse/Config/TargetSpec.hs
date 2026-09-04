-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.TargetSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (StoreTagConflict))
import Ecluse.Composition.Endpoints (vetEndpoints)
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    AppConfig (cfgMounts),
    Config (configApp, configMounts),
    ConfigError,
    ControlPlane (ControlCodeArtifact, ControlNone),
    DeletionConsent (DeletionPermitted, DeletionWithheld),
    MintPlan (MintCodeArtifact, MintStatic),
    MirrorTarget (mtBackend),
    Mount (mountRegistries),
    MountConfig,
    StoreBackend (BackendRegistry, BackendVerdaccio),
    StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio),
    loadConfig,
    regMirrorTarget,
    renderConfigError,
    sbControl,
    sbMint,
    sbTag,
 )
import Ecluse.Config.Target (parseCodeArtifactHost)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatToken)

spec :: Spec
spec = do
    admissionSpec
    layeringSpec
    hostValidationSpec
    backendSpec
    tagCollisionSpec
    parseCodeArtifactHostSpec

{- Every cell of the store admission matrix: which tags an endpoint admits, and which keys each
tag admits there. A shape outside its cell refuses at load, naming the key path. -}
admissionSpec :: Spec
admissionSpec = describe "the tags and keys each endpoint admits" $ do
    describe "publicUpstream" $ do
        it "admits registry with a url" $
            loadsWith [decl "publicUpstream" "registry" [url publicUrl]]

        it "admits no other tag, naming the one it does" $ do
            refuses
                [decl "publicUpstream" "codeArtifact" [url codeArtifactMirror]]
                "publicUpstream must name exactly one store tag (registry)"
            refuses
                [decl "publicUpstream" "verdaccio" [url verdaccioUrl]]
                "publicUpstream must name exactly one store tag (registry)"

        it "admits no key beside url" $
            -- The shipped template always supplies this url, so the merged document can never
            -- lack it. 'privateUpstream' below pins the registry tag's own required-url refusal.
            refuses
                [decl "publicUpstream" "registry" [url publicUrl, field "token" "t"]]
                "unexpected publicUpstream.registry key(s): \"token\""

    describe "privateUpstream" $ do
        it "admits every tag, each carrying the url alone" $ do
            loadsWith [decl "privateUpstream" "registry" [url privateUrl]]
            loadsWith [decl "privateUpstream" "codeArtifact" [url codeArtifactInternal]]
            loadsWith [decl "privateUpstream" "verdaccio" [url verdaccioUrl]]

        it "requires the url and admits no credential, because a read is passthrough" $ do
            refuses [decl "privateUpstream" "registry" []] "privateUpstream.registry.url is required"
            refuses
                [decl "privateUpstream" "verdaccio" [url verdaccioUrl, field "token" "t"]]
                "unexpected privateUpstream.verdaccio key(s): \"token\""

    describe "mirrorTarget" $ do
        it "admits registry with a url and the static write token it needs" $
            loadsWith (mirrored [decl "mirrorTarget" "registry" [url mirrorUrl, field "token" "t"]])

        it "refuses a registry mirror target with no write token" $
            refuses
                (mirrored [decl "mirrorTarget" "registry" [url mirrorUrl]])
                "mirrorTarget.registry.token is required"

        it "admits codeArtifact with a url and an optional token duration" $ do
            loadsWith (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror]])
            loadsWith
                (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror, field "tokenDuration" "3600"]])

        it "refuses a static token beside the one codeArtifact mints" $
            -- The mirror write is Écluse's one standing credential, so a minting tag admits
            -- no operator-supplied bearer to contend with it.
            refuses
                (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror, field "token" "t"]])
                "unexpected mirrorTarget.codeArtifact key(s): \"token\""

        it "admits verdaccio with a url, a write token, and the optional deletion consent" $ do
            loadsWith (mirrored [decl "mirrorTarget" "verdaccio" [url verdaccioUrl, field "token" "t"]])
            loadsWith (mirrored [decl "mirrorTarget" "verdaccio" verdaccioWriteKeys])

        it "refuses a verdaccio mirror target with no write token" $
            refuses
                (mirrored [decl "mirrorTarget" "verdaccio" [url verdaccioUrl]])
                "mirrorTarget.verdaccio.token is required"

        it "keeps each tag's own keys off the others" $ do
            refuses
                (mirrored [decl "mirrorTarget" "registry" [url mirrorUrl, field "token" "t", permitDeletion]])
                "unexpected mirrorTarget.registry key(s): \"permitDeletion\""
            refuses
                (mirrored [decl "mirrorTarget" "verdaccio" [url verdaccioUrl, field "token" "t", field "tokenDuration" "3600"]])
                "unexpected mirrorTarget.verdaccio key(s): \"tokenDuration\""

    describe "publicationTarget" $ do
        it "admits every tag, with the fallback token optional on each" $ do
            loadsWith [decl "publicationTarget" "registry" [url publishUrl]]
            loadsWith [decl "publicationTarget" "registry" [url publishUrl, field "token" "t"]]
            loadsWith [decl "publicationTarget" "codeArtifact" [url codeArtifactInternal, field "token" "t"]]
            loadsWith [decl "publicationTarget" "verdaccio" [url verdaccioUrl, field "token" "t"]]

        it "requires the url and admits neither a mint lifetime nor a consent flag" $ do
            refuses [decl "publicationTarget" "codeArtifact" []] "publicationTarget.codeArtifact.url is required"
            refuses
                [decl "publicationTarget" "registry" [url publishUrl, field "tokenDuration" "3600"]]
                "unexpected publicationTarget.registry key(s): \"tokenDuration\""
            refuses
                [decl "publicationTarget" "verdaccio" [url verdaccioUrl, permitDeletion]]
                "unexpected publicationTarget.verdaccio key(s): \"permitDeletion\""

{- A layer fills keys under a tag, it never switches one, because 'deepMerge' unions the two
objects rather than replacing the document's. -}
layeringSpec :: Spec
layeringSpec = describe "one tag per endpoint" $ do
    it "refuses two tags written on one endpoint, listing what was written" $
        refuses
            (mirrored [twoTagged])
            "mirrorTarget must name exactly one store tag (registry, codeArtifact, verdaccio), got: \"codeArtifact\", \"verdaccio\""

    it "refuses an environment override that writes a second tag over the document's" $
        loadConfig
            (pubUrlEnv <> [("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN", "t")])
            (Just (mountDoc (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror]])))
            `shouldSatisfy` refusalMentions "must name exactly one store tag"

    it "fills a key under the tag the document declared, from the environment layer" $ do
        -- The arrangement a deployment writes: the document names the tag, and the environment
        -- supplies a value under it without ever naming a second tag.
        config <-
            expectLoad
                (pubUrlEnv <> [("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__TOKEN_DURATION", "1800")])
                (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror]])
        backend <- mirrorBackendOf config
        case sbMint backend of
            MintCodeArtifact caConfig -> caDurationSeconds caConfig `shouldBe` Just 1800
            MintStatic _ -> expectationFailure "expected the CodeArtifact mint the tag names"

    it "refuses an endpoint that is not an object at all" $
        refuses
            [decl' "mirrorTarget" "\"https://mirror.example.test\""]
            "mirrorTarget must be an object naming exactly one store tag"

-- The tag names the store, so a URL that contradicts it is refused under the key that wrote it.
hostValidationSpec :: Spec
hostValidationSpec = describe "the URL a tag admits" $ do
    it "refuses a codeArtifact read target whose host is not a CodeArtifact endpoint" $ do
        let outcome = loadMount [decl "privateUpstream" "codeArtifact" [url verdaccioUrl]]
        outcome `shouldSatisfy` refusalMentions "mounts.npm.privateUpstream.codeArtifact.url"
        outcome `shouldSatisfy` refusalMentions "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL"

    it "refuses a codeArtifact publication target whose host is not a CodeArtifact endpoint" $
        loadMount [decl "publicationTarget" "codeArtifact" [url publishUrl]]
            `shouldSatisfy` refusalMentions "mounts.npm.publicationTarget.codeArtifact.url"

    it "refuses a codeArtifact mirror target whose host is not a CodeArtifact endpoint" $
        loadMount (mirrored [decl "mirrorTarget" "codeArtifact" [url mirrorUrl]])
            `shouldSatisfy` refusalMentions "mounts.npm.mirrorTarget.codeArtifact.url"

    it "refuses a mirror target addressing another format's endpoint of the same repository" $
        -- A repository's per-format endpoints are separate stores, so an npm mount pointed at
        -- the pypi endpoint would mirror into a store it never reads back.
        loadMount (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactPyPI]])
            `shouldSatisfy` refusalMentions "its path must be /npm/{repository}/"

    it "refuses a mirror target naming no repository at all" $
        loadMount (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactBare]])
            `shouldSatisfy` refusalMentions "its path must be /npm/{repository}/"

    it "refuses a codeArtifact mirror target on an ecosystem CodeArtifact has no format for" $
        loadConfig pubUrlEnv (Just rubygemsDoc)
            `shouldSatisfy` refusalMentions "CodeArtifact carries no package format for the rubygems ecosystem"

-- The tag the operator declared is the backend every role reads, with no host shape consulted.
backendSpec :: Spec
backendSpec = describe "the store backend a mirror target resolves to" $ do
    it "resolves registry to a static write credential and no control plane" $ do
        backend <- backendFor [decl "mirrorTarget" "registry" [url mirrorUrl, field "token" "write-token"]]
        backend `shouldBe` BackendRegistry (mkSecret "write-token")
        sbTag backend `shouldBe` TagRegistry
        sbMint backend `shouldBe` MintStatic (mkSecret "write-token")
        sbControl backend `shouldBe` ControlNone

    it "resolves codeArtifact to the identity its host carries and the repository it addresses" $ do
        backend <- backendFor [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror, field "tokenDuration" "1800"]]
        sbTag backend `shouldBe` TagCodeArtifact
        sbMint backend
            `shouldBe` MintCodeArtifact
                CodeArtifactConfig
                    { caRegion = "eu-west-1"
                    , caDomain = "acme"
                    , caDomainOwner = Just "111122223333"
                    , caDurationSeconds = Just 1800
                    }
        case sbControl backend of
            ControlCodeArtifact store -> do
                casRepository store `shouldBe` "mirror"
                formatToken (casFormat store) `shouldBe` "npm"
            ControlNone -> expectationFailure "expected a CodeArtifact control plane"

    it "resolves verdaccio to a static credential carrying the declared deletion consent" $ do
        permitted <- backendFor [decl "mirrorTarget" "verdaccio" verdaccioWriteKeys]
        permitted `shouldBe` BackendVerdaccio (mkSecret "write-token") DeletionPermitted
        sbTag permitted `shouldBe` TagVerdaccio
        sbMint permitted `shouldBe` MintStatic (mkSecret "write-token")
        sbControl permitted `shouldBe` ControlNone

    it "withholds deletion consent on a verdaccio store that never declares it" $ do
        backend <- backendFor [decl "mirrorTarget" "verdaccio" [url verdaccioUrl, field "token" "write-token"]]
        backend `shouldBe` BackendVerdaccio (mkSecret "write-token") DeletionWithheld

-- One store has one backend, so two endpoints at one registry must name one tag.
tagCollisionSpec :: Spec
tagCollisionSpec = describe "one tag per store" $ do
    it "refuses one registry declared under two tags, for every role" $ do
        mounts <- mountsFor [decl "privateUpstream" "verdaccio" [url sharedUrl], decl "mirrorTarget" "registry" [url sharedUrl, field "token" "t"]]
        for_ [MirrorWriter, MirrorPruner] $ \role -> do
            let refusals = tagConflicts role mounts
            refusals `shouldSatisfy` any (isConflictAt "privateUpstream.verdaccio" "mirrorTarget.registry")

    it "stays silent on the development topology that names one Verdaccio on every endpoint" $ do
        -- The end-to-end harness declares exactly this, which is why verdaccio is admitted
        -- on a read target at all.
        mounts <- mountsFor developmentTopology
        for_ [MirrorWriter, MirrorPruner] $ \role -> tagConflicts role mounts `shouldBe` []

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

-- One endpoint declaration: the key, the tag under it, and the keys written under that tag.
decl :: Text -> Text -> [Text] -> Text
decl key tag keys = decl' key ("{\"" <> tag <> "\":{" <> T.intercalate "," keys <> "}}")

-- An endpoint key holding an arbitrary value, for the shapes a tagged object refuses.
decl' :: Text -> Text -> Text
decl' key body = "\"" <> key <> "\":" <> body

field :: Text -> Text -> Text
field key value = "\"" <> key <> "\":\"" <> value <> "\""

url :: Text -> Text
url = field "url"

permitDeletion :: Text
permitDeletion = "\"permitDeletion\":true"

-- The two keys a Verdaccio mirror write declares beyond its url.
verdaccioWriteKeys :: [Text]
verdaccioWriteKeys = [url verdaccioUrl, field "token" "write-token", permitDeletion]

-- A mirrored mount needs the private upstream its mirror is read back through.
mirrored :: [Text] -> [Text]
mirrored endpoints = decl "privateUpstream" "registry" [url privateUrl] : endpoints

-- The harness topology: one Verdaccio named by every endpoint that may hold one.
developmentTopology :: [Text]
developmentTopology =
    [ decl "privateUpstream" "verdaccio" [url sharedUrl]
    , decl "mirrorTarget" "verdaccio" [url sharedUrl, field "token" "t"]
    , decl "publicationTarget" "verdaccio" [url sharedUrl]
    ]

twoTagged :: Text
twoTagged =
    decl' "mirrorTarget" $
        "{\"codeArtifact\":{" <> url codeArtifactMirror <> "},\"verdaccio\":{" <> url verdaccioUrl <> "}}"

mountDoc :: [Text] -> ByteString
mountDoc endpoints = encodeUtf8 ("{\"mounts\":{\"npm\":{" <> T.intercalate "," endpoints <> "}}}")

-- A rubygems mount mirroring into CodeArtifact, which carries no format for that ecosystem.
rubygemsDoc :: ByteString
rubygemsDoc =
    encodeUtf8 $
        "{\"mounts\":{\"rubygems\":{"
            <> T.intercalate
                ","
                [ decl "privateUpstream" "registry" [url privateUrl]
                , decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror]
                ]
            <> "}}}"

loadMount :: [Text] -> Either [ConfigError] Config
loadMount endpoints = loadConfig pubUrlEnv (Just (mountDoc endpoints))

loadsWith :: [Text] -> Expectation
loadsWith endpoints = case loadMount endpoints of
    Left errs -> expectationFailure ("expected a load, got " <> show (map renderConfigError errs))
    Right _ -> pass

refuses :: [Text] -> Text -> Expectation
refuses endpoints phrase = loadMount endpoints `shouldSatisfy` refusalMentions phrase

refusalMentions :: Text -> Either [ConfigError] a -> Bool
refusalMentions phrase = \case
    Left errs -> any (T.isInfixOf phrase . renderConfigError) errs
    Right _ -> False

-- The npm mount's resolved mirror-write backend, failing the test on anything else.
backendFor :: [Text] -> IO StoreBackend
backendFor endpoints = mirrorBackendOf =<< expectLoad pubUrlEnv (mirrored endpoints)

mirrorBackendOf :: Config -> IO StoreBackend
mirrorBackendOf config =
    case regMirrorTarget . mountRegistries =<< Map.lookup Npm (configMounts config) of
        Just target -> pure (mtBackend target)
        Nothing -> fail "the npm mount resolved no mirror target"

mountsFor :: [Text] -> IO (Map Ecosystem MountConfig)
mountsFor endpoints = cfgMounts . configApp <$> expectLoad pubUrlEnv endpoints

-- Load one npm mount under an environment layer, failing the test on a refusal.
expectLoad :: [(String, String)] -> [Text] -> IO Config
expectLoad env endpoints =
    either (fail . show . map renderConfigError) pure (loadConfig env (Just (mountDoc endpoints)))

-- The tag-conflict refusals one role's endpoint pass earns, with its other findings dropped.
tagConflicts :: RegistryRole -> Map Ecosystem MountConfig -> [BootError]
tagConflicts role mounts = case snd (runVet role (vetEndpoints mounts)) of
    Left errs -> [err | err@StoreTagConflict{} <- errs]
    Right _ -> []

isConflictAt :: Text -> Text -> BootError -> Bool
isConflictAt key otherKey = \case
    StoreTagConflict _ written _ otherWritten _ -> written == key && otherWritten == otherKey
    _ -> False

pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

publicUrl, privateUrl, mirrorUrl, publishUrl, verdaccioUrl, sharedUrl :: Text
publicUrl = "https://registry.npmjs.org"
privateUrl = "https://private.example.test"
mirrorUrl = "https://mirror.example.test"
publishUrl = "https://publish.example.test"
verdaccioUrl = "https://verdaccio.example.test/"
sharedUrl = "https://shared.example.test/"

codeArtifactMirror, codeArtifactInternal, codeArtifactPyPI, codeArtifactBare :: Text
codeArtifactMirror = codeArtifactDomain <> "/npm/mirror/"
codeArtifactInternal = codeArtifactDomain <> "/npm/internal/"
codeArtifactPyPI = codeArtifactDomain <> "/pypi/mirror/"
codeArtifactBare = codeArtifactDomain <> "/npm/"

codeArtifactDomain :: Text
codeArtifactDomain = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com"
