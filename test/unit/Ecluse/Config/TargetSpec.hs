-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.TargetSpec (spec) where

import Prelude hiding (universe)

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Universe.Class (universe)
import Network.HTTP.Client qualified as Client
import Test.Hspec

import Ecluse.Composition.BootError (BootError (StoreTagConflict))
import Ecluse.Composition.Endpoints (vetEndpoints)
import Ecluse.Composition.Support (codeArtifactDomain, expectConfig, mountDocFor, npmMountDoc, pubUrlEnv)
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    AppConfig (cfgMounts),
    Config (configApp, configMounts),
    ConfigError,
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    DeletionConsent (DeletionPermitted, DeletionWithheld),
    MintPlan (MintCodeArtifact, MintStatic),
    MirrorTarget (mtBackend),
    Mount (mountRegistries),
    MountConfig (mntPrivateUpstream),
    PrivateEndpoint (preTarget),
    StoreBackend (BackendRegistry, BackendVerdaccio),
    StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio),
    Target,
    loadConfig,
    regMirrorTarget,
    renderConfigError,
    sbControl,
    sbMint,
    sbTag,
 )
import Ecluse.Config.Target (parseCodeArtifactHost, resolvePrivateBackend, storePresentation)
import Ecluse.Core.Credential (ClientCredential (ClientCredential), mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, RubyGems))
import Ecluse.Core.Registry.Request (StoreOperation (StoreRead, StoreWrite), attachCredential, nativePresentation, presentedMapping)
import Ecluse.Core.Registry.RubyGems.Credential (rubyPublishCredential, rubyReadCredential)
import Ecluse.Test.Support (parseRequestOrFail)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatToken)

spec :: Spec
spec = do
    admissionSpec
    layeringSpec
    hostValidationSpec
    backendSpec
    privateBackendSpec
    tagCollisionSpec
    parseCodeArtifactHostSpec
    storePresentationSpec

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

        it "requires the url and admits independent Verdaccio maintenance fields" $ do
            refuses [decl "privateUpstream" "registry" []] "privateUpstream.registry.url is required"
            loadsWith [decl "privateUpstream" "verdaccio" [url verdaccioUrl, field "token" "private-token", permitDeletion]]
            refuses [decl "privateUpstream" "registry" [url privateUrl, permitDeletion]] "unexpected privateUpstream.registry key(s): \"permitDeletion\""

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

    -- url is where Écluse reads and uploadUrl where it writes, so only a protocol that writes elsewhere takes one.
    describe "uploadUrl" $ do
        for_ [("npm", "npm"), ("rubygems", "ruby")] $ \(eco, format) ->
            it ("refuses uploadUrl under every tag of both " <> toString eco <> " write targets") $
                for_ (writeTargets format) $ \target@(key, tag, _, _) -> do
                    let outcome = loadFor eco (withUploadUrl target)
                    outcome `shouldSatisfy` refusalMentions ("mounts." <> eco <> "." <> key <> "." <> tag <> ".uploadUrl")
                    outcome `shouldSatisfy` refusalMentions (eco <> " derives its write address from url")

        for_ [("pypi", "pypi"), ("cargo", "cargo")] $ \(eco, format) ->
            it ("admits uploadUrl under every tag of both " <> toString eco <> " write targets") $
                for_ (writeTargets format) $ \target ->
                    void (expectConfig pubUrlEnv (Just (mountDocFor eco (withUploadUrl target))))

        it "names the refused key in both spellings and the remedy" $
            renderedRefusals (loadFor "rubygems" [decl "publicationTarget" "registry" [url publishUrl, field "uploadUrl" publishUrl]])
                `shouldBe` [ "mounts.rubygems.publicationTarget.registry.uploadUrl (ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL) is set, but rubygems derives its write address from url: remove uploadUrl and keep the registry address in url"
                           ]

        it "reports every refused uploadUrl across mounts in one load" $ do
            let refusals =
                    renderedRefusals . flip loadConfig Nothing $
                        pubUrlEnv
                            <> [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL", toString publishUrl)
                               , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL", toString publishUrl)
                               , ("ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__URL", toString publishUrl)
                               , ("ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL", toString publishUrl)
                               ]
            refusals `shouldSatisfy` any (T.isInfixOf "mounts.npm.publicationTarget.registry.uploadUrl")
            refusals `shouldSatisfy` any (T.isInfixOf "mounts.rubygems.publicationTarget.registry.uploadUrl")

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
            (Just (npmMountDoc (mirrored [decl "mirrorTarget" "codeArtifact" [url codeArtifactMirror]])))
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

    it "refuses a codeArtifact private upstream naming no repository at all, under its own key" $ do
        -- The boot asks that repository what it aggregates, so one the URL does not name is
        -- refused here rather than left unasked.
        let outcome = loadMount [decl "privateUpstream" "codeArtifact" [url codeArtifactBare]]
        outcome `shouldSatisfy` refusalMentions "its path must be /npm/{repository}/"
        outcome `shouldSatisfy` refusalMentions "mounts.npm.privateUpstream.codeArtifact.url"
        outcome `shouldSatisfy` refusalMentions "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL"
        outcome `shouldNotSatisfy` refusalMentions "mirrorTarget"

    it "refuses a codeArtifact private upstream addressing another format's endpoint, under its own key" $ do
        -- A repository's per-format endpoints are separate stores, so the npm mount would ask the
        -- aggregation question of a repository it never reads from.
        let outcome = loadMount [decl "privateUpstream" "codeArtifact" [url codeArtifactPyPI]]
        outcome `shouldSatisfy` refusalMentions "its path must be /npm/{repository}/"
        outcome `shouldSatisfy` refusalMentions "mounts.npm.privateUpstream.codeArtifact.url"
        outcome `shouldNotSatisfy` refusalMentions "mirrorTarget"

    it "resolves a Ruby CodeArtifact mirror endpoint" $ do
        config <- expectConfig pubUrlEnv (Just (mountDocFor "rubygems" (rubygemsMirror (codeArtifactDomain <> "/ruby/mirror/"))))
        let target = regMirrorTarget . mountRegistries =<< Map.lookup RubyGems (configMounts config)
        (sbTag . mtBackend <$> target) `shouldBe` Just TagCodeArtifact

    for_ [("missing repository", "/ruby/"), ("wrong format", "/npm/mirror/")] $ \(description, path) ->
        it ("refuses a Ruby CodeArtifact mirror endpoint with " <> description) $ do
            let outcome = loadFor "rubygems" (rubygemsMirror (codeArtifactDomain <> path))
            outcome `shouldSatisfy` refusalMentions "its path must be /ruby/{repository}/"
            outcome `shouldSatisfy` refusalMentions "mounts.rubygems.mirrorTarget.codeArtifact.url"

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
            other -> expectationFailure ("expected a CodeArtifact control plane, got: " <> show other)

    it "resolves verdaccio to a static credential carrying the declared deletion consent" $ do
        permitted <- backendFor [decl "mirrorTarget" "verdaccio" verdaccioWriteKeys]
        permitted `shouldBe` BackendVerdaccio (mkSecret "write-token") DeletionPermitted
        sbTag permitted `shouldBe` TagVerdaccio
        sbMint permitted `shouldBe` MintStatic (mkSecret "write-token")
        sbControl permitted `shouldBe` ControlProtocol (mkSecret "write-token") DeletionPermitted

    it "withholds deletion consent on a verdaccio store that never declares it" $ do
        backend <- backendFor [decl "mirrorTarget" "verdaccio" [url verdaccioUrl, field "token" "write-token"]]
        backend `shouldBe` BackendVerdaccio (mkSecret "write-token") DeletionWithheld

{- The private cache is a store of its own. The pass that sweeps it resolves its backend from the
private endpoint alone, so the repository it addresses is the one written there. -}
privateBackendSpec :: Spec
privateBackendSpec = describe "the store backend a private cache resolves to" $
    it "resolves codeArtifact to the identity its host carries and the repository it addresses" $ do
        target <- privateTargetFor [decl "privateUpstream" "codeArtifact" [url codeArtifactInternal]]
        case resolvePrivateBackend Npm target of
            Right (backend, store) -> do
                sbTag backend `shouldBe` TagCodeArtifact
                casRepository store `shouldBe` "internal"
                formatToken (casFormat store) `shouldBe` "npm"
                sbControl backend `shouldBe` ControlCodeArtifact store
            Left err -> expectationFailure ("expected the private backend to resolve, got: " <> show (renderConfigError err))

-- The npm mount's resolved private-upstream target, failing the test on a mount without one.
privateTargetFor :: [Text] -> IO Target
privateTargetFor endpoints = do
    config <- expectLoad pubUrlEnv endpoints
    case mntPrivateUpstream =<< Map.lookup Npm (cfgMounts (configApp config)) of
        Just endpoint -> pure (preTarget endpoint)
        Nothing -> fail "the npm mount resolved no private upstream"

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
        -- The domain keeps every hyphen it was written with: only the last one separates
        -- the owner, so a hyphenated domain does not lose its head.
        parseCodeArtifactHost "my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com"
            `shouldBe` Just ("my-domain", "111122223333", "us-west-2")

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

rubygemsMirror :: Text -> [Text]
rubygemsMirror targetUrl = mirrored [decl "mirrorTarget" "codeArtifact" [url targetUrl]]

{- Every tag of both write targets, each with the url and keys it needs to load. A CodeArtifact
path addresses the repository under the mount's own format token. -}
writeTargets :: Text -> [(Text, Text, Text, [Text])]
writeTargets format =
    [ ("mirrorTarget", "registry", mirrorUrl, [field "token" "t"])
    , ("mirrorTarget", "codeArtifact", codeArtifactDomain <> "/" <> format <> "/mirror/", [])
    , ("mirrorTarget", "verdaccio", verdaccioUrl, [field "token" "t"])
    , ("publicationTarget", "registry", publishUrl, [])
    , ("publicationTarget", "codeArtifact", codeArtifactDomain <> "/" <> format <> "/first-party/", [])
    , ("publicationTarget", "verdaccio", verdaccioUrl, [])
    ]

-- One write target writing where it reads, beside the private upstream a mirror target needs.
withUploadUrl :: (Text, Text, Text, [Text]) -> [Text]
withUploadUrl (key, tag, target, keys) = mirrored [decl key tag (url target : field "uploadUrl" target : keys)]

loadFor :: Text -> [Text] -> Either [ConfigError] Config
loadFor eco endpoints = loadConfig pubUrlEnv (Just (mountDocFor eco endpoints))

loadMount :: [Text] -> Either [ConfigError] Config
loadMount = loadFor "npm"

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

renderedRefusals :: Either [ConfigError] a -> [Text]
renderedRefusals = either (map renderConfigError) (const [])

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
expectLoad env endpoints = expectConfig env (Just (npmMountDoc endpoints))

-- The tag-conflict refusals one role's endpoint pass earns, with its other findings dropped.
tagConflicts :: RegistryRole -> Map Ecosystem MountConfig -> [BootError]
tagConflicts role mounts = case snd (runVet role (vetEndpoints mounts)) of
    Left errs -> [err | err@StoreTagConflict{} <- errs]
    Right _ -> []

isConflictAt :: Text -> Text -> BootError -> Bool
isConflictAt key otherKey = \case
    StoreTagConflict _ written _ otherWritten _ -> written == key && otherWritten == otherKey
    _ -> False

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

storePresentationSpec :: Spec
storePresentationSpec = describe "storePresentation" $ do
    it "keeps the protocol's own presentation on registry and verdaccio stores, for every ecosystem" $
        for_ (universe :: [Ecosystem]) $ \eco ->
            map (`storePresentation` eco) [TagRegistry, TagVerdaccio] `shouldBe` [nativePresentation, nativePresentation]

    it "renders a CodeArtifact RubyGems token as Basic aws on reads and Bearer on writes" $ do
        request <- parseRequestOrFail "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/ruby/internal/api/v1/gems"
        let presentation = storePresentation TagCodeArtifact RubyGems
            token = Just (ClientCredential (Just "aws") (mkSecret "key"))
            authorization mapping = lookup "Authorization" (Client.requestHeaders (attachCredential mapping token request))
        authorization (presentedMapping presentation StoreRead rubyReadCredential) `shouldBe` Just "Basic YXdzOmtleQ=="
        authorization (presentedMapping presentation StoreWrite rubyPublishCredential) `shouldBe` Just "Bearer key"
