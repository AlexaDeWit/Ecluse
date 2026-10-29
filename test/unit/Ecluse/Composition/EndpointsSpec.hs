-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Declared coordinates preserve endpoint refusals and each role's store-sharing contract.
module Ecluse.Composition.EndpointsSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (
    Advisory (MirrorTargetOnOwnPublicationTarget, MirrorTargetOnPrivateUpstream),
    BootError (..),
 )
import Ecluse.Composition.Endpoints (
    PublicationTarget,
    VettedEndpoints (vePublicationTargets),
    publicationReadUrl,
    publicationTargetUrl,
    vetEndpoints,
 )
import Ecluse.Composition.Support (expectConfig, overrideEnv, staticEnvVars)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp), MirrorEndpoint (meUploadUrl), MountConfig (..), PublicationEndpoint (peUploadUrl))
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Test.Package (unsafeRegistryUrl)

-- | Check endpoint collisions independently of native transport and discovery.
spec :: Spec
spec = do
    uploadCoordinateSpec
    publicUpstreamSpec
    otherMountSpec
    ownPrivatePublicationSpec
    mirrorTargetSpec
    mirrorStoreSpec
    privateUpstreamSpec
    advisorySpec
    aggregationSpec

uploadCoordinateSpec :: Spec
uploadCoordinateSpec = describe "configured Cargo read and upload coordinates" $ do
    for_ [("privateUpstream", "https://pypi-private.example.test"), ("mirrorTarget", "https://pypi-mirror.example.test"), ("publicationTarget", "https://pypi-publish.example.test")] $ \(key, url) ->
        it ("refuses a publication API on another mount's " <> toString key) $ do
            mounts <- cargoMounts
            let changed = withPublicationUpload Cargo url mounts
                expected = case key of
                    "publicationTarget" ->
                        [ PublicationTargetOnMountEndpoint PyPI Cargo "publicationTarget" url
                        , PublicationTargetOnMountEndpoint Cargo PyPI "publicationTarget" url
                        ]
                    _ -> [PublicationTargetOnMountEndpoint Cargo PyPI key url]
            for_ endpointRoles $ \role ->
                refusalsIn role changed `shouldBe` expected

    it "checks an upload coordinate loaded through the generic registry configuration" $
        refusalsFor
            MirrorWriter
            (overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL" "https://pypi-private.example.test" (withPyPI publishingEnv))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"]

    it "compares publication APIs when both mounts have distinct read roots" $ do
        mounts <- cargoMounts
        let shared = "https://shared-api.example.test/registry"
            changed = withPublicationUpload Cargo shared (withPublicationUpload PyPI shared mounts)
        refusalsIn MirrorWriter changed
            `shouldMatchList` [ PublicationTargetOnMountEndpoint Cargo PyPI "publicationTarget" shared
                              , PublicationTargetOnMountEndpoint PyPI Cargo "publicationTarget" shared
                              ]

    it "compares a publication API with another mount's mirror API" $ do
        mounts <- cargoMounts
        let shared = "https://shared-api.example.test/registry"
            changed = withPublicationUpload Cargo shared (withMirrorUpload PyPI shared mounts)
        for_ endpointRoles $ \role ->
            refusalsIn role changed `shouldBe` [PublicationTargetOnMountEndpoint Cargo PyPI "mirrorTarget" shared]

    it "refuses publication and mirror APIs on public hosts across mounts" $ do
        mounts <- cargoMounts
        let url = "https://pypi-public.example.test/api"
        for_ endpointRoles $ \role -> do
            refusalsIn role (withPublicationUpload Cargo url mounts) `shouldBe` [PublicationTargetOnPublicUpstream Cargo PyPI url]
            refusalsIn role (withMirrorUpload Cargo url mounts) `shouldBe` [MirrorTargetOnPublicUpstream Cargo PyPI url]

    it "refuses publication and mirror APIs on their own mount's public host, whatever the port" $ do
        mounts <- cargoMounts
        let url = "https://public.example.test:8443/api"
        for_ endpointRoles $ \role -> do
            refusalsIn role (withPublicationUpload Cargo url mounts) `shouldBe` [PublicationTargetOnPublicUpstream Cargo Cargo url]
            refusalsIn role (withMirrorUpload Cargo url mounts) `shouldBe` [MirrorTargetOnPublicUpstream Cargo Cargo url]

    it "keeps the mirror API's private-store collapse advisory specific to the writing role" $ do
        mounts <- cargoMounts
        let url = "https://pypi-private.example.test"
            changed = withMirrorUpload Cargo url mounts
        refusalsIn MirrorWriter changed `shouldBe` []
        advisoriesIn MirrorWriter changed `shouldBe` [MirrorTargetOnPrivateUpstream Cargo PyPI (unsafeRegistryUrl url)]
        for_ [MirrorPruner, MirrorPreviewer] $ \role -> do
            refusalsIn role changed `shouldBe` [MirrorTargetOnMountEndpoint Cargo PyPI "privateUpstream" url]
            advisoriesIn role changed `shouldBe` []

    it "refuses a mirror API on another mount's publication target" $ do
        mounts <- cargoMounts
        let url = "https://pypi-publish.example.test"
            changed = withMirrorUpload Cargo url mounts
        for_ endpointRoles $ \role ->
            refusalsIn role changed `shouldBe` [PublicationTargetOnMountEndpoint PyPI Cargo "mirrorTarget" url]

    it "preserves publication API sharing with its own private store for writing roles" $ do
        mounts <- cargoMounts
        let url = "https://private.example.test"
            changed = withPublicationUpload Cargo url mounts
        refusalsIn MirrorWriter changed `shouldBe` []
        advisoriesIn MirrorWriter changed `shouldBe` []
        for_ [MirrorPruner, MirrorPreviewer] $ \role ->
            refusalsIn role changed `shouldBe` [PublicationTargetOnMountEndpoint Cargo Cargo "privateUpstream" url]

    it "preserves own mirror and publication API collapse without a blanket refusal" $ do
        mounts <- cargoMounts
        let url = "https://shared-api.example.test/registry"
            changed = withPublicationUpload Cargo url (withMirrorUpload Cargo url mounts)
        refusalsIn MirrorWriter changed `shouldBe` []
        advisoriesIn MirrorWriter changed `shouldBe` [MirrorTargetOnOwnPublicationTarget Cargo (unsafeRegistryUrl url)]
        vetted <- either (fail . show) pure (clearedTargets changed)
        fmap (registryUrlText . publicationTargetUrl) (Map.lookup Cargo vetted) `shouldBe` Just url
        fmap (registryUrlText . publicationReadUrl) (Map.lookup Cargo vetted) `shouldBe` Just "https://publish.example.test"
        for_ [MirrorPruner, MirrorPreviewer] $ \role ->
            refusalsIn role changed `shouldBe` [MirrorTargetOnMountEndpoint Cargo Cargo "publicationTarget" url]

    it "clears distinct read and API paths on their configured hosts" $ do
        mounts <- cargoMounts
        let api = "https://publish.example.test/api"
            changed = withPublicationUpload Cargo api (withMirrorUpload Cargo "https://mirror.example.test/api" mounts)
        for_ endpointRoles $ \role -> do
            refusalsIn role changed `shouldBe` []
            advisoriesIn role changed `shouldBe` []
        vetted <- either (fail . show) pure (clearedTargets changed)
        fmap (registryUrlText . publicationTargetUrl) (Map.lookup Cargo vetted) `shouldBe` Just api
        fmap (registryUrlText . publicationReadUrl) (Map.lookup Cargo vetted) `shouldBe` Just "https://publish.example.test"

    it "reports an equivalent read and API coordinate once" $ do
        mounts <- cargoMounts
        let url = "https://publish.example.test"
            changed = withPublicationUpload Cargo "https://PUBLISH.example.test:443/" (withMirrorUpload Cargo url mounts)
        refusalsIn MirrorWriter changed `shouldBe` []
        advisoriesIn MirrorWriter changed `shouldBe` [MirrorTargetOnOwnPublicationTarget Cargo (unsafeRegistryUrl url)]

endpointRoles :: [RegistryRole]
endpointRoles = [MirrorWriter, MirrorPruner, MirrorPreviewer]

-- Endpoint rules use declared roles without interpreting first-party package names.
cargoMounts :: IO (Map Ecosystem MountConfig)
cargoMounts = do
    mounts <- mountsFor (pypiPublishingTo "https://pypi-publish.example.test" publishingEnv)
    cargo <- maybe (fail "missing registry fixture") pure (Map.lookup Npm mounts)
    pure (Map.insert Cargo cargo (Map.delete Npm mounts))

withPublicationUpload :: Ecosystem -> Text -> Map Ecosystem MountConfig -> Map Ecosystem MountConfig
withPublicationUpload eco url = Map.adjust (\mount -> mount{mntPublicationTarget = fmap (\endpoint -> endpoint{peUploadUrl = Just (unsafeRegistryUrl url)}) (mntPublicationTarget mount)}) eco

withMirrorUpload :: Ecosystem -> Text -> Map Ecosystem MountConfig -> Map Ecosystem MountConfig
withMirrorUpload eco url = Map.adjust (\mount -> mount{mntMirrorTarget = fmap (\endpoint -> endpoint{meUploadUrl = Just (unsafeRegistryUrl url)}) (mntMirrorTarget mount)}) eco

publicUpstreamSpec :: Spec
publicUpstreamSpec = describe "publicationTarget against a public upstream" $ do
    it "vets a publication target that shares no host and no URL with another role" $ do
        mounts <- mountsFor publishingEnv
        case clearedTargets mounts of
            Left errs -> expectationFailure ("unexpected collisions: " <> show errs)
            Right vetted ->
                fmap (registryUrlText . publicationTargetUrl) (Map.lookup Npm vetted)
                    `shouldBe` Just "https://publish.example.test"

    it "refuses a publication target on its own mount's public-upstream host" $
        -- The path differs, so only the host comparison catches it. A publish carries the
        -- publisher's own credential, which must never reach the public leg.
        refusalsFor MirrorWriter (publishingTo "https://public.example.test/npm/")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm "https://public.example.test/npm/"]

    it "refuses a publication target on another mount's public-upstream host" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-public.example.test"))
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm PyPI "https://pypi-public.example.test"]

    it "compares the host case-insensitively, as URL authority semantics require" $
        refusalsFor MirrorWriter (publishingTo "https://PUBLIC.Example.Test")
            -- The refusal quotes the value as configured, so an operator finds the key by search.
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm "https://PUBLIC.Example.Test"]

    it "refuses the deleting role that same publication target, which no role may relay" $
        refusalsFor MirrorPruner (publishingTo "https://public.example.test/npm/")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm "https://public.example.test/npm/"]

    it "produces no witness at all once a collision refuses the boot" $ do
        mounts <- mountsFor (publishingTo "https://public.example.test")
        fmap Map.keys (clearedTargets mounts)
            `shouldBe` Left [PublicationTargetOnPublicUpstream Npm Npm "https://public.example.test"]

otherMountSpec :: Spec
otherMountSpec = describe "publicationTarget against another mount's endpoints" $ do
    it "refuses a publication target that is another mount's private upstream" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-private.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"]

    it "refuses a publication target that is another mount's mirror target" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-mirror.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "mirrorTarget" "https://pypi-mirror.example.test"]

    it "refuses two mounts that publish to one registry" $ do
        -- Each mount relays a different publisher's credential, so one shared publication
        -- target crosses the two tenancies. Both mounts report it.
        let env =
                overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL" "https://shared-publish.example.test" $
                    withPyPI (publishingTo "https://shared-publish.example.test")
        refusals <- refusalsFor MirrorWriter env
        refusals
            `shouldBe` [ PublicationTargetOnMountEndpoint Npm PyPI "publicationTarget" "https://shared-publish.example.test"
                       , PublicationTargetOnMountEndpoint PyPI Npm "publicationTarget" "https://shared-publish.example.test"
                       ]

    it "ignores a trailing-slash difference when comparing full URLs" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-private.example.test/"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test/"]

ownPrivatePublicationSpec :: Spec
ownPrivatePublicationSpec = describe "publicationTarget against its own private upstream" $
    forM_ [MirrorWriter, MirrorPruner, MirrorPreviewer] $ \role -> describe (show role) $ do
        forM_
            [ "https://private.example.test"
            , "https://private.example.test/"
            , "https://PRIVATE.Example.Test"
            , "https://private.example.test:443"
            , "https://PRIVATE.Example.Test:443/"
            ]
            $ \url -> it ("compares the same registry spelled " <> url) $ do
                let env = publishingTo url
                    expected = case role of
                        MirrorWriter -> []
                        MirrorPruner -> [PublicationTargetOnMountEndpoint Npm Npm "privateUpstream" (toText url)]
                        MirrorPreviewer -> [PublicationTargetOnMountEndpoint Npm Npm "privateUpstream" (toText url)]
                refusalsFor role env `shouldReturn` expected
                advisoriesForRole role env `shouldReturn` []

        it "accepts distinct repository paths on the same host" $ do
            let env =
                    overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" "https://store.example.test/npm/cache/" $
                        publishingTo "https://store.example.test/npm/publish/"
            refusalsFor role env `shouldReturn` []

        it "accepts a distinct non-default port" $
            refusalsFor role (publishingTo "https://private.example.test:8443") `shouldReturn` []

        it "accepts an absent publication target" $
            refusalsFor role staticEnvVars `shouldReturn` []

        it "preserves a cross-mount publication refusal" $
            refusalsFor role (withPyPI (publishingTo "https://pypi-private.example.test"))
                `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"]

mirrorTargetSpec :: Spec
mirrorTargetSpec = describe "mirrorTarget against a public upstream" $ do
    it "refuses every role a mirror target on its own mount's public-upstream host" $ do
        let env = mirroringTo mirrorOnPublicUrl staticEnvVars
        refusalsFor MirrorWriter env `shouldReturn` [MirrorTargetOnPublicUpstream Npm Npm mirrorOnPublicUrl]
        refusalsFor MirrorPruner env `shouldReturn` [MirrorTargetOnPublicUpstream Npm Npm mirrorOnPublicUrl]

    it "refuses a mirror target on another mount's public-upstream host" $
        refusalsFor MirrorWriter (withPyPI (mirroringTo "https://pypi-public.example.test" staticEnvVars))
            `shouldReturn` [MirrorTargetOnPublicUpstream Npm PyPI "https://pypi-public.example.test"]

    it "leaves a mirror target on a registry of its own alone" $
        refusalsFor MirrorWriter staticEnvVars `shouldReturn` []

mirrorStoreSpec :: Spec
mirrorStoreSpec = describe "mirrorTarget against another declared endpoint" $ do
    it "refuses the deleting role a mirror target on its own mount's private upstream" $ do
        let env = mirroringTo "https://private.example.test" staticEnvVars
        refusalsFor MirrorPruner env
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm Npm "privateUpstream" "https://private.example.test"]
        refusalsFor MirrorWriter env `shouldReturn` []

    it "refuses the deleting role a mirror target on its own mount's publication target" $ do
        let env = publishingTo "https://mirror.example.test"
        refusalsFor MirrorPruner env
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm Npm "publicationTarget" "https://mirror.example.test"]
        refusalsFor MirrorWriter env `shouldReturn` []

    it "refuses the deleting role a mirror target on another mount's private upstream" $ do
        -- The collapse that can destroy first-party data: the sweep would delete versions
        -- the neighbouring mount serves as already vetted.
        let env = withPyPI (mirroringTo "https://pypi-private.example.test" staticEnvVars)
        refusalsFor MirrorPruner env
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"]
        refusalsFor MirrorWriter env `shouldReturn` []

    it "refuses every role a mirror target on another mount's publication target" $ do
        -- The publication side reports this collision once.
        let env = pypiPublishingTo "https://mirror.example.test" staticEnvVars
        refusalsFor MirrorWriter env `shouldReturn` [PublicationTargetOnMountEndpoint PyPI Npm "mirrorTarget" "https://mirror.example.test"]
        refusalsFor MirrorPruner env `shouldReturn` [PublicationTargetOnMountEndpoint PyPI Npm "mirrorTarget" "https://mirror.example.test"]

    it "treats two format endpoints of one repository as distinct stores" $ do
        -- A repository's per-format endpoints share an authority and differ by path, and deletion
        -- is format-scoped, so the path stays outside the fold that the authority goes through.
        let env = mirrorAgainstPypiPrivate "https://store.example.test/npm/mirror/" "https://store.example.test/pypi/mirror/"
        refusalsFor MirrorPruner env `shouldReturn` []
        advisoriesFor env `shouldReturn` []

    it "matches one store written with a different authority case" $ do
        -- DNS and TLS resolve one store here. Compared as raw text it reads as two, and the
        -- sweep would delete the neighbouring mount's first-party read path.
        let env = mirrorAgainstPypiPrivate "https://Store.example.test/npm/mirror/" "https://store.example.test/npm/mirror/"
        refusalsFor MirrorPruner env
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://Store.example.test/npm/mirror/"]

    it "matches an explicit default port against a portless endpoint" $ do
        let env = mirrorAgainstPypiPrivate "https://store.example.test:443/npm/mirror/" "https://store.example.test/npm/mirror/"
        refusalsFor MirrorPruner env
            `shouldReturn` [MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://store.example.test:443/npm/mirror/"]

    it "keeps a non-default port a distinct store" $ do
        let env = mirrorAgainstPypiPrivate "https://store.example.test:8443/npm/mirror/" "https://store.example.test/npm/mirror/"
        refusalsFor MirrorPruner env `shouldReturn` []
        advisoriesFor env `shouldReturn` []

privateUpstreamSpec :: Spec
privateUpstreamSpec = describe "privateUpstream against its public upstream" $
    forM_ [MirrorWriter, MirrorPruner] $ \role -> describe (show role) $ do
        forM_
            [ "https://public.example.test"
            , "https://public.example.test/"
            , "https://PUBLIC.Example.Test"
            , "https://public.example.test:443"
            , "https://PUBLIC.Example.Test:443/"
            ]
            $ \url -> it ("refuses the same repository spelled " <> url) $ do
                let env = overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" url staticEnvVars
                refusalsFor role env
                    `shouldReturn` [PrivateUpstreamOnPublicUpstream Npm (toText url)]
                advisoriesForRole role env `shouldReturn` []

        it "accepts distinct repository paths on the same host" $ do
            let env =
                    overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" "https://public.example.test/npm/private/" $
                        overrideEnv "ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL" "https://public.example.test/npm/public/" staticEnvVars
            refusalsFor role env `shouldReturn` []
            advisoriesForRole role env `shouldReturn` []

        it "preserves a private upstream equal to another mount's public upstream" $ do
            let env =
                    overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" "https://pypi-public.example.test" (withPyPI staticEnvVars)
            refusalsFor role env `shouldReturn` []
            advisoriesForRole role env `shouldReturn` []

        it "accepts a distinct non-default port on the same host" $ do
            let env = overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" "https://public.example.test:8443" staticEnvVars
            refusalsFor role env `shouldReturn` []
            advisoriesForRole role env `shouldReturn` []

advisorySpec :: Spec
advisorySpec = describe "the advisories a writing role logs" $ do
    it "says nothing when every registry endpoint is distinct" $
        advisoriesFor staticEnvVars `shouldReturn` []

    it "warns on a mirror target equal to its own mount's private upstream" $
        advisoriesFor (mirroringTo "https://private.example.test" staticEnvVars)
            `shouldReturn` [MirrorTargetOnPrivateUpstream Npm Npm (unsafeRegistryUrl "https://private.example.test")]

    it "warns on a mirror target equal to its own mount's publication target" $
        advisoriesFor (publishingTo "https://mirror.example.test")
            `shouldReturn` [MirrorTargetOnOwnPublicationTarget Npm (unsafeRegistryUrl "https://mirror.example.test")]

    it "warns once on a mirror target equal to another mount's private upstream" $
        advisoriesFor (withPyPI (mirroringTo "https://pypi-private.example.test" staticEnvVars))
            `shouldReturn` [MirrorTargetOnPrivateUpstream Npm PyPI (unsafeRegistryUrl "https://pypi-private.example.test")]

    it "ignores a trailing-slash difference when comparing endpoints" $
        -- The comparison folds the slash away, and the advisory still carries the value as
        -- configured, so an operator finds the key by search.
        advisoriesFor (mirroringTo "https://private.example.test/" staticEnvVars)
            `shouldReturn` [MirrorTargetOnPrivateUpstream Npm Npm (unsafeRegistryUrl "https://private.example.test/")]

    it "logs no advisory for a collapse it refuses outright" $
        advisoriesFor (withPyPI (publishingTo "https://pypi-mirror.example.test")) `shouldReturn` []

aggregationSpec :: Spec
aggregationSpec = describe "aggregation" $ do
    it "accumulates a same-mount publication refusal before mirror refusals" $
        forM_ [MirrorPruner, MirrorPreviewer] $ \role -> do
            let env = mirroringTo "https://public.example.test/npm/" (publishingTo "https://private.example.test")
            refusalsFor role env
                `shouldReturn` [ PublicationTargetOnMountEndpoint Npm Npm "privateUpstream" "https://private.example.test"
                               , MirrorTargetOnPublicUpstream Npm Npm "https://public.example.test/npm/"
                               ]

    it "reports every refusing rule in the order the pass declares them" $ do
        refusals <- refusalsFor MirrorPruner everyEndpointCollapseEnv
        refusals
            `shouldBe` [ PublicationTargetOnPublicUpstream Npm PyPI sharedRegistryText
                       , PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" sharedRegistryText
                       , MirrorTargetOnPublicUpstream Npm PyPI sharedRegistryText
                       , MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" sharedRegistryText
                       , MirrorTargetOnMountEndpoint Npm Npm "publicationTarget" sharedRegistryText
                       , PrivateUpstreamOnPublicUpstream PyPI sharedRegistryText
                       ]

    it "logs the advising rules of that same configuration in that same order" $
        -- The mirror collisions still advise a writing role when another rule refuses.
        advisoriesFor everyEndpointCollapseEnv
            `shouldReturn` [ MirrorTargetOnPrivateUpstream Npm PyPI sharedRegistryUrl
                           , MirrorTargetOnOwnPublicationTarget Npm sharedRegistryUrl
                           ]

    it "reports every collision in one boot failure, in rule order" $ do
        -- One publication target on two public-upstream hosts is impossible, so this
        -- collides the publication target with one mount and the mirror target with another.
        let env =
                mirroringTo "https://pypi-public.example.test" $
                    withPyPI (publishingTo "https://pypi-private.example.test")
        refusals <- refusalsFor MirrorWriter env
        refusals
            `shouldBe` [ PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"
                       , MirrorTargetOnPublicUpstream Npm PyPI "https://pypi-public.example.test"
                       ]

-- The active mounts an environment layer resolves to: the input every rule reads.
mountsFor :: [(String, String)] -> IO (Map Ecosystem MountConfig)
mountsFor env = cfgMounts . configApp <$> expectConfig env Nothing

-- Every refusal a role earns from an environment layer.
refusalsFor :: RegistryRole -> [(String, String)] -> IO [BootError]
refusalsFor role env = refusalsIn role <$> mountsFor env

refusalsIn :: RegistryRole -> Map Ecosystem MountConfig -> [BootError]
refusalsIn role = fromLeft [] . snd . runVet role . vetEndpoints

-- Every advisory a writing role (@ecluse proxy@ and @ecluse mirror@ alike) logs.
advisoriesFor :: [(String, String)] -> IO [Advisory]
advisoriesFor = advisoriesForRole MirrorWriter

-- Every advisory one role logs, whatever that role's pass decided.
advisoriesForRole :: RegistryRole -> [(String, String)] -> IO [Advisory]
advisoriesForRole role env = advisoriesIn role <$> mountsFor env

advisoriesIn :: RegistryRole -> Map Ecosystem MountConfig -> [Advisory]
advisoriesIn role = fst . runVet role . vetEndpoints

-- The publish endpoints a writing role's pass clears, or every refusal at once.
clearedTargets :: Map Ecosystem MountConfig -> Either [BootError] (Map Ecosystem PublicationTarget)
clearedTargets mounts = vePublicationTargets <$> snd (runVet MirrorWriter (vetEndpoints mounts))

mirrorOnPublicUrl :: (IsString s) => s
mirrorOnPublicUrl = "https://public.example.test/npm/"

-- The npm mount publishing to its own registry, the collision-free baseline.
publishingEnv :: [(String, String)]
publishingEnv = publishingTo "https://publish.example.test"

-- The npm mount publishing to the given target, with the namespaces the publish path needs.
publishingTo :: String -> [(String, String)]
publishingTo target =
    overrideEnv "ECLUSE_MOUNTS__NPM__FIRST_PARTY" "@acme" $
        overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL" target staticEnvVars

-- The npm mount mirroring to the given target.
mirroringTo :: String -> [(String, String)] -> [(String, String)]
mirroringTo = overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL"

-- These endpoint checks do not consult the PyPI adapter's write capabilities.
withPyPI :: [(String, String)] -> [(String, String)]
withPyPI env =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL" "https://pypi-public.example.test" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" "https://pypi-private.example.test" $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL" "https://pypi-mirror.example.test" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__TOKEN" "pypi-write-token" env

mirrorAgainstPypiPrivate :: String -> String -> [(String, String)]
mirrorAgainstPypiPrivate mirror private =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" private (withPyPI (mirroringTo mirror staticEnvVars))

everyEndpointCollapseEnv :: [(String, String)]
everyEndpointCollapseEnv =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL" sharedRegistry $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" sharedRegistry $
            withPyPI (mirroringTo sharedRegistry (publishingTo sharedRegistry))

sharedRegistry :: String
sharedRegistry = "https://shared.example.test"

sharedRegistryText :: Text
sharedRegistryText = toText sharedRegistry

sharedRegistryUrl :: RegistryUrl
sharedRegistryUrl = unsafeRegistryUrl sharedRegistryText

-- The PyPI neighbour publishing to the given target. The endpoint rules read no namespaces.
pypiPublishingTo :: String -> [(String, String)] -> [(String, String)]
pypiPublishingTo target env =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL" target (withPyPI env)
