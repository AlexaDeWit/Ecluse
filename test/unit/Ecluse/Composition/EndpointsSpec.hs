-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.EndpointsSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Endpoints (
    endpointAdvisories,
    endpointRefusals,
    mirrorStoreUrl,
    publicationTargetUrl,
    vetMirrorStores,
    vetPublicationTargets,
 )
import Ecluse.Composition.Support (expectConfig, overrideEnv, staticEnvVars)
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp), MountConfig)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Security.Egress (registryUrlText)

spec :: Spec
spec = do
    publicUpstreamSpec
    otherMountSpec
    mirrorTargetSpec
    mirrorStoreSpec
    advisorySpec
    aggregationSpec

publicUpstreamSpec :: Spec
publicUpstreamSpec = describe "publicationTarget against a public upstream" $ do
    it "vets a publication target that shares no host and no URL with another role" $ do
        mounts <- mountsFor publishingEnv
        case vetPublicationTargets mounts of
            Left errs -> expectationFailure ("unexpected collisions: " <> show errs)
            Right vetted ->
                fmap (registryUrlText . publicationTargetUrl) (Map.lookup Npm vetted)
                    `shouldBe` Just "https://publish.example.test"

    it "refuses a publication target on its own mount's public-upstream host" $
        -- The path differs, so only the host comparison catches it. A publish carries the
        -- publisher's own credential, which must never reach the public leg.
        refusalsFor MirrorWriter (publishingTo "https://public.example.test/npm/")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm]

    it "refuses a publication target on another mount's public-upstream host" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-public.example.test"))
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm PyPI]

    it "compares the host case-insensitively, as URL authority semantics require" $
        refusalsFor MirrorWriter (publishingTo "https://PUBLIC.Example.Test")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm]

    it "produces no witness at all once a collision refuses the boot" $ do
        mounts <- mountsFor (publishingTo "https://public.example.test")
        fmap Map.keys (vetPublicationTargets mounts) `shouldBe` Left [PublicationTargetOnPublicUpstream Npm Npm]

otherMountSpec :: Spec
otherMountSpec = describe "publicationTarget against another mount's endpoints" $ do
    it "refuses a publication target that is another mount's private upstream" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-private.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"]

    it "refuses a publication target that is another mount's mirror target" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-mirror.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "mirrorTarget"]

    it "refuses two mounts that publish to one registry" $ do
        -- Each mount relays a different publisher's credential, so one shared publication
        -- target crosses the two tenancies. Both mounts report it.
        let env =
                overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET" "https://shared-publish.example.test" $
                    withPyPI (publishingTo "https://shared-publish.example.test")
        refusals <- refusalsFor MirrorWriter env
        refusals
            `shouldMatchList` [ PublicationTargetOnMountEndpoint Npm PyPI "publicationTarget"
                              , PublicationTargetOnMountEndpoint PyPI Npm "publicationTarget"
                              ]

    it "boots a publication target equal to its own mount's private upstream" $ do
        -- The recommended read-back topology: the publisher writes where the mount reads.
        let env = publishingTo "https://private.example.test"
        refusalsFor MirrorWriter env `shouldReturn` []
        advisoriesFor env `shouldReturn` []

    it "ignores a trailing-slash difference when comparing full URLs" $
        refusalsFor MirrorWriter (withPyPI (publishingTo "https://pypi-private.example.test/"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"]

mirrorTargetSpec :: Spec
mirrorTargetSpec = describe "mirrorTarget against a public upstream" $ do
    it "refuses every role a mirror target on its own mount's public-upstream host" $ do
        let env = mirroringTo "https://public.example.test/npm/" staticEnvVars
        refusalsFor MirrorWriter env `shouldReturn` [MirrorTargetOnPublicUpstream Npm Npm]
        refusalsFor MirrorPruner env `shouldReturn` [MirrorTargetOnPublicUpstream Npm Npm]

    it "refuses a mirror target on another mount's public-upstream host" $
        refusalsFor MirrorWriter (withPyPI (mirroringTo "https://pypi-public.example.test" staticEnvVars))
            `shouldReturn` [MirrorTargetOnPublicUpstream Npm PyPI]

    it "leaves a mirror target on a registry of its own alone" $
        refusalsFor MirrorWriter staticEnvVars `shouldReturn` []

mirrorStoreSpec :: Spec
mirrorStoreSpec = describe "mirrorTarget against another declared endpoint" $ do
    it "vets a mirror store no other endpoint holds" $ do
        mounts <- mountsFor staticEnvVars
        case vetMirrorStores mounts of
            Left errs -> expectationFailure ("unexpected refusals: " <> show errs)
            Right vetted ->
                fmap (registryUrlText . mirrorStoreUrl) (Map.lookup Npm vetted)
                    `shouldBe` Just "https://mirror.example.test"

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

    it "produces no mirror-store witness once a collapse refuses the deleting role" $ do
        mounts <- mountsFor (withPyPI (mirroringTo "https://pypi-private.example.test" staticEnvVars))
        fmap Map.keys (vetMirrorStores mounts)
            `shouldBe` Left [MirrorTargetOnMountEndpoint Npm PyPI "privateUpstream" "https://pypi-private.example.test"]

    it "refuses every role a mirror target on another mount's publication target" $ do
        -- Already fatal before this rule existed, read from the publishing side, and it stays
        -- one refusal rather than one per direction.
        let env = pypiPublishingTo "https://mirror.example.test" staticEnvVars
        refusalsFor MirrorWriter env `shouldReturn` [PublicationTargetOnMountEndpoint PyPI Npm "mirrorTarget"]
        refusalsFor MirrorPruner env `shouldReturn` [PublicationTargetOnMountEndpoint PyPI Npm "mirrorTarget"]

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
        -- The fold applies the default port, it does not drop the port. A host comparison would
        -- drop it and trade this slice's fail-open for another one.
        let env = mirrorAgainstPypiPrivate "https://store.example.test:8443/npm/mirror/" "https://store.example.test/npm/mirror/"
        refusalsFor MirrorPruner env `shouldReturn` []
        advisoriesFor env `shouldReturn` []

advisorySpec :: Spec
advisorySpec = describe "the advisories a writing role logs" $ do
    it "says nothing when every registry endpoint is distinct" $
        advisoriesFor staticEnvVars `shouldReturn` []

    it "warns on a mirror target equal to its own mount's private upstream" $
        advisoriesFor (mirroringTo "https://private.example.test" staticEnvVars)
            `shouldReturn` ["mount \"npm\": mirrorTarget and privateUpstream resolve to the same registry (https://private.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"]

    it "warns on a mirror target equal to its own mount's publication target" $
        advisoriesFor (publishingTo "https://mirror.example.test")
            `shouldReturn` ["mount \"npm\": mirrorTarget and publicationTarget resolve to the same registry (https://mirror.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"]

    it "warns once on a mirror target equal to another mount's private upstream" $
        advisoriesFor (withPyPI (mirroringTo "https://pypi-private.example.test" staticEnvVars))
            `shouldReturn` ["mount \"npm\": mirrorTarget and mount \"pypi\" privateUpstream resolve to the same registry (https://pypi-private.example.test); the Dredger refuses this configuration, so pruning this mirror stays manual"]

    it "warns when a mount's private and public upstreams collide" $
        advisoriesFor (overrideEnv "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM" "https://public.example.test" staticEnvVars)
            `shouldReturn` ["mount \"npm\": privateUpstream and publicUpstream resolve to the same registry (https://public.example.test); the merge trusts the private leg, so this registry's versions are admitted unfiltered"]

    it "ignores a trailing-slash difference when comparing endpoints" $
        advisoriesFor (mirroringTo "https://private.example.test/" staticEnvVars)
            `shouldReturn` ["mount \"npm\": mirrorTarget and privateUpstream resolve to the same registry (https://private.example.test/); the Dredger refuses this configuration, so pruning this mirror stays manual"]

    it "logs no advisory for a collapse it refuses outright" $
        advisoriesFor (withPyPI (publishingTo "https://pypi-mirror.example.test")) `shouldReturn` []

aggregationSpec :: Spec
aggregationSpec = describe "aggregation" $
    it "reports every collision in one boot failure" $ do
        -- One publication target on two public-upstream hosts is impossible, so this
        -- collides the publication target with one mount and the mirror target with another.
        let env =
                mirroringTo "https://pypi-public.example.test" $
                    withPyPI (publishingTo "https://pypi-private.example.test")
        refusals <- refusalsFor MirrorWriter env
        refusals
            `shouldMatchList` [ PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"
                              , MirrorTargetOnPublicUpstream Npm PyPI
                              ]

-- The active mounts an environment layer resolves to: the input every rule reads.
mountsFor :: [(String, String)] -> IO (Map Ecosystem MountConfig)
mountsFor env = cfgMounts . configApp <$> expectConfig env Nothing

-- Every refusal a role earns from an environment layer.
refusalsFor :: RegistryRole -> [(String, String)] -> IO [BootError]
refusalsFor role env = endpointRefusals role <$> mountsFor env

-- Every advisory a writing role (@ecluse proxy@ and @ecluse mirror@ alike) logs.
advisoriesFor :: [(String, String)] -> IO [Text]
advisoriesFor env = endpointAdvisories MirrorWriter <$> mountsFor env

-- The npm mount publishing to its own registry, the collision-free baseline.
publishingEnv :: [(String, String)]
publishingEnv = publishingTo "https://publish.example.test"

-- The npm mount publishing to the given target, with the allow-list the publish path needs.
publishingTo :: String -> [(String, String)]
publishingTo target =
    overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW" "@acme" $
        overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET" target staticEnvVars

-- The npm mount mirroring to the given target.
mirroringTo :: String -> [(String, String)] -> [(String, String)]
mirroringTo = overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"

{- | Activate a second, mirrored PyPI mount, so the cross-mount rules have a neighbour to
collide with. No adapter ships for it, which these pure endpoint rules never consult.
-}
withPyPI :: [(String, String)] -> [(String, String)]
withPyPI env =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM" "https://pypi-public.example.test" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM" "https://pypi-private.example.test" $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET" "https://pypi-mirror.example.test" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET_TOKEN" "pypi-write-token" env

{- | The npm mount mirroring to one URL against a PyPI neighbour reading its private upstream
from another: the pair the store comparison decides.
-}
mirrorAgainstPypiPrivate :: String -> String -> [(String, String)]
mirrorAgainstPypiPrivate mirror private =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM" private (withPyPI (mirroringTo mirror staticEnvVars))

-- The PyPI neighbour publishing to the given target. The endpoint rules read no allow-list.
pypiPublishingTo :: String -> [(String, String)] -> [(String, String)]
pypiPublishingTo target env =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET" target (withPyPI env)
