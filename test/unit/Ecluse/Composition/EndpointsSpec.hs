-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.EndpointsSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Endpoints (endpointCollisions, publicationTargetUrl, vetPublicationTargets)
import Ecluse.Composition.Support (expectConfig, overrideEnv, staticEnvVars)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp), MountConfig)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Security.Egress (registryUrlText)

spec :: Spec
spec = do
    publicUpstreamSpec
    otherMountSpec
    mirrorTargetSpec
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

    it "refuses a publication target on its own mount's public-upstream host" $ do
        -- The path differs, so only the host comparison catches it. A publish carries the
        -- publisher's own credential, which must never reach the public leg.
        collisionsFor (publishingTo "https://public.example.test/npm/")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm]

    it "refuses a publication target on another mount's public-upstream host" $
        collisionsFor (withPyPI (publishingTo "https://pypi-public.example.test/simple/"))
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm PyPI]

    it "compares the host case-insensitively, as URL authority semantics require" $
        collisionsFor (publishingTo "https://PUBLIC.Example.Test")
            `shouldReturn` [PublicationTargetOnPublicUpstream Npm Npm]

    it "produces no witness at all once a collision refuses the boot" $ do
        mounts <- mountsFor (publishingTo "https://public.example.test")
        fmap Map.keys (vetPublicationTargets mounts) `shouldBe` Left [PublicationTargetOnPublicUpstream Npm Npm]

otherMountSpec :: Spec
otherMountSpec = describe "publicationTarget against another mount's endpoints" $ do
    it "refuses a publication target that is another mount's private upstream" $
        collisionsFor (withPyPI (publishingTo "https://pypi-private.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"]

    it "refuses a publication target that is another mount's mirror target" $
        collisionsFor (withPyPI (publishingTo "https://pypi-mirror.example.test"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "mirrorTarget"]

    it "refuses two mounts that publish to one registry" $ do
        -- Each mount relays a different publisher's credential, so one shared publication
        -- target crosses the two tenancies. Both mounts report it.
        let env =
                overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET" "https://shared-publish.example.test" $
                    withPyPI (publishingTo "https://shared-publish.example.test")
        collisions <- collisionsFor env
        collisions
            `shouldMatchList` [ PublicationTargetOnMountEndpoint Npm PyPI "publicationTarget"
                              , PublicationTargetOnMountEndpoint PyPI Npm "publicationTarget"
                              ]

    it "boots a publication target equal to its own mount's private upstream" $
        -- The recommended read-back topology: the publisher writes where the mount reads.
        collisionsFor (publishingTo "https://private.example.test") `shouldReturn` []

    it "boots a publication target equal to its own mount's mirror target" $
        -- The sanctioned degenerate floor, where one registry carries both writes.
        collisionsFor (publishingTo "https://mirror.example.test") `shouldReturn` []

    it "ignores a trailing-slash difference when comparing full URLs" $
        collisionsFor (withPyPI (publishingTo "https://pypi-private.example.test/"))
            `shouldReturn` [PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"]

mirrorTargetSpec :: Spec
mirrorTargetSpec = describe "mirrorTarget against a public upstream" $ do
    it "refuses a mirror target on its own mount's public-upstream host" $
        collisionsFor (overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" "https://public.example.test/npm/" staticEnvVars)
            `shouldReturn` [MirrorTargetOnPublicUpstream Npm Npm]

    it "refuses a mirror target on another mount's public-upstream host" $
        collisionsFor (withPyPI (overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" "https://pypi-public.example.test" staticEnvVars))
            `shouldReturn` [MirrorTargetOnPublicUpstream Npm PyPI]

    it "leaves a mirror target on a registry of its own alone" $
        collisionsFor staticEnvVars `shouldReturn` []

aggregationSpec :: Spec
aggregationSpec = describe "aggregation" $
    it "reports every collision in one boot failure" $ do
        -- One publication target on two public-upstream hosts is impossible, so this
        -- collides the publication target with one mount and the mirror target with another.
        let env =
                overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" "https://pypi-public.example.test" $
                    withPyPI (publishingTo "https://pypi-private.example.test")
        collisions <- collisionsFor env
        collisions
            `shouldMatchList` [ PublicationTargetOnMountEndpoint Npm PyPI "privateUpstream"
                              , MirrorTargetOnPublicUpstream Npm PyPI
                              ]

-- The active mounts an environment layer resolves to: the input every rule reads.
mountsFor :: [(String, String)] -> IO (Map Ecosystem MountConfig)
mountsFor env = cfgMounts . configApp <$> expectConfig env Nothing

-- Every cross-mount refusal an environment layer earns.
collisionsFor :: [(String, String)] -> IO [BootError]
collisionsFor env = endpointCollisions <$> mountsFor env

-- The npm mount publishing to its own registry, the collision-free baseline.
publishingEnv :: [(String, String)]
publishingEnv = publishingTo "https://publish.example.test"

-- The npm mount publishing to the given target, with the allow-list the publish path needs.
publishingTo :: String -> [(String, String)]
publishingTo target =
    overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW" "@acme" $
        overrideEnv "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET" target staticEnvVars

{- | Activate a second, mirrored PyPI mount, so the cross-mount rules have a neighbour to
collide with. No adapter ships for it, which these pure endpoint rules never consult.
-}
withPyPI :: [(String, String)] -> [(String, String)]
withPyPI env =
    overrideEnv "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM" "https://pypi-public.example.test" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM" "https://pypi-private.example.test" $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET" "https://pypi-mirror.example.test" $
                overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET_TOKEN" "pypi-write-token" env
