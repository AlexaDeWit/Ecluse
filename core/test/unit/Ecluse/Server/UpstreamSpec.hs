-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.UpstreamSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security (TarballHostGate, tarballHostGate)
import Ecluse.Core.Server.Upstream (
    MirrorServePlan (MirrorOnAdmit, NoMirrorWrite),
    MountUpstreams,
    mountUpstreams,
    upstreamMirror,
    upstreamPrivateBaseUrl,
    upstreamPublicBaseUrl,
    upstreamTarballHostGate,
 )

{- | Pin the invariant 'MountUpstreams' exists to hold: the tarball-host gate a bound value
carries is the gate of the upstreams that same value reports, so the serve path's SSRF check
runs against the mount's actual upstreams. Divergence is unwritable, because the constructor
is private and no record selector escapes "Ecluse.Core.Server.Upstream", so a stale pair is a
compile error rather than a failing case here.
-}
spec :: Spec
spec = describe "MountUpstreams (a mount's upstreams and their derived gate)" $ do
    it "reports back the upstreams it was bound with" $
        map (\u -> (upstreamPrivateBaseUrl u, upstreamPublicBaseUrl u, upstreamMirror u)) bound
            `shouldBe` map (\(_, private, public, mirror) -> (private, public, mirror)) shapes

    it "carries the gate derived from the upstreams it reports" $
        map upstreamTarballHostGate bound
            `shouldBe` zipWith (\(ecosystemHosts, _, _, _) -> gateOf ecosystemHosts) shapes bound

    it "moves the gate with a rebound upstream: the old authorities cannot be kept" $ do
        let original = mountUpstreams [] Nothing "https://public.one" NoMirrorWrite
            rebound = mountUpstreams [] Nothing "https://public.two" NoMirrorWrite
        upstreamTarballHostGate rebound `shouldNotBe` upstreamTarballHostGate original
        upstreamTarballHostGate rebound `shouldBe` gateOf [] rebound
  where
    bound :: [MountUpstreams]
    bound = map (\(ecosystemHosts, private, public, mirror) -> mountUpstreams ecosystemHosts private public mirror) shapes

    -- The gate these upstreams should carry, derived from what the value reports rather than
    -- the arguments it was built with. An accessor that disagreed with the stored gate fails here.
    gateOf :: [Text] -> MountUpstreams -> TarballHostGate
    gateOf ecosystemHosts u =
        tarballHostGate
            ecosystemHosts
            (upstreamPrivateBaseUrl u)
            (upstreamPublicBaseUrl u)
            (case upstreamMirror u of MirrorOnAdmit url -> Just url; NoMirrorWrite -> Nothing)

    -- The mount shapes the composition root can produce, including ecosystem-declared artifact
    -- hosts (the PyPI files-host shape). The last shape's empty private URL authorises nothing.
    shapes :: [([Text], Maybe Text, Text, MirrorServePlan)]
    shapes =
        [ ([], Just "https://private.example.test", "https://public.example.test", MirrorOnAdmit "https://mirror.example.test")
        , ([], Just "https://private.example.test", "https://public.example.test", NoMirrorWrite)
        , ([], Nothing, "https://registry.npmjs.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org"], Nothing, "https://pypi.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org:8443"], Just "https://private.example.test:9443", "https://pypi.org:8443", MirrorOnAdmit "https://mirror.example.test:7443")
        , ([], Just "", "https://public.example.test", NoMirrorWrite)
        ]
