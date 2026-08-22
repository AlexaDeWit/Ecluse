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

{- | Pin the invariant 'MountUpstreams' exists to hold: the tarball-host gate a bound
cluster carries is the gate of the upstreams that same cluster reports, so the SSRF
check the serve path runs against the precomputed gate is a check against the mount's
actual upstreams.

The other half of the guarantee is type-level and has no runtime case to write. The
constructor is private and no record selector is exported, so outside
"Ecluse.Core.Server.Upstream" there is no way to place a gate beside URLs it was not
derived from, and no way to record-update a URL and leave the gate behind: a stale pair
would be a compile error at the offending call site, not a failing assertion here. That
is why these cases assert agreement over a spread of mount shapes rather than probing
divergence, which is unwritable.
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

    -- The gate these upstreams should carry, derived from what the value itself
    -- reports rather than from the arguments it was built with, so an accessor that
    -- disagreed with the stored gate would fail here.
    gateOf :: [Text] -> MountUpstreams -> TarballHostGate
    gateOf ecosystemHosts u =
        tarballHostGate
            ecosystemHosts
            (upstreamPrivateBaseUrl u)
            (upstreamPublicBaseUrl u)
            (case upstreamMirror u of MirrorOnAdmit url -> Just url; NoMirrorWrite -> Nothing)

    -- The mount shapes the composition root can produce: a fully wired mirrored mount,
    -- a serve-only mount, a pure public gate with no private upstream, an ecosystem
    -- that declares its own artifact hosts (the PyPI files-host shape), explicit ports,
    -- and a private URL from which no authority extracts (which authorises nothing).
    shapes :: [([Text], Maybe Text, Text, MirrorServePlan)]
    shapes =
        [ ([], Just "https://private.example.test", "https://public.example.test", MirrorOnAdmit "https://mirror.example.test")
        , ([], Just "https://private.example.test", "https://public.example.test", NoMirrorWrite)
        , ([], Nothing, "https://registry.npmjs.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org"], Nothing, "https://pypi.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org:8443"], Just "https://private.example.test:9443", "https://pypi.org:8443", MirrorOnAdmit "https://mirror.example.test:7443")
        , ([], Just "", "https://public.example.test", NoMirrorWrite)
        ]
