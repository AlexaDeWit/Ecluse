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

{- | Pin the invariant 'MountUpstreams' exists to hold. The tarball-host gate a bound
value carries is the gate of the upstreams that same value reports. The serve path runs
its SSRF check against the precomputed gate, so that check runs against the mount's
actual upstreams.

The other half of the guarantee is type-level and has no runtime case to write. The
constructor is private and the module exports no record selector. Outside
"Ecluse.Core.Server.Upstream" nothing can place a gate beside URLs it was not derived
from. Nothing can record-update a URL and leave the gate behind. A stale pair would be
a compile error at the offending call site, not a failing assertion here. These cases
therefore assert agreement over a spread of mount shapes. They do not probe divergence,
which is unwritable.
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

    -- The gate these upstreams should carry. It derives from what the value itself
    -- reports, not from the arguments it was built with. An accessor that disagreed
    -- with the stored gate would fail here.
    gateOf :: [Text] -> MountUpstreams -> TarballHostGate
    gateOf ecosystemHosts u =
        tarballHostGate
            ecosystemHosts
            (upstreamPrivateBaseUrl u)
            (upstreamPublicBaseUrl u)
            (case upstreamMirror u of MirrorOnAdmit url -> Just url; NoMirrorWrite -> Nothing)

    -- The mount shapes the composition root can produce. First a fully wired mirrored
    -- mount, a serve-only mount, and a pure public gate with no private upstream. Then
    -- an ecosystem that declares its own artifact hosts (the PyPI files-host shape),
    -- explicit ports, and a private URL from which no authority extracts. That last
    -- URL authorises nothing.
    shapes :: [([Text], Maybe Text, Text, MirrorServePlan)]
    shapes =
        [ ([], Just "https://private.example.test", "https://public.example.test", MirrorOnAdmit "https://mirror.example.test")
        , ([], Just "https://private.example.test", "https://public.example.test", NoMirrorWrite)
        , ([], Nothing, "https://registry.npmjs.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org"], Nothing, "https://pypi.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org:8443"], Just "https://private.example.test:9443", "https://pypi.org:8443", MirrorOnAdmit "https://mirror.example.test:7443")
        , ([], Just "", "https://public.example.test", NoMirrorWrite)
        ]
