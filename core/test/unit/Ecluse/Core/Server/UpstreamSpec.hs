-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.UpstreamSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security (TarballHostGate, tarballHostGate)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
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
        let original = mountUpstreams [] Nothing (url "https://public.one") NoMirrorWrite
            rebound = mountUpstreams [] Nothing (url "https://public.two") NoMirrorWrite
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
            (registryUrlText <$> upstreamPrivateBaseUrl u)
            (registryUrlText (upstreamPublicBaseUrl u))
            (case upstreamMirror u of MirrorOnAdmit mirror -> Just (registryUrlText mirror); NoMirrorWrite -> Nothing)

    -- The witness a fixture URL enters as. The suite exercises the gate's derivation, not the
    -- https-only parse, so the loopback former stands in for every shape.
    url :: Text -> RegistryUrl
    url = loopbackRegistryUrl

    -- The mount shapes the composition root can produce, including ecosystem-declared artifact
    -- hosts (the PyPI files-host shape). The last shape's empty private URL authorises nothing.
    shapes :: [([Text], Maybe RegistryUrl, RegistryUrl, MirrorServePlan)]
    shapes =
        [ ([], Just (url "https://private.example.test"), url "https://public.example.test", MirrorOnAdmit (url "https://mirror.example.test"))
        , ([], Just (url "https://private.example.test"), url "https://public.example.test", NoMirrorWrite)
        , ([], Nothing, url "https://registry.npmjs.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org"], Nothing, url "https://pypi.org", NoMirrorWrite)
        , (["https://files.pythonhosted.org:8443"], Just (url "https://private.example.test:9443"), url "https://pypi.org:8443", MirrorOnAdmit (url "https://mirror.example.test:7443"))
        , ([], Just (url ""), url "https://public.example.test", NoMirrorWrite)
        ]
