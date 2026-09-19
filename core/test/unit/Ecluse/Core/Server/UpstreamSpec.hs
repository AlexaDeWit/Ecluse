-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pin the invariant 'MountUpstreams' exists to hold: the tarball-host gate a bound value
carries authorises the upstreams that same value reports, so the serve path's SSRF check runs
against the mount's actual upstreams. The gate is read through the question the serve path
asks it, 'isAllowedUpstreamHost' over a fixed candidate set, so a gate that authorised the
wrong authority fails here rather than agreeing with a restatement of its own derivation.
Divergence is unwritable, because the constructor is private and no record selector escapes
"Ecluse.Core.Server.Upstream", so a stale pair is a compile error rather than a failing case.
-}
module Ecluse.Core.Server.UpstreamSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Security (
    AllowedHostPorts,
    HostPort (HostPort),
    TarballHostGate,
    allowedHostPorts,
    isAllowedUpstreamHost,
    thgAllowlist,
    thgEcosystemHosts,
    thgPrivateHostPort,
    thgPublicHostPort,
 )
import Ecluse.Core.Security.Egress (RegistryUrl)
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

spec :: Spec
spec = describe "MountUpstreams (a mount's upstreams and their derived gate)" $ do
    it "reports back the upstreams it was bound with" $
        map (\u -> (upstreamPrivateBaseUrl u, upstreamPublicBaseUrl u, upstreamMirror u)) bound
            `shouldBe` map (\s -> (shPrivate s, shPublic s, shMirror s)) shapes

    it "authorises the outbound authorities its upstreams name, at their own ports alone" $
        map (authorised . upstreamTarballHostGate) bound
            `shouldBe` map shAuthorised shapes

    it "carries the reference authorities and the ecosystem artifact hosts its upstreams name" $
        map (references . upstreamTarballHostGate) bound
            `shouldBe` map shReferences shapes
  where
    bound :: [MountUpstreams]
    bound = map (\s -> mountUpstreams (shEcosystemHosts s) (shPrivate s) (shPublic s) (shMirror s)) shapes

-- | One mount the composition root can produce, and the gate its upstreams must derive.
data Shape = Shape
    { shEcosystemHosts :: [Text]
    , shPrivate :: Maybe RegistryUrl
    , shPublic :: RegistryUrl
    , shMirror :: MirrorServePlan
    , shAuthorised :: [HostPort]
    -- ^ The 'candidates' this shape's gate authorises, in candidate order.
    , shReferences :: (Maybe HostPort, Maybe HostPort, AllowedHostPorts)
    -- ^ The private and public reference authorities, and the ecosystem artifact hosts.
    }

-- The mount shapes, including ecosystem-declared artifact hosts (the PyPI files-host shape),
-- the written ports, and a private URL that holds no authority and so authorises nothing.
shapes :: [Shape]
shapes =
    [ Shape
        { shEcosystemHosts = []
        , shPrivate = Just (url "https://private.example.test")
        , shPublic = url "https://public.example.test"
        , shMirror = MirrorOnAdmit (url "https://mirror.example.test")
        , shAuthorised = [hp "private.example.test" 443, hp "public.example.test" 443, hp "mirror.example.test" 443]
        , shReferences = (Just (hp "private.example.test" 443), Just (hp "public.example.test" 443), noEcosystemHosts)
        }
    , Shape
        { shEcosystemHosts = []
        , shPrivate = Just (url "https://private.example.test")
        , shPublic = url "https://public.example.test"
        , shMirror = NoMirrorWrite
        , shAuthorised = [hp "private.example.test" 443, hp "public.example.test" 443]
        , shReferences = (Just (hp "private.example.test" 443), Just (hp "public.example.test" 443), noEcosystemHosts)
        }
    , Shape
        { shEcosystemHosts = []
        , shPrivate = Nothing
        , shPublic = url "https://registry.npmjs.org"
        , shMirror = NoMirrorWrite
        , shAuthorised = [hp "registry.npmjs.org" 443]
        , shReferences = (Nothing, Just (hp "registry.npmjs.org" 443), noEcosystemHosts)
        }
    , Shape
        { shEcosystemHosts = ["https://files.pythonhosted.org"]
        , shPrivate = Nothing
        , shPublic = url "https://pypi.org"
        , shMirror = NoMirrorWrite
        , shAuthorised = [hp "pypi.org" 443, hp "files.pythonhosted.org" 443]
        , shReferences = (Nothing, Just (hp "pypi.org" 443), ecosystemHosts [hp "files.pythonhosted.org" 443])
        }
    , Shape
        { shEcosystemHosts = ["https://files.pythonhosted.org:8443"]
        , shPrivate = Just (url "https://private.example.test:9443")
        , shPublic = url "https://pypi.org:8443"
        , shMirror = MirrorOnAdmit (url "https://mirror.example.test:7443")
        , shAuthorised = [hp "private.example.test" 9443, hp "mirror.example.test" 7443, hp "pypi.org" 8443, hp "files.pythonhosted.org" 8443]
        , shReferences = (Just (hp "private.example.test" 9443), Just (hp "pypi.org" 8443), ecosystemHosts [hp "files.pythonhosted.org" 8443])
        }
    , Shape
        { shEcosystemHosts = []
        , shPrivate = Just (url "")
        , shPublic = url "https://public.example.test"
        , shMirror = NoMirrorWrite
        , shAuthorised = [hp "public.example.test" 443]
        , shReferences = (Nothing, Just (hp "public.example.test" 443), noEcosystemHosts)
        }
    ]

{- | Every authority the shapes name, at every port they write, plus one no shape names. A gate
answers each one, so a widened gate fails as loudly as a narrowed one.
-}
candidates :: [HostPort]
candidates =
    [ hp "private.example.test" 443
    , hp "private.example.test" 9443
    , hp "public.example.test" 443
    , hp "mirror.example.test" 443
    , hp "mirror.example.test" 7443
    , hp "registry.npmjs.org" 443
    , hp "pypi.org" 443
    , hp "pypi.org" 8443
    , hp "files.pythonhosted.org" 443
    , hp "files.pythonhosted.org" 8443
    , hp "unrelated.example.test" 443
    ]

-- The candidates a gate admits, asked exactly as the per-request SSRF check asks.
authorised :: TarballHostGate -> [HostPort]
authorised gate = filter (isAllowedUpstreamHost (thgAllowlist gate)) candidates

-- The gate's three remaining fields, which the artifact authority check reads.
references :: TarballHostGate -> (Maybe HostPort, Maybe HostPort, AllowedHostPorts)
references gate = (thgPrivateHostPort gate, thgPublicHostPort gate, thgEcosystemHosts gate)

hp :: Text -> Word16 -> HostPort
hp = HostPort

ecosystemHosts :: [HostPort] -> AllowedHostPorts
ecosystemHosts = allowedHostPorts . Set.fromList

noEcosystemHosts :: AllowedHostPorts
noEcosystemHosts = ecosystemHosts []

-- The witness a fixture URL enters as. The suite exercises the gate's derivation, not the
-- https-only parse, so the loopback former stands in for every shape.
url :: Text -> RegistryUrl
url = loopbackRegistryUrl
