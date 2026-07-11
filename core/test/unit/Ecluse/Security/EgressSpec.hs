-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.EgressSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText, resolveTarballUrl)

spec :: Spec
spec = do
    mkRegistryUrlSpec
    resolveTarballUrlSpec

{- | 'mkRegistryUrl' is the production boundary: a registry target is https by
construction, so a plain-HTTP value cannot be represented in a running system. These
prove the rejection (the load-bearing half) and that a release build's only path to a
'Ecluse.Core.Security.Egress.RegistryUrl' refuses http.
-}
mkRegistryUrlSpec :: Spec
mkRegistryUrlSpec = describe "mkRegistryUrl (https-only by construction)" $ do
    it "accepts an https URL, preserving its text" $
        (registryUrlText <$> mkRegistryUrl "https://registry.npmjs.org")
            `shouldBe` Right "https://registry.npmjs.org"

    it "accepts an https URL regardless of scheme case (schemes are case-insensitive)" $
        (registryUrlText <$> mkRegistryUrl "HTTPS://registry.npmjs.org")
            `shouldBe` Right "HTTPS://registry.npmjs.org"

    it "trims surrounding whitespace" $
        (registryUrlText <$> mkRegistryUrl "  https://registry.npmjs.org  ")
            `shouldBe` Right "https://registry.npmjs.org"

    it "rejects a plain-HTTP URL (the load-bearing rejection)" $
        mkRegistryUrl "http://registry.npmjs.org" `shouldSatisfy` isLeft

    it "rejects an empty value" $
        mkRegistryUrl "   " `shouldSatisfy` isLeft

    it "rejects a non-http(s) scheme" $
        mkRegistryUrl "ftp://registry.example/" `shouldSatisfy` isLeft

{- | 'resolveTarballUrl' normalises an upstream-declared @dist.tarball@ against the
host the packument was served from: https kept, same-host http upgraded, foreign-host
http (or any non-http(s)) refused. The refusals feed the per-version drop.
-}
resolveTarballUrlSpec :: Spec
resolveTarballUrlSpec = describe "resolveTarballUrl (dist.tarball scheme normalisation)" $ do
    let upstream = "registry.npmjs.org"
        resolved = fmap registryUrlText . resolveTarballUrl upstream

    it "keeps an https tarball on the same host" $
        resolved "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            `shouldBe` Right "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

    it "keeps an https tarball on a different (CDN) host" $
        resolved "https://cdn.example.net/thing-1.0.0.tgz"
            `shouldBe` Right "https://cdn.example.net/thing-1.0.0.tgz"

    it "upgrades a same-host http tarball to https" $
        resolved "http://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            `shouldBe` Right "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

    it "upgrades a same-host http tarball matched case-insensitively and ignoring the port" $
        resolved "http://Registry.NpmJS.org:443/thing/-/thing-1.0.0.tgz"
            `shouldBe` Right "https://Registry.NpmJS.org:443/thing/-/thing-1.0.0.tgz"

    it "refuses a foreign-host http tarball (it is dropped, not dialled in plaintext)" $
        resolveTarballUrl upstream "http://cdn.example.net/thing-1.0.0.tgz" `shouldSatisfy` isLeft

    it "refuses a non-http(s) tarball URL" $
        resolveTarballUrl upstream "ftp://files.example/thing-1.0.0.tgz" `shouldSatisfy` isLeft
