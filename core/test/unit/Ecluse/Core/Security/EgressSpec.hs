-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Security.EgressSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Security.Egress (mkConfiguredRegistryUrl, mkRegistryUrl, registryUrlText, resolveTarballUrl)

spec :: Spec
spec = do
    mkRegistryUrlSpec
    mkConfiguredRegistryUrlSpec
    resolveTarballUrlSpec

{- | 'mkRegistryUrl' is the production boundary: a registry target is https by construction, so
a running system cannot hold a plain-HTTP value. The rejection is the load-bearing half.
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

{- | 'mkConfiguredRegistryUrl' is the boundary an __operator-configured__ endpoint is
built at: https as above, and no credential material. Boot prints every resolved key,
the endpoint-collision warnings, and a posture line per mount. Each renders a
configured registry URL as the operator wrote it. A value carrying userinfo or a
query string must therefore never become a 'Ecluse.Core.Security.Egress.RegistryUrl'.
-}
mkConfiguredRegistryUrlSpec :: Spec
mkConfiguredRegistryUrlSpec = describe "mkConfiguredRegistryUrl (a configured endpoint carries no credential)" $ do
    it "accepts an ordinary configured endpoint" $
        (registryUrlText <$> mkConfiguredRegistryUrl "https://repo.internal.example.test/npm/")
            `shouldBe` Right "https://repo.internal.example.test/npm/"

    it "accepts a bracketed IPv6 literal with a port" $
        (registryUrlText <$> mkConfiguredRegistryUrl "https://[2001:db8::10]:8443/npm")
            `shouldBe` Right "https://[2001:db8::10]:8443/npm"

    it "accepts a path segment beginning with @ (an npm scope is not userinfo)" $
        (registryUrlText <$> mkConfiguredRegistryUrl "https://repo.internal.example.test/@acme/npm")
            `shouldBe` Right "https://repo.internal.example.test/@acme/npm"

    it "refuses a bare userinfo authority" $
        mkConfiguredRegistryUrl "https://deploy@repo.internal.example.test/" `shouldSatisfy` isLeft

    it "refuses a user:password authority" $
        mkConfiguredRegistryUrl "https://deploy:hunter2@repo.internal.example.test/npm" `shouldSatisfy` isLeft

    it "refuses a query string" $
        mkConfiguredRegistryUrl "https://repo.internal.example.test/npm?token=abc" `shouldSatisfy` isLeft

    it "refuses a fragment" $
        mkConfiguredRegistryUrl "https://repo.internal.example.test/npm#frag" `shouldSatisfy` isLeft

    it "names the requirement without quoting the value (the refusal reaches the boot log)" $
        refusalOf "https://deploy:hunter2@repo.internal.example.test/npm?token=abc"
            `shouldBe` "registry URL must not carry userinfo (a credential belongs in its own configuration key)"

    it "refuses the credential before the non-https refusal, which would quote the value" $
        refusalOf "http://deploy:hunter2@repo.internal.example.test/npm"
            `shouldSatisfy` (not . T.isInfixOf "hunter2")
  where
    refusalOf raw = fromLeft "unexpectedly accepted" (mkConfiguredRegistryUrl raw)

{- | 'resolveTarballUrl' normalises an upstream-declared @dist.tarball@ against the
host that served the packument. It keeps https, upgrades same-host http, and refuses
foreign-host http or any other non-http(s) scheme. A refusal drops that version.
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
