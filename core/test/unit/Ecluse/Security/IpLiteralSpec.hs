-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.IpLiteralSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security (parseIpLiteral)

spec :: Spec
spec = parseIpLiteralSpec

parseIpLiteralSpec :: Spec
parseIpLiteralSpec = describe "parseIpLiteral" $ do
    it "returns Nothing for empty strings" $
        void (parseIpLiteral "") `shouldBe` Nothing

    it "returns Nothing for regular hostnames" $
        void (parseIpLiteral "registry.npmjs.org") `shouldBe` Nothing

    it "returns Just for standard IPv4" $
        void (parseIpLiteral "127.0.0.1") `shouldBe` Just ()

    it "returns Just for hex IPv4" $
        void (parseIpLiteral "0x7f.0.0.1") `shouldBe` Just ()

    it "returns Just for octal IPv4" $
        void (parseIpLiteral "0177.0.0.1") `shouldBe` Just ()

    it "returns Just for standard IPv6" $ do
        void (parseIpLiteral "::1") `shouldBe` Just ()
        void (parseIpLiteral "fe80::1") `shouldBe` Just ()

    it "returns Just for IPv4-mapped IPv6" $
        void (parseIpLiteral "::ffff:127.0.0.1") `shouldBe` Just ()

    it "returns Nothing for invalid short IPv4" $
        void (parseIpLiteral "127.0.0") `shouldBe` Nothing

    it "returns Nothing for large IPv4 octets" $
        void (parseIpLiteral "0400.0.0.1") `shouldBe` Nothing

    it "returns Nothing for malformed IPv6" $ do
        void (parseIpLiteral "fe80::1ffff") `shouldBe` Nothing
        void (parseIpLiteral "1::2::3") `shouldBe` Nothing
