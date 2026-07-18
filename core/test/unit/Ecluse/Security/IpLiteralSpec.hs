-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.IpLiteralSpec (spec) where

import Test.Hspec

import Ecluse.Core.Security (isDecimal, isHex, parseIpLiteral)

spec :: Spec
spec = do
    isDecimalSpec
    isHexSpec
    parseIpLiteralSpec

isDecimalSpec :: Spec
isDecimalSpec = describe "isDecimal" $ do
    it "returns True for a string with only decimal digits" $
        isDecimal "1234567890" `shouldBe` True
    it "returns False for an empty string" $
        isDecimal "" `shouldBe` False
    it "returns False for a string with spaces" $
        isDecimal "123 456" `shouldBe` False
    it "returns False for a string with letters" $
        isDecimal "123a456" `shouldBe` False
    it "returns False for a string with a sign" $
        isDecimal "-123" `shouldBe` False
    it "returns False for a string with a decimal point" $
        isDecimal "123.456" `shouldBe` False
    it "returns True for a single digit" $
        isDecimal "0" `shouldBe` True

isHexSpec :: Spec
isHexSpec = describe "isHex" $ do
    it "accepts valid lowercase hex strings" $ do
        isHex "0123456789abcdef" `shouldBe` True
        isHex "a" `shouldBe` True
        isHex "f" `shouldBe` True

    it "accepts valid uppercase hex strings" $ do
        isHex "0123456789ABCDEF" `shouldBe` True
        isHex "A" `shouldBe` True
        isHex "F" `shouldBe` True

    it "accepts mixed case hex strings" $ do
        isHex "aBcDeF" `shouldBe` True
        isHex "1a2B3c" `shouldBe` True

    it "rejects empty strings" $ do
        isHex "" `shouldBe` False

    it "rejects strings with non-hex characters" $ do
        isHex "g" `shouldBe` False
        isHex "G" `shouldBe` False
        isHex "0123456789abcdefg" `shouldBe` False
        isHex "abc def" `shouldBe` False
        isHex "abc-def" `shouldBe` False
        isHex "-1a" `shouldBe` False
        isHex "0x1a" `shouldBe` False

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
