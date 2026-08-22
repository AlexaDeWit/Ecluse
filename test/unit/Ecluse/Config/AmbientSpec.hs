-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.AmbientSpec (spec) where

import Test.Hspec

import Ecluse.Config.Ambient (AmbientAws (..), ambientAwsFromEnv, parseEndpointUrl)

spec :: Spec
spec = do
    ambientAwsFromEnvSpec
    parseEndpointUrlSpec

ambientAwsFromEnvSpec :: Spec
ambientAwsFromEnvSpec = describe "ambientAwsFromEnv" $ do
    it "reads the three consulted AWS variables from the process environment" $ do
        let env =
                [ ("AWS_REGION", "eu-west-1")
                , ("AWS_ENDPOINT_URL_SQS", "http://localhost:4566")
                , ("AWS_ENDPOINT_URL", "http://localhost:9000")
                , ("UNRELATED", "x")
                ]
        ambientAwsFromEnv env
            `shouldBe` AmbientAws
                { ambientAwsRegion = Just "eu-west-1"
                , ambientAwsEndpointUrlSqs = Just "http://localhost:4566"
                , ambientAwsEndpointUrl = Just "http://localhost:9000"
                }

    it "yields Nothing per variable when unset (blank handling stays with each consumer)" $ do
        ambientAwsFromEnv [] `shouldBe` AmbientAws Nothing Nothing Nothing

parseEndpointUrlSpec :: Spec
parseEndpointUrlSpec = describe "parseEndpointUrl" $ do
    it "parses http with explicit port" $ do
        parseEndpointUrl "http://localhost:4566" `shouldBe` Just (False, "localhost", 4566)
    it "parses https with implicit port" $ do
        parseEndpointUrl "https://s3.amazonaws.com" `shouldBe` Just (True, "s3.amazonaws.com", 443)
    it "parses http with implicit port" $ do
        parseEndpointUrl "http://s3.amazonaws.com" `shouldBe` Just (False, "s3.amazonaws.com", 80)
    it "rejects malformed URLs" $ do
        parseEndpointUrl "not-a-url" `shouldBe` Nothing
    it "hands back a bracketed IPv6 literal's host bare, with its written port" $ do
        parseEndpointUrl "http://[::1]:8080" `shouldBe` Just (False, "::1", 8080)

    describe "the egress gate's port grammar, never a second one" $ do
        -- The override names the endpoint the AWS SDK dials. Its port reads the way the SSRF
        -- gate reads one: decimal digits, no leading zero, a value in 1..65535.
        it "accepts a canonical port" $
            parseEndpointUrl "http://h:8080" `shouldBe` Just (False, "h", 8080)
        it "refuses a leading-zero port" $
            parseEndpointUrl "http://h:007" `shouldBe` Nothing
        it "refuses a signed port" $ do
            parseEndpointUrl "http://h:-1" `shouldBe` Nothing
            parseEndpointUrl "http://h:+5" `shouldBe` Nothing
        it "refuses a hexadecimal port" $
            -- Haskell's own reader takes 0x10 for 16, so a decimal-digit check is what
            -- keeps one spelling per port.
            parseEndpointUrl "http://h:0x10" `shouldBe` Nothing
        it "refuses a port past 65535" $
            parseEndpointUrl "http://h:65536" `shouldBe` Nothing
        it "refuses a trailing colon with no port digits" $
            -- A written-but-empty port is malformed, never the scheme's default.
            parseEndpointUrl "http://h:" `shouldBe` Nothing
