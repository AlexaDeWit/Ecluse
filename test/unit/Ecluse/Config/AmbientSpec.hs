-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.AmbientSpec (spec) where

import Test.Hspec

import Ecluse.Config.Ambient (AmbientAws (..), ambientAwsFromEnv, ambientS3Endpoint, parseEndpointUrl)
import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (AwsEndpoint))

spec :: Spec
spec = do
    ambientAwsFromEnvSpec
    parseEndpointUrlSpec
    ambientS3EndpointSpec

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
        parseEndpointUrl "http://localhost:4566" `shouldBe` parsed False "localhost" 4566
    it "parses https with implicit port" $ do
        parseEndpointUrl "https://s3.amazonaws.com" `shouldBe` parsed True "s3.amazonaws.com" 443
    it "parses http with implicit port" $ do
        parseEndpointUrl "http://s3.amazonaws.com" `shouldBe` parsed False "s3.amazonaws.com" 80
    it "refuses a malformed URL, carrying the value back redacted" $ do
        parseEndpointUrl "not-a-url" `shouldBe` refused "not-a-url"
    it "hands back a bracketed IPv6 literal's host bare, with its written port" $ do
        parseEndpointUrl "http://[::1]:8080" `shouldBe` parsed False "::1" 8080

    describe "the egress gate's port grammar, never a second one" $ do
        -- The override names the endpoint the AWS SDK dials. Its port reads the way the SSRF
        -- gate reads one: decimal digits, no leading zero, a value in 1..65535.
        it "accepts a canonical port" $
            parseEndpointUrl "http://h:8080" `shouldBe` parsed False "h" 8080
        it "refuses a leading-zero port" $
            parseEndpointUrl "http://h:007" `shouldBe` refused "http://h:007"
        it "refuses a signed port" $ do
            parseEndpointUrl "http://h:-1" `shouldBe` refused "http://h:-1"
            parseEndpointUrl "http://h:+5" `shouldBe` refused "http://h:+5"
        it "refuses a hexadecimal port" $
            -- Haskell's own reader takes 0x10 for 16, so a decimal-digit check is what
            -- keeps one spelling per port.
            parseEndpointUrl "http://h:0x10" `shouldBe` refused "http://h:0x10"
        it "refuses a port past 65535" $
            parseEndpointUrl "http://h:65536" `shouldBe` refused "http://h:65536"
        it "refuses credential material anywhere in the URL" $ do
            -- An operator URL carrying userinfo, a query, or a fragment is refused, never
            -- silently stripped: a pre-signed query is a credential too.
            parseEndpointUrl "http://user:secret@h:1" `shouldBe` refused "http://user:secret@h:1"
            parseEndpointUrl "http://@h:80" `shouldBe` refused "http://@h:80"
            parseEndpointUrl "http://h/?sig=x" `shouldBe` refused "http://h/?sig=x"
            parseEndpointUrl "http://h#f" `shouldBe` refused "http://h#f"
        it "refuses a trailing colon with no port digits" $
            -- A written-but-empty port is malformed, never the scheme's default.
            parseEndpointUrl "http://h:" `shouldBe` refused "http://h:"

ambientS3EndpointSpec :: Spec
ambientS3EndpointSpec = describe "ambientS3Endpoint" $ do
    it "reads no override from an unset or blank AWS_ENDPOINT_URL" $ do
        ambientS3Endpoint (withS3Url Nothing) `shouldBe` Right Nothing
        ambientS3Endpoint (withS3Url (Just "   ")) `shouldBe` Right Nothing
    it "parses a set override" $ do
        ambientS3Endpoint (withS3Url (Just "http://localhost:9000"))
            `shouldBe` Right (Just (AwsEndpoint False "localhost" 9000))
    it "refuses a set-but-malformed override rather than reading it as no override" $ do
        ambientS3Endpoint (withS3Url (Just "http://u:tok@h")) `shouldBe` Left (mkSecret "http://u:tok@h")

withS3Url :: Maybe Text -> AmbientAws
withS3Url raw = AmbientAws{ambientAwsRegion = Nothing, ambientAwsEndpointUrlSqs = Nothing, ambientAwsEndpointUrl = raw}

parsed :: Bool -> Text -> Int -> Either Secret AwsEndpoint
parsed secure host port = Right (AwsEndpoint secure host port)

refused :: Text -> Either Secret AwsEndpoint
refused = Left . mkSecret
