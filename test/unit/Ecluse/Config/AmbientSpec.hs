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
