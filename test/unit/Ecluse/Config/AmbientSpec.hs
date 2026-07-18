-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.AmbientSpec (spec) where

import Test.Hspec

import Ecluse.Config.Ambient (AmbientAws (..), ambientAwsFromEnv)

spec :: Spec
spec = describe "ambientAwsFromEnv" $ do
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
