-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.Rule (PolicyError (..), renderPolicyError)
import Ecluse.Config.Types (HttpScheme (Http, Https), mkUrl, splitHttpScheme, unUrl)

spec :: Spec
spec = do
    urlSpec
    schemeSpec
    policyErrorRenderSpec

{- A 'Url' exists only for a value that cleared every check, so each refusal below is a value the
type cannot hold. The key names each one, because boot reports the key an operator must fix. -}
urlSpec :: Spec
urlSpec = describe "mkUrl" $ do
    it "trims surrounding whitespace and round-trips" $
        (unUrl <$> mkUrl "server.publicUrl" "  https://registry.example.test  ")
            `shouldBe` Right "https://registry.example.test"

    it "accepts plain http, which loopback deployments write" $
        (unUrl <$> mkUrl "server.publicUrl" "http://localhost:8080")
            `shouldBe` Right "http://localhost:8080"

    it "refuses userinfo, naming the key and never the credential" $
        mkUrl "server.publicUrl" "https://deploy:hunter2@registry.example.test"
            `shouldBe` Left
                "server.publicUrl must not carry userinfo (a credential belongs in its own configuration key)"

    it "refuses a query string and a fragment" $ do
        mkUrl "advisories.osvExportBaseUrl" "https://osv.example.test?sig=abc"
            `shouldBe` Left "advisories.osvExportBaseUrl must not carry a query string"
        mkUrl "advisories.osvExportBaseUrl" "https://osv.example.test#frag"
            `shouldBe` Left "advisories.osvExportBaseUrl must not carry a fragment"

    it "refuses a credential ahead of the scheme refusal, which quotes the value" $
        -- A schemeless value would otherwise fall to the scheme refusal and echo the credential.
        mkUrl "server.publicUrl" "deploy:hunter2@registry.example.test"
            `shouldBe` Left
                "server.publicUrl must not carry userinfo (a credential belongs in its own configuration key)"

    it "refuses a scheme that is neither http nor https, a blank value included" $ do
        mkUrl "advisories.osvExportBaseUrl" "s3://osv.example.test/exports"
            `shouldBe` Left
                "advisories.osvExportBaseUrl must be an http:// or https:// URL (got s3://osv.example.test/exports)"
        mkUrl "server.publicUrl" "   "
            `shouldBe` Left "server.publicUrl must be an http:// or https:// URL (got )"

    it "refuses a URL the egress gate can extract no authority from" $ do
        mkUrl "server.publicUrl" "https:///npm"
            `shouldBe` Left
                "server.publicUrl must carry a host and, when a port is written, a decimal port in 1..65535 (got https:///npm)"
        mkUrl "server.publicUrl" "https://registry.example.test:0/npm"
            `shouldBe` Left
                "server.publicUrl must carry a host and, when a port is written, a decimal port in 1..65535 (got https://registry.example.test:0/npm)"

schemeSpec :: Spec
schemeSpec = describe "splitHttpScheme" $
    it "names the scheme a URL writes, and only http or https" $ do
        splitHttpScheme "https://registry.example.test/npm" `shouldBe` Just (Https, "registry.example.test/npm")
        splitHttpScheme "http://localhost:8080" `shouldBe` Just (Http, "localhost:8080")
        splitHttpScheme "sqs://queue.example.test" `shouldBe` Nothing
        splitHttpScheme "registry.example.test" `shouldBe` Nothing

policyErrorRenderSpec :: Spec
policyErrorRenderSpec = describe "renderPolicyError" $
    -- Each constructor renders a distinct, operator-facing line.
    it "renders every policy-error kind" $ do
        renderPolicyError (MissingRuleType "x") `shouldSatisfy` ("missing" `T.isInfixOf`)
        renderPolicyError (UnknownRuleType "x" "Y") `shouldSatisfy` ("unknown type" `T.isInfixOf`)
        renderPolicyError (MalformedRule "x" "bad") `shouldSatisfy` ("bad" `T.isInfixOf`)
        renderPolicyError (SuppressUnknownRule "x") `shouldSatisfy` ("disables" `T.isInfixOf`)
