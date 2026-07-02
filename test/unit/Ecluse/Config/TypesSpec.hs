{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.Rule (PolicyError (..), renderPolicyError)
import Ecluse.Config.Types (CredentialBackend (..), MirrorCredentialProvider (..), QueueBackend (..), mkUrl, parseCredentialBackend, parseMirrorCredentialProvider, parseQueueBackend, unUrl)
import Ecluse.Core.Wire (parseWire, renderWire)

spec :: Spec
spec = do
    backendSpec
    urlSpec
    policyErrorRenderSpec

backendSpec :: Spec
backendSpec = describe "backend selection" $ do
    describe "QueueBackend" $ do
        it "round-trips each backend through parse/render" $ do
            parseWire "sqs" `shouldBe` Right SqsQueue
            parseWire "pubsub" `shouldBe` Right PubSubQueue
            parseWire "memory" `shouldBe` Right MemoryQueue
            renderWire SqsQueue `shouldBe` "sqs"
            renderWire PubSubQueue `shouldBe` "pubsub"
            renderWire MemoryQueue `shouldBe` "memory"
        it "rejects an unknown name, naming the accepted set" $
            (parseWire "kafka" :: Either Text QueueBackend)
                `shouldBe` Left "unknown queue provider \"kafka\" (expected one of: sqs, pubsub, memory)"

    describe "parseQueueBackend" $ do
        it "parses sqs to SqsQueue" $
            parseQueueBackend "sqs" `shouldBe` Right SqsQueue
        it "parses memory to MemoryQueue" $
            parseQueueBackend "memory" `shouldBe` Right MemoryQueue
        it "parses pubsub to PubSubQueue" $
            parseQueueBackend "pubsub" `shouldBe` Right PubSubQueue
        it "rejects unknown backends" $
            parseQueueBackend "kafka"
                `shouldBe` Left "unknown queue provider \"kafka\" (expected one of: sqs, pubsub, memory)"

    describe "CredentialBackend" $ do
        it "round-trips each backend through parse/render" $ do
            parseWire "codeartifact" `shouldBe` Right CodeArtifactCredential
            parseWire "static" `shouldBe` Right StaticCredential
            parseWire "adc" `shouldBe` Right AdcCredential
            renderWire CodeArtifactCredential `shouldBe` "codeartifact"
            renderWire StaticCredential `shouldBe` "static"
            renderWire AdcCredential `shouldBe` "adc"
        it "rejects an unknown name, naming the accepted set" $
            (parseWire "vault" :: Either Text CredentialBackend)
                `shouldBe` Left "unknown credential provider \"vault\" (expected one of: codeartifact, static, adc)"

    describe "parseCredentialBackend" $ do
        it "parses codeartifact to CodeArtifactCredential" $
            parseCredentialBackend "codeartifact" `shouldBe` Right CodeArtifactCredential
        it "parses static to StaticCredential" $
            parseCredentialBackend "static" `shouldBe` Right StaticCredential
        it "parses adc to AdcCredential" $
            parseCredentialBackend "adc" `shouldBe` Right AdcCredential
        it "rejects unknown backends" $
            parseCredentialBackend "vault"
                `shouldBe` Left "unknown credential provider \"vault\" (expected one of: codeartifact, static, adc)"

    describe "MirrorCredentialProvider" $ do
        it "round-trips each backend through parse/render" $ do
            parseWire "static" `shouldBe` Right (MirrorCredentialProvider StaticCredential)
            parseWire "codeartifact" `shouldBe` Right (MirrorCredentialProvider CodeArtifactCredential)
            parseWire "gcp-artifact-registry" `shouldBe` Right (MirrorCredentialProvider AdcCredential)
            renderWire (MirrorCredentialProvider StaticCredential) `shouldBe` "static"
            renderWire (MirrorCredentialProvider CodeArtifactCredential) `shouldBe` "codeartifact"
            renderWire (MirrorCredentialProvider AdcCredential) `shouldBe` "gcp-artifact-registry"
        it "rejects an unknown name, naming the accepted set" $
            (parseWire "vault" :: Either Text MirrorCredentialProvider)
                `shouldBe` Left "unknown mirror-target credential provider \"vault\" (expected one of: static, codeartifact, gcp-artifact-registry)"

    describe "parseMirrorCredentialProvider" $ do
        it "parses codeartifact to CodeArtifactCredential" $
            parseMirrorCredentialProvider "codeartifact" `shouldBe` Right CodeArtifactCredential
        it "parses static to StaticCredential" $
            parseMirrorCredentialProvider "static" `shouldBe` Right StaticCredential
        it "parses gcp-artifact-registry to AdcCredential" $
            parseMirrorCredentialProvider "gcp-artifact-registry" `shouldBe` Right AdcCredential
        it "rejects unknown backends" $
            parseMirrorCredentialProvider "vault"
                `shouldBe` Left "unknown mirror-target credential provider \"vault\" (expected one of: static, codeartifact, gcp-artifact-registry)"

urlSpec :: Spec
urlSpec = describe "Url" $ do
    it "trims surrounding whitespace and round-trips" $
        (unUrl <$> mkUrl "  https://x  ") `shouldBe` Right "https://x"
    it "rejects an all-whitespace value" $
        mkUrl "   " `shouldBe` Left "expected a non-empty URL"

policyErrorRenderSpec :: Spec
policyErrorRenderSpec = describe "renderPolicyError" $
    -- Each constructor renders a distinct, operator-facing line.
    it "renders every policy-error kind" $ do
        renderPolicyError (MissingRuleType "x") `shouldSatisfy` ("missing" `T.isInfixOf`)
        renderPolicyError (UnknownRuleType "x" "Y") `shouldSatisfy` ("unknown type" `T.isInfixOf`)
        renderPolicyError (MalformedRule "x" "bad") `shouldSatisfy` ("bad" `T.isInfixOf`)
        renderPolicyError (SuppressUnknownRule "x") `shouldSatisfy` ("disables" `T.isInfixOf`)
