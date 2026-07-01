{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.Rule (PolicyError (..), renderPolicyError)
import Ecluse.Config.Types (CredentialBackend (..), QueueBackend (..), mkUrl, parseCredentialBackend, parseQueueBackend, renderCredentialBackend, unUrl)
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
            parseCredentialBackend "codeartifact" `shouldBe` Right CodeArtifactCredential
            parseCredentialBackend "static" `shouldBe` Right StaticCredential
            parseCredentialBackend "adc" `shouldBe` Right AdcCredential
            renderCredentialBackend CodeArtifactCredential `shouldBe` "codeartifact"
            renderCredentialBackend StaticCredential `shouldBe` "static"
            renderCredentialBackend AdcCredential `shouldBe` "adc"
        it "rejects an unknown name, naming the accepted set" $
            parseCredentialBackend "vault"
                `shouldBe` Left "unknown credential provider \"vault\" (expected one of: codeartifact, static, adc)"

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
