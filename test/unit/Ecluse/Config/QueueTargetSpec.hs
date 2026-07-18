-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.QueueTargetSpec (spec) where

import Test.Hspec

import Ecluse.Config.QueueTarget (QueueTarget (..), parseQueueTarget)

spec :: Spec
spec = describe "parseQueueTarget" $ do
    it "parses a real SQS queue URL, taking the region from the host" $ do
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Just (SqsTarget "us-east-1")
        parseQueueTarget "https://sqs.eu-central-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Just (SqsTarget "eu-central-1")

    it "rejects a non-https scheme (the canonical form is https only)" $ do
        parseQueueTarget "http://sqs.us-east-1.amazonaws.com/123456789012/mirror" `shouldBe` Nothing
        parseQueueTarget "sqs.us-east-1.amazonaws.com/123456789012/mirror" `shouldBe` Nothing

    it "rejects an explicit port, :443 included (the canonical form carries none)" $ do
        -- A nearly-but-not canonical URL is a transcription error to surface,
        -- never to repair.
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com:443/123456789012/mirror" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com:8443/123456789012/mirror" `shouldBe` Nothing

    it "rejects an AWS host that is not an SQS endpoint" $ do
        -- A dotted region slot means some other AWS endpoint shape (never an SQS
        -- queue's), so it must not mis-parse into a bogus region.
        parseQueueTarget "https://sqs.foo.bar.amazonaws.com/123456789012/mirror" `shouldBe` Nothing
        parseQueueTarget "https://s3.us-east-1.amazonaws.com/bucket/key" `shouldBe` Nothing

    it "rejects an empty region label" $
        parseQueueTarget "https://sqs..amazonaws.com/123456789012/mirror" `shouldBe` Nothing

    it "rejects a non-AWS host, however SQS-like its path" $
        parseQueueTarget "https://sqs.example.test/123456789012/mirror" `shouldBe` Nothing

    it "rejects an account that is not 12 digits" $ do
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/12345678901/mirror" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/1234567890123/mirror" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/12345678901x/mirror" `shouldBe` Nothing

    it "rejects a missing or empty queue segment" $ do
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/" `shouldBe` Nothing

    it "rejects anything after the queue segment" $
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/mirror/extra" `shouldBe` Nothing

    it "rejects a query or fragment" $ do
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/mirror?attr=1" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/mirror#frag" `shouldBe` Nothing

    it "parses a Pub/Sub topic resource into its project and topic" $
        parseQueueTarget "projects/acme/topics/mirror"
            `shouldBe` Just (PubSubTarget "acme" "mirror")

    it "rejects a malformed Pub/Sub resource" $ do
        parseQueueTarget "projects//topics/mirror" `shouldBe` Nothing
        parseQueueTarget "projects/acme/topics" `shouldBe` Nothing
        parseQueueTarget "projects/acme/subscriptions/mirror" `shouldBe` Nothing

    it "recognises no other shape" $
        parseQueueTarget "https://queue.example.test/q" `shouldBe` Nothing
