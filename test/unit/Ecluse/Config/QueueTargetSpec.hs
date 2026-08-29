-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.QueueTargetSpec (spec) where

import Test.Hspec

import Ecluse.Config.QueueTarget (QueueTarget (..), QueueUrl (..), mkQueueUrl, parseQueueTarget)

spec :: Spec
spec = do
    parseQueueTargetSpec
    mkQueueUrlSpec

parseQueueTargetSpec :: Spec
parseQueueTargetSpec = describe "parseQueueTarget" $ do
    it "parses a real SQS queue URL, taking the region from the host" $ do
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Just (SqsTarget "us-east-1")
        parseQueueTarget "https://sqs.eu-central-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Just (SqsTarget "eu-central-1")

    it "rejects a non-https scheme (the canonical form is https only)" $ do
        parseQueueTarget "http://sqs.us-east-1.amazonaws.com/123456789012/mirror" `shouldBe` Nothing
        parseQueueTarget "sqs.us-east-1.amazonaws.com/123456789012/mirror" `shouldBe` Nothing

    it "rejects an explicit port, :443 included (the canonical form carries none)" $ do
        -- A nearly canonical URL is a transcription error to surface, never to repair.
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com:443/123456789012/mirror" `shouldBe` Nothing
        parseQueueTarget "https://sqs.us-east-1.amazonaws.com:8443/123456789012/mirror" `shouldBe` Nothing

    it "rejects an AWS host that is not an SQS endpoint" $ do
        -- A dotted region slot means some other AWS endpoint shape, never an SQS
        -- queue's, so the parse must not yield a bogus region.
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

    it "rejects a bracketed host (the canonical form writes no brackets)" $
        parseQueueTarget "https://[sqs.us-east-1.amazonaws.com]/123456789012/mirror"
            `shouldBe` Nothing

    it "rejects userinfo in the authority (the canonical form carries none)" $ do
        parseQueueTarget "https://user@sqs.us-east-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Nothing
        parseQueueTarget "https://user:pass@sqs.us-east-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Nothing

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

mkQueueUrlSpec :: Spec
mkQueueUrlSpec = describe "mkQueueUrl" $ do
    it "derives the backend once, keeping the value as written" $ do
        mkQueueUrl "queue.url" "  https://sqs.us-east-1.amazonaws.com/123456789012/mirror  "
            `shouldBe` Right
                ( QueueUrl
                    { queueUrlText = "https://sqs.us-east-1.amazonaws.com/123456789012/mirror"
                    , queueUrlTarget = Just (SqsTarget "us-east-1")
                    }
                )
        mkQueueUrl "queue.url" "projects/acme/topics/mirror"
            `shouldBe` Right
                (QueueUrl{queueUrlText = "projects/acme/topics/mirror", queueUrlTarget = Just (PubSubTarget "acme" "mirror")})

    it "carries a shape that names no backend, which only the endpoint override dials" $
        -- An emulator queue URL matches no public shape, so the load cannot refuse one:
        -- "Ecluse.Composition.MirrorQueue" decides against the ambient AWS_ENDPOINT_URL_SQS.
        mkQueueUrl "queue.url" "http://ministack:4566/000000000000/mirror"
            `shouldBe` Right
                (QueueUrl{queueUrlText = "http://ministack:4566/000000000000/mirror", queueUrlTarget = Nothing})

    it "refuses credential material, naming the key and never the value" $ do
        mkQueueUrl "queue.url" "https://deploy:hunter2@sqs.us-east-1.amazonaws.com/123456789012/mirror"
            `shouldBe` Left
                "queue.url must not carry userinfo (a credential belongs in its own configuration key)"
        mkQueueUrl "queue.url" "https://sqs.us-east-1.amazonaws.com/123456789012/mirror?attr=1"
            `shouldBe` Left "queue.url must not carry a query string"
        mkQueueUrl "queue.url" "https://sqs.us-east-1.amazonaws.com/123456789012/mirror#frag"
            `shouldBe` Left "queue.url must not carry a fragment"

    it "refuses a blank value, which names no queue at all" $
        mkQueueUrl "queue.url" "   " `shouldBe` Left "queue.url must be a non-empty URL"
