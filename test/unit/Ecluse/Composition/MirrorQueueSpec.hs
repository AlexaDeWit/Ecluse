-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MirrorQueueSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (..),
    MirrorRuntimePlan (..),
    memoryQueueBootWarning,
    mirrorQueuePlanWarning,
    parseEndpointUrl,
    planMirrorQueue,
    planMirrorRuntime,
 )
import Ecluse.Composition.Support (expectConfig, expectEnv, overrideEnv, staticEnvVars, withoutQueueUrl)
import Ecluse.Config (AppConfig, QueueBackend (..))
import Ecluse.Config.Ambient (AmbientAws (..))
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint, sqsQueueUrl, sqsRegion), SqsEndpoint (endpointHost, endpointPort, endpointSecure))

spec :: Spec
spec = do
    mirrorRuntimeSpec
    mirrorQueueSpec
    parseEndpointUrlSpec

mirrorRuntimeSpec :: Spec
mirrorRuntimeSpec = describe "planMirrorRuntime" $ do
    it "plans no mirror runtime when no mount mirrors (queue variables never consulted)" $ do
        -- The serve-only deployment boots under the shipped sqs default with no
        -- ECLUSE_QUEUE_URL and no AWS_REGION: the queue selection never runs.
        cfg <- expectConfig [("ECLUSE_MOUNTS__NPM__ENABLED", "true")] Nothing
        planMirrorRuntime noAmbient' cfg `shouldBe` Right NoMirroring

    it "delegates to the queue selection when a mount mirrors, surfacing its errors" $ do
        -- The same missing-region/missing-URL failures as planMirrorQueue: the
        -- mirroring mount is what makes the queue configuration load-bearing.
        cfg <- expectConfig (withoutQueueUrl staticEnvVars) Nothing
        planMirrorRuntime noAmbient' cfg `shouldBe` Left [QueueRegionMissing, QueueUrlMissing SqsQueue]

    it "plans the selected backend when a mount mirrors and the queue resolves" $ do
        cfg <- expectConfig staticEnvVars Nothing
        case planMirrorRuntime noAmbient'{ambientAwsRegion = Just "us-east-1"} cfg of
            Right (MirrorWith (SqsBackend _)) -> pass
            other -> expectationFailure ("expected an SQS mirror runtime, got: " <> show other)
  where
    noAmbient' :: AmbientAws
    noAmbient' = AmbientAws Nothing Nothing Nothing

mirrorQueueSpec :: Spec
mirrorQueueSpec = describe "planMirrorQueue" $ do
    it "selects the SQS backend from the configured queue URL and region" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend (withRegion "us-east-1") env
        sqsQueueUrl cfg `shouldBe` "https://sqs.example.test/q"
        sqsRegion cfg `shouldBe` "us-east-1"

    it "fails fast when the SQS backend has no AWS_REGION" $ do
        -- AWS_REGION is required for the AWS queue; absent, the backend cannot be
        -- region-scoped, so it is a loud boot failure rather than a silent default.
        env <- expectEnv staticEnvVars
        planMirrorQueue noAmbient env `shouldBe` Left [QueueRegionMissing]

    it "treats a blank AWS_REGION as missing" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue (withRegion "   ") env `shouldBe` Left [QueueRegionMissing]

    it "fails fast when the SQS backend has no ECLUSE_QUEUE_URL" $ do
        -- ECLUSE_QUEUE_URL is optional at the env layer but required for sqs here: the
        -- jobs need a queue to be sent to, so an absent one is a fail-loud boot error.
        env <- expectEnv (withoutQueueUrl staticEnvVars)
        planMirrorQueue (withRegion "us-east-1") env `shouldBe` Left [QueueUrlMissing SqsQueue]

    it "aggregates a missing region and a missing queue URL under sqs in one report" $ do
        env <- expectEnv (withoutQueueUrl staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Left [QueueRegionMissing, QueueUrlMissing SqsQueue]

    it "refuses the GCP pubsub backend as not built in this binary (no silent fallback)" $ do
        -- The pubsub arm is recognised by config (S03) but has no backend compiled in;
        -- it must route to a clear "not built" error, never quietly to a different queue.
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE_BACKEND" "pubsub" staticEnvVars)
        planMirrorQueue (withRegion "us-east-1") env `shouldBe` Left [QueueProviderUnavailable PubSubQueue]

    it "selects the bounded in-memory backend with the configured cap (no AWS_REGION or ECLUSE_QUEUE_URL needed)" $ do
        -- The memory backend is an explicit operator choice that needs no cloud queue:
        -- it carries only its depth cap, and neither AWS_REGION nor ECLUSE_QUEUE_URL is
        -- consulted, so it resolves cleanly with both absent.
        env <-
            expectEnv
                ( overrideEnv "ECLUSE_QUEUE_BACKEND" "memory" $
                    ("ECLUSE_QUEUE_MEMORY_MAX_DEPTH", "1234") : withoutQueueUrl staticEnvVars
                )
        planMirrorQueue noAmbient env `shouldBe` Right (MemoryBackend (defaultMemoryQueueConfig 1234))

    it "defaults the in-memory backend's cap when ECLUSE_QUEUE_MEMORY_MAX_DEPTH is unset" $ do
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE_BACKEND" "memory" staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Right (MemoryBackend (defaultMemoryQueueConfig 50000))

    it "warns loudly on selecting the in-memory backend, and not on the durable SQS one" $ do
        -- AC3: selecting memory emits a loud non-durable/best-effort boot warning;
        -- a durable backend warrants none. The composition root logs the Just.
        memEnv <- expectEnv (overrideEnv "ECLUSE_QUEUE_BACKEND" "memory" staticEnvVars)
        sqsEnv <- expectEnv staticEnvVars
        (mirrorQueuePlanWarning <$> planMirrorQueue noAmbient memEnv) `shouldBe` Right (Just memoryQueueBootWarning)
        (mirrorQueuePlanWarning <$> planMirrorQueue (withRegion "us-east-1") sqsEnv) `shouldBe` Right Nothing
        -- The warning names the load-bearing caveats so an operator cannot miss them.
        memoryQueueBootWarning `shouldSatisfy` ("NON-DURABLE" `T.isInfixOf`)
        memoryQueueBootWarning `shouldSatisfy` ("BEST-EFFORT" `T.isInfixOf`)

    it "honours the AWS-standard SQS endpoint override (AWS_ENDPOINT_URL_SQS)" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend (withRegion "us-east-1"){ambientAwsEndpointUrlSqs = Just "http://localhost:4566"} env
        case sqsEndpoint cfg of
            Just ep -> do
                endpointSecure ep `shouldBe` False
                endpointHost ep `shouldBe` "localhost"
                endpointPort ep `shouldBe` 4566
            Nothing -> expectationFailure "expected the endpoint override to resolve"

    it "uses AWS default resolution (no endpoint) when no override is set" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend (withRegion "us-east-1") env
        sqsEndpoint cfg `shouldBe` Nothing

    it "fails fast on a malformed SQS endpoint override" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue (withRegion "us-east-1"){ambientAwsEndpointUrlSqs = Just "not-a-url"} env
            `shouldBe` Left [QueueEndpointMalformed "not-a-url"]
  where
    -- Resolve the SQS config from a plan that must select the SQS backend, failing
    -- the example with the actual plan / boot errors otherwise.
    expectSqsBackend :: AmbientAws -> AppConfig -> IO SqsConfig
    expectSqsBackend ambient env = case planMirrorQueue ambient env of
        Right (SqsBackend cfg) -> pure cfg
        other -> fail ("expected an SQS mirror-queue plan, got: " <> show other)

    noAmbient :: AmbientAws
    noAmbient = AmbientAws Nothing Nothing Nothing

    withRegion :: Text -> AmbientAws
    withRegion r = noAmbient{ambientAwsRegion = Just r}

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
