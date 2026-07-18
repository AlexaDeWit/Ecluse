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
    planMirrorQueue,
    planMirrorRuntime,
 )
import Ecluse.Composition.Support (expectConfig, expectEnv, overrideEnv, staticEnvVars, withoutQueueUrl)
import Ecluse.Config (AppConfig)
import Ecluse.Config.Ambient (AmbientAws (..))
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint, sqsQueueUrl, sqsRegion), SqsEndpoint (endpointHost, endpointPort, endpointSecure))

spec :: Spec
spec = do
    mirrorRuntimeSpec
    mirrorQueueSpec

mirrorRuntimeSpec :: Spec
mirrorRuntimeSpec = describe "planMirrorRuntime" $ do
    it "plans no mirror runtime when no mount mirrors (queue variables never consulted)" $ do
        -- The serve-only deployment boots with no ECLUSE_QUEUE__URL and no
        -- AWS_REGION: the queue selection never runs.
        cfg <- expectConfig [("ECLUSE_MOUNTS__NPM__ENABLED", "true"), ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")] Nothing
        planMirrorRuntime noAmbient' cfg `shouldBe` Right NoMirroring

    it "delegates to the queue selection when a mount mirrors, surfacing its errors" $ do
        -- The same shape failure as planMirrorQueue: the mirroring mount is what
        -- makes the queue configuration load-bearing.
        cfg <- expectConfig (overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" staticEnvVars) Nothing
        planMirrorRuntime noAmbient' cfg `shouldBe` Left [QueueUrlUnrecognised "https://queue.example.test/q"]

    it "plans the SQS backend from the queue URL alone when a mount mirrors (no AWS_REGION)" $ do
        cfg <- expectConfig staticEnvVars Nothing
        case planMirrorRuntime noAmbient' cfg of
            Right (MirrorWith (SqsBackend _)) -> pass
            other -> expectationFailure ("expected an SQS mirror runtime, got: " <> show other)

    it "rolls a mirroring mount with no queue URL over to the in-memory queue" $ do
        cfg <- expectConfig (withoutQueueUrl staticEnvVars) Nothing
        planMirrorRuntime noAmbient' cfg `shouldBe` Right (MirrorWith MemoryBackend)
  where
    noAmbient' :: AmbientAws
    noAmbient' = AmbientAws Nothing Nothing Nothing

mirrorQueueSpec :: Spec
mirrorQueueSpec = describe "planMirrorQueue" $ do
    it "selects the SQS backend from the queue URL's shape, region from the host (no AWS_REGION)" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend noAmbient env
        sqsQueueUrl cfg `shouldBe` "https://sqs.us-east-1.amazonaws.com/123456789012/mirror"
        sqsRegion cfg `shouldBe` "us-east-1"

    it "rolls an absent ECLUSE_QUEUE__URL over to the bounded in-memory queue" $ do
        -- Graceful rollover, never a boot failure: mirroring is demand-driven and
        -- self-healing, so the missing URL degrades durability, not safety. The
        -- depth cap is the memory plan's tenant, allocated after this selection.
        env <- expectEnv (withoutQueueUrl staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Right MemoryBackend

    it "refuses a Pub/Sub topic resource as not built in this binary (no silent fallback)" $ do
        -- The topic shape names the GCP backend, which has no implementation
        -- compiled in; it must route to a clear "not built" error, never quietly
        -- to a different queue.
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE__URL" "projects/acme/topics/mirror" staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Left [QueueProviderUnavailable "pubsub"]

    it "refuses a queue URL whose shape names no backend" $ do
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Left [QueueUrlUnrecognised "https://queue.example.test/q"]

    it "warns loudly on the in-memory rollover, and not on the durable SQS backend" $ do
        memEnv <- expectEnv (withoutQueueUrl staticEnvVars)
        sqsEnv <- expectEnv staticEnvVars
        (mirrorQueuePlanWarning <$> planMirrorQueue noAmbient memEnv) `shouldBe` Right (Just memoryQueueBootWarning)
        (mirrorQueuePlanWarning <$> planMirrorQueue noAmbient sqsEnv) `shouldBe` Right Nothing
        -- The warning names the load-bearing caveats so an operator cannot miss them.
        memoryQueueBootWarning `shouldSatisfy` ("NON-DURABLE" `T.isInfixOf`)
        memoryQueueBootWarning `shouldSatisfy` ("BEST-EFFORT" `T.isInfixOf`)

    it "forces the SQS interpretation under AWS_ENDPOINT_URL_SQS, however the URL is shaped" $ do
        -- The emulator path: a ministack queue URL matches no public shape by
        -- design, so the override picks the backend and AWS_REGION scopes it.
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE__URL" "http://ministack:4566/000000000000/mirror" staticEnvVars)
        cfg <- expectSqsBackend (withRegion "us-east-1"){ambientAwsEndpointUrlSqs = Just "http://localhost:4566"} env
        sqsQueueUrl cfg `shouldBe` "http://ministack:4566/000000000000/mirror"
        sqsRegion cfg `shouldBe` "us-east-1"
        case sqsEndpoint cfg of
            Just ep -> do
                endpointSecure ep `shouldBe` False
                endpointHost ep `shouldBe` "localhost"
                endpointPort ep `shouldBe` 4566
            Nothing -> expectationFailure "expected the endpoint override to resolve"

    it "fails fast when the endpoint override is set with no AWS_REGION" $ do
        -- An emulator or VPC endpoint does not carry a region in its host, so the
        -- ambient region is required exactly (and only) here.
        env <- expectEnv staticEnvVars
        planMirrorQueue noAmbient{ambientAwsEndpointUrlSqs = Just "http://localhost:4566"} env
            `shouldBe` Left [QueueRegionMissing]

    it "treats a blank AWS_REGION under the endpoint override as missing" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue (withRegion "   "){ambientAwsEndpointUrlSqs = Just "http://localhost:4566"} env
            `shouldBe` Left [QueueRegionMissing]

    it "uses AWS default resolution (no endpoint) when no override is set" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend noAmbient env
        sqsEndpoint cfg `shouldBe` Nothing

    it "fails fast on a malformed SQS endpoint override" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue (withRegion "us-east-1"){ambientAwsEndpointUrlSqs = Just "not-a-url"} env
            `shouldBe` Left [QueueEndpointMalformed "not-a-url"]

    it "aggregates a missing region and a malformed override in one report" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue noAmbient{ambientAwsEndpointUrlSqs = Just "not-a-url"} env
            `shouldBe` Left [QueueRegionMissing, QueueEndpointMalformed "not-a-url"]
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
