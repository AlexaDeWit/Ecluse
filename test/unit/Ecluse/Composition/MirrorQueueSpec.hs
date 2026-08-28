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
    deadLetterTerminusWarning,
    memoryQueueBootWarning,
    mirrorQueuePlanWarning,
    planMirrorQueue,
    planMirrorRuntime,
 )
import Ecluse.Composition.Support (expectConfig, expectEnv, overrideEnv, staticEnvVars, withoutQueueUrl)
import Ecluse.Config (AppConfig)
import Ecluse.Config.Ambient (AmbientAws (..))
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
 )
import Ecluse.Runtime.Aws.Env (AwsEndpoint (endpointHost, endpointPort, endpointSecure))
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsEndpoint, sqsMaxReceiveCount, sqsQueueUrl, sqsRegion), defaultSqsConfig)

spec :: Spec
spec = do
    mirrorRuntimeSpec
    mirrorQueueSpec
    deadLetterTerminusSpec

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
        -- Mirroring is demand-driven and self-healing, so a missing URL degrades durability,
        -- not safety. Boot rolls over instead of failing.
        env <- expectEnv (withoutQueueUrl staticEnvVars)
        planMirrorQueue noAmbient env `shouldBe` Right MemoryBackend

    it "refuses a Pub/Sub topic resource as not built in this binary (no silent fallback)" $ do
        -- The topic shape names the GCP backend, which has no implementation compiled in, so
        -- boot must报 a clear "not built" error rather than route quietly to another queue.
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
            `shouldBe` Left [QueueEndpointMalformed (mkSecret "not-a-url")]

    it "aggregates a missing region and a malformed override in one report" $ do
        env <- expectEnv staticEnvVars
        planMirrorQueue noAmbient{ambientAwsEndpointUrlSqs = Just "not-a-url"} env
            `shouldBe` Left [QueueRegionMissing, QueueEndpointMalformed (mkSecret "not-a-url")]

    it "carries the configured redelivery budget into the SQS backend's config" $ do
        -- The plan carries the operator's floor to the backend. The backend then
        -- raises it past any attached terminus when it probes the queue.
        env <- expectEnv (overrideEnv "ECLUSE_QUEUE__MAX_RECEIVE_COUNT" "9" staticEnvVars)
        cfg <- expectSqsBackend noAmbient env
        sqsMaxReceiveCount cfg `shouldBe` DeliveryBudget 9

    it "carries the pinned default budget when the operator sets none" $ do
        env <- expectEnv staticEnvVars
        cfg <- expectSqsBackend noAmbient env
        sqsMaxReceiveCount cfg `shouldBe` DeliveryBudget 5
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

deadLetterTerminusSpec :: Spec
deadLetterTerminusSpec = describe "deadLetterTerminusWarning (issue #935)" $ do
    it "warns loudly when a durable queue has nothing to capture a poison message" $ do
        -- With no redrive policy the message cycles until the retention window discards it unseen.
        -- The budget quoted here is the built handle's, deliberately not the plan's floor.
        case deadLetterTerminusWarning (SqsBackend planConfig) (DeliveryBudget 7) (Right TerminusAbsent) of
            Nothing -> expectationFailure "expected a no-terminus warning on the durable backend"
            Just warning -> do
                warning `shouldSatisfy` ("NO DEAD-LETTER TERMINUS" `T.isInfixOf`)
                -- The warning names what happens instead, with the budget that
                -- governs it.
                warning `shouldSatisfy` ("delivered 7 times" `T.isInfixOf`)
                warning `shouldSatisfy` ("redrive policy" `T.isInfixOf`)

    it "stays silent when a terminus is attached" $ do
        deadLetterTerminusWarning (SqsBackend planConfig) (DeliveryBudget 7) (Right (TerminusAttached (Just (DeliveryBudget 3))))
            `shouldBe` Nothing
        deadLetterTerminusWarning (SqsBackend planConfig) (DeliveryBudget 7) (Right (TerminusAttached Nothing))
            `shouldBe` Nothing

    it "warns when the redrive policy could not be read, carrying the fault detail" $
        -- Boot continues on the configured budget. The failed probe leaves that budget
        -- unconfirmed against a terminus's capture count, so the warning says why.
        case deadLetterTerminusWarning (SqsBackend planConfig) (DeliveryBudget 7) (Left probeFault) of
            Nothing -> expectationFailure "expected a warning when the probe faulted"
            Just warning -> do
                warning `shouldSatisfy` ("sqs:GetQueueAttributes" `T.isInfixOf`)
                warning `shouldSatisfy` ("access denied by the emulator" `T.isInfixOf`)

    it "stays silent for the in-memory backend, whose own boot warning already covers it" $
        -- The in-memory backend truly has no terminus, and 'memoryQueueBootWarning' already says
        -- the mirror is non-durable. A second line would dilute it.
        deadLetterTerminusWarning MemoryBackend (DeliveryBudget 7) (Right TerminusAbsent) `shouldBe` Nothing
  where
    -- The plan's configured floor. It differs on purpose from the handle budget the
    -- warning receives, so a reader cannot mistake one for the other.
    planConfig :: SqsConfig
    planConfig =
        (defaultSqsConfig "https://sqs.us-east-1.amazonaws.com/123456789012/mirror" "us-east-1")
            { sqsMaxReceiveCount = DeliveryBudget 2
            }

    probeFault = transportFault TransportUnreachable "access denied by the emulator"
