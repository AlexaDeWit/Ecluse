-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Queue.SqsSpec (spec) where

import Data.Text qualified as T
import Katip (closeScribes)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (mkPackageName, mkScope)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob (..),
    QueueMessage (..),
    RemoteSpanContext (..),
    Seconds (..),
    decodeJob,
    defaultDeliveryBudget,
    encodeJob,
 )
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Queue.Sqs (
    ReceivedMessage (..),
    SqsConfig (..),
    deadLetterTerminusOf,
    defaultSqsConfig,
    liftReceivedMessages,
    mirrorJobPackage,
 )
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv)
import Ecluse.Test.Package (unsafeFilename, unsafeRegistryUrl)
import Ecluse.Test.Registry.Npm qualified as NpmFixture

-- | An unscoped npm job fixture.
npmJob :: MirrorJob
npmJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "lodash"
        , jobVersion = mkVersion Npm "4.17.21"
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
        , jobArtifactFilename = unsafeFilename "lodash-4.17.21.tgz"
        , -- A populated trace-context carrier, so the round-trip proves the W3C
          -- traceparent/tracestate survive the wire mapping.
          jobTraceContext =
            Just
                RemoteSpanContext
                    { rscTraceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
                    , rscTracestate = "ecluse=1"
                    }
        }

-- | A scoped npm job fixture, to exercise the scope arm of the wire mapping.
scopedJob :: MirrorJob
scopedJob =
    MirrorJob
        { jobPackage = mkPackageName Npm (Just (mkScope "babel")) "core"
        , jobVersion = mkVersion Npm "7.24.0"
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.npmjs.org/@babel/core/-/core-7.24.0.tgz"
        , jobArtifactFilename = unsafeFilename "core-7.24.0.tgz"
        , -- The absent-carrier case (tracing off at enqueue), so both arms round-trip.
          jobTraceContext = Nothing
        }

-- | A PyPI job fixture: a different ecosystem, no scope.
pypiJob :: MirrorJob
pypiJob =
    MirrorJob
        { jobPackage = mkPackageName PyPI Nothing "Flask"
        , jobVersion = mkVersion PyPI "3.0.2"
        , jobArtifactUrl = unsafeRegistryUrl "https://files.pythonhosted.org/packages/flask-3.0.2.tar.gz"
        , jobArtifactFilename = unsafeFilename "flask-3.0.2.tar.gz"
        , jobTraceContext = Nothing
        }

{- | A job body with every required field and no @traceContext@ key at all, as a job
enqueued with tracing off carries. The decode must accept it as a 'Nothing' carrier.
-}
noTraceContextBody :: Text
noTraceContextBody =
    "{\"ecosystem\":\"npm\",\"name\":\"left-pad\",\
    \\"version\":\"1.3.0\",\"artifactUrl\":\"https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz\",\
    \\"filename\":\"left-pad-1.3.0.tgz\"}"

spec :: Spec
spec = do
    describe "encodeJob / decodeJob round-trip" $ do
        it "round-trips an unscoped npm job" $
            decodeJob mirrorJobPackage mkRegistryUrl (encodeJob npmJob) `shouldBe` Right npmJob

        it "round-trips a scoped npm job (scope and bare name both recovered)" $
            decodeJob mirrorJobPackage mkRegistryUrl (encodeJob scopedJob) `shouldBe` Right scopedJob

        it "round-trips a PyPI job (ecosystem carried through)" $
            decodeJob mirrorJobPackage mkRegistryUrl (encodeJob pypiJob) `shouldBe` Right pypiJob

        it "carries every field through unchanged" $ do
            -- Field-by-field so a single mangled field is pinpointed, not lost in
            -- a whole-record comparison.
            case decodeJob mirrorJobPackage mkRegistryUrl (encodeJob npmJob) of
                Left err -> expectationFailure (toString err)
                Right job -> do
                    jobPackage job `shouldBe` jobPackage npmJob
                    jobVersion job `shouldBe` jobVersion npmJob
                    jobArtifactUrl job `shouldBe` jobArtifactUrl npmJob
                    jobArtifactFilename job `shouldBe` jobArtifactFilename npmJob
                    jobTraceContext job `shouldBe` jobTraceContext npmJob

        it "decodes a job body with no traceContext key to a Nothing carrier" $
            -- A job enqueued with tracing off carries no "traceContext" key, not even a null. It
            -- must decode to a job with no carrier through the '.:?'-absent path, rather than fail.
            case decodeJob mirrorJobPackage mkRegistryUrl noTraceContextBody of
                Left err -> expectationFailure (toString err)
                Right job -> do
                    jobTraceContext job `shouldBe` Nothing
                    jobPackage job `shouldBe` mkPackageName Npm Nothing "left-pad"
                    jobVersion job `shouldBe` mkVersion Npm "1.3.0"

    describe "decodeJob -- the one npm name grammar at the queue trust boundary" $ do
        -- The payload is untrusted, so its wire name is read through the same splitter the front
        -- door uses ('mirrorJobPackage'). The verdicts are the shared table's.
        for_ NpmFixture.npmNameVerdicts $ \(raw, valid) ->
            it (NpmFixture.nameVerdictLabel raw valid) $
                isRight (decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "npm" raw)) `shouldBe` valid

        it "reads a scoped name back from the single rendered wire name" $
            case decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "npm" "@babel/core") of
                Left err -> expectationFailure (toString err)
                Right job -> jobPackage job `shouldBe` mkPackageName Npm (Just (mkScope "babel")) "core"

        it "names the unusable component when it refuses an npm name" $
            case decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "npm" "@scope/p@g") of
                Left err -> err `shouldSatisfy` ("unusable npm name component" `T.isInfixOf`)
                Right job -> expectationFailure ("expected a decode error, got " <> show job)

        it "takes a PyPI name as given: PyPI has no scope grammar to read it through" $
            -- The same spelling npm refuses, kept because no PyPI grammar rejects it.
            decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "pypi" "a b") `shouldSatisfy` isRight

        it "takes a RubyGems name as given: RubyGems has no scope grammar either" $
            decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "rubygems" "a b") `shouldSatisfy` isRight

    describe "decodeJob rejects a malformed body" $ do
        it "rejects non-JSON" $
            decodeJob mirrorJobPackage mkRegistryUrl "not json at all" `shouldSatisfy` isLeft

        it "rejects a JSON value that is not an object" $
            decodeJob mirrorJobPackage mkRegistryUrl "[1,2,3]" `shouldSatisfy` isLeft

        it "rejects an object missing a required field" $
            -- No "artifactUrl".
            decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"filename\":\"x-1.0.0.tgz\"}"
                `shouldSatisfy` isLeft

        it "rejects an unknown ecosystem, naming it in the error" $
            case decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"cargo\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"filename\":\"x-1.0.0.tgz\"}" of
                Left err -> err `shouldSatisfy` ("cargo" `T.isInfixOf`)
                Right job -> expectationFailure ("expected a decode error, got " <> show job)

        it "rejects a body with no filename" $
            -- The selection key is mandatory: without it the worker's ingest
            -- re-evaluation has no artifact to gate.
            decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\"}"
                `shouldSatisfy` isLeft

        it "rejects an artifact filename that is not a safe path component" $
            -- The filename is interpolated into an upstream path, so a traversal in the
            -- payload is refused at the boundary rather than carried into a fetch.
            decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\
                \\"artifactUrl\":\"https://registry.npmjs.org/x/-/x-1.0.0.tgz\",\
                \\"filename\":\"../../etc/passwd\"}"
                `shouldSatisfy` isLeft

        it "rejects a job with a malformed traceContext (missing traceparent)" $
            decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\
                \\"filename\":\"x-1.0.0.tgz\",\
                \\"traceContext\":{\"tracestate\":\"ecluse=1\"}}"
                `shouldSatisfy` isLeft

        it "rejects a job with traceContext present but not an object" $
            decodeJob
                mirrorJobPackage
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\
                \\"filename\":\"x-1.0.0.tgz\",\
                \\"traceContext\":\"just-a-string\"}"
                `shouldSatisfy` isLeft

    describe "defaultSqsConfig" $ do
        let cfg = defaultSqsConfig "https://sqs.example/q" "us-east-1"
        it "carries the queue URL and region through" $ do
            sqsQueueUrl cfg `shouldBe` "https://sqs.example/q"
            sqsRegion cfg `shouldBe` "us-east-1"
        it "defaults to no endpoint override (real AWS / ambient credentials)" $
            sqsEndpoint cfg `shouldBe` Nothing
        it "defaults the batch size to a full SQS batch of 10" $
            sqsBatchSize cfg `shouldBe` 10
        it "defaults the long-poll window to the SQS maximum of 20 seconds" $
            sqsWaitSeconds cfg `shouldBe` 20
        it "defaults the visibility timeout to 30 seconds" $
            sqsVisibilityTimeout cfg `shouldBe` Seconds 30
        it "defaults the redelivery budget to the shared shipped value" $
            -- The floor an unconfigured backend runs on. A configured deployment gets
            -- the operator's ECLUSE_QUEUE__MAX_RECEIVE_COUNT here instead.
            sqsMaxReceiveCount cfg `shouldBe` defaultDeliveryBudget

    describe "deadLetterTerminusOf -- reading the queue's redrive policy (issue #935)" $ do
        it "reports no terminus when the queue carries no redrive policy" $
            -- SQS omits an unset attribute entirely, so an absent value is the
            -- no-dead-letter-queue case the boot warning exists for.
            deadLetterTerminusOf Nothing `shouldBe` TerminusAbsent

        it "reports no terminus for a blank policy value" $
            deadLetterTerminusOf (Just "   ") `shouldBe` TerminusAbsent

        it "reads the capture count from a policy that states it as a string" $
            -- The spelling AWS itself returns: the embedded JSON quotes the count.
            deadLetterTerminusOf (Just "{\"deadLetterTargetArn\":\"arn:aws:sqs:us-east-1:123456789012:dlq\",\"maxReceiveCount\":\"10\"}")
                `shouldBe` TerminusAttached (Just (DeliveryBudget 10))

        it "reads the capture count from a policy that states it as a number" $
            -- Emulators and some SDK paths render it unquoted. Both spell one policy.
            deadLetterTerminusOf (Just "{\"deadLetterTargetArn\":\"arn:aws:sqs:us-east-1:123456789012:dlq\",\"maxReceiveCount\":4}")
                `shouldBe` TerminusAttached (Just (DeliveryBudget 4))

        it "still reports a terminus when the policy's count cannot be read" $ do
            -- The boot warning must never fire for an operator who has a dead-letter
            -- queue. Only the capture count is lost, so the configured floor stands.
            deadLetterTerminusOf (Just "{\"deadLetterTargetArn\":\"arn:aws:sqs:us-east-1:123456789012:dlq\"}")
                `shouldBe` TerminusAttached Nothing
            deadLetterTerminusOf (Just "not json at all") `shouldBe` TerminusAttached Nothing

    describe "liftReceivedMessages -- delivering a batch and logging poison drops" $ do
        it "delivers the well-formed sibling and drops each poison message in the batch" $ do
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl poisonBatch
            -- Only the well-formed message is delivered. The three poison ones are dropped and
            -- left un-acked for redelivery or dead-lettering.
            map msgJob delivered `shouldBe` [npmJob]

        it "logs each drop at Debug with its reason and message id, never the body" $ do
            logEnv <- jsonLogEnv
            logged <- captureStdout $ do
                _ <- liftReceivedMessages logEnv mkRegistryUrl poisonBatch
                void (closeScribes logEnv)
            -- One Debug drop line per poison message, tagged with this module.
            T.count "\"sev\":\"Debug\"" logged `shouldBe` 3
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Runtime.Queue.Sqs\""
            logged `shouldSatisfy` T.isInfixOf "missing body"
            logged `shouldSatisfy` T.isInfixOf "missing receipt"
            logged `shouldSatisfy` T.isInfixOf "undecodable body"
            logged `shouldSatisfy` T.isInfixOf "\"messageId\":\"m-no-body\""
            logged `shouldSatisfy` T.isInfixOf "\"messageId\":\"m-no-receipt\""
            logged `shouldSatisfy` T.isInfixOf "\"messageId\":\"m-bad-body\""
            -- The untrusted body of the undecodable message never reaches the log.
            logged `shouldNotSatisfy` T.isInfixOf "not-a-valid-body"

        it "carries the ApproximateReceiveCount through as the delivery count" $ do
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl (map deliveredWithCount [Just "1", Just "3", Just "17"])
            map msgReceiveCount delivered `shouldBe` [1, 3, 17]

        it "reads a missing or unusable count as a first delivery" $ do
            -- Only evidence may put a message past its budget. SQS omits the attribute unless a
            -- request asks for it, and an unusable value says nothing, so neither retires a job.
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl (map deliveredWithCount [Nothing, Just "", Just "not-a-number", Just "0", Just "-4"])
            map msgReceiveCount delivered `shouldBe` [1, 1, 1, 1, 1]

{- | A job body for @ecosystem@ naming @wireName@ in the single @name@ field 'encodeJob' writes.
Every other field is well-formed.
-}
jobBodyFor :: Text -> Text -> Text
jobBodyFor ecosystem wireName =
    "{\"ecosystem\":"
        <> quoted ecosystem
        <> ",\"name\":"
        <> quoted wireName
        <> ",\"version\":\"1.0.0\""
        <> ",\"artifactUrl\":\"https://registry.npmjs.org/x/-/x-1.0.0.tgz\""
        <> ",\"filename\":\"x-1.0.0.tgz\"}"
  where
    quoted t = "\"" <> t <> "\""

{- | One well-formed message and one of each drop cause: missing body, missing receipt,
undecodable body. Distinct message ids make the drop log's id field assertable.
-}
poisonBatch :: [ReceivedMessage]
poisonBatch =
    [ ReceivedMessage{rmBody = Just (encodeJob npmJob), rmReceipt = Just "receipt-good", rmMessageId = Just "m-good", rmReceiveCount = Nothing}
    , ReceivedMessage{rmBody = Nothing, rmReceipt = Just "receipt-1", rmMessageId = Just "m-no-body", rmReceiveCount = Nothing}
    , ReceivedMessage{rmBody = Just (encodeJob scopedJob), rmReceipt = Nothing, rmMessageId = Just "m-no-receipt", rmReceiveCount = Nothing}
    , ReceivedMessage{rmBody = Just "not-a-valid-body", rmReceipt = Just "receipt-3", rmMessageId = Just "m-bad-body", rmReceiveCount = Nothing}
    ]

-- A well-formed received message carrying the given raw @ApproximateReceiveCount@,
-- so a test drives the delivery-count lift without the AWS types.
deliveredWithCount :: Maybe Text -> ReceivedMessage
deliveredWithCount raw =
    ReceivedMessage
        { rmBody = Just (encodeJob npmJob)
        , rmReceipt = Just "receipt-good"
        , rmMessageId = Just "m-good"
        , rmReceiveCount = raw
        }
