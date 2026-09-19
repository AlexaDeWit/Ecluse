-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Queue.SqsSpec (spec) where

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
import Ecluse.Core.Queue.Lease (MonoTime (MonoTime), ReceiptLease, receiptLease)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Queue.Sqs.Internal (
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

-- | A scoped npm job fixture, to exercise the namespace arm of the wire mapping.
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

{- | A namespaced non-npm job fixture. No ecosystem ships namespaced names beside npm today, but
the wire mapping is the one every backend inherits, so the namespace must survive the hop.
-}
namespacedPypiJob :: MirrorJob
namespacedPypiJob =
    pypiJob{jobPackage = mkPackageName PyPI (Just (mkScope "acme")) "Flask"}

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
        for_ roundTripJobs $ \(label, job) ->
            it (toString label) $
                decodeJob mirrorJobPackage mkRegistryUrl (encodeJob job) `shouldBe` Right job

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
        -- The payload is untrusted, so its namespace and name are re-joined and read through the
        -- same splitter the front door uses ('mirrorJobPackage'). The verdicts are the shared table's.
        for_ NpmFixture.npmNameVerdicts $ \(raw, valid) ->
            it (NpmFixture.nameVerdictLabel raw valid) $
                isRight (decodeJob mirrorJobPackage mkRegistryUrl (jobBodyFor "npm" raw)) `shouldBe` valid

        it "rebuilds a scoped name from the payload's separate namespace and name fields" $
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

    describe "defaultSqsConfig" $
        it "carries the queue URL and region through, and ships every other knob defaulted" $ do
            -- The floor an unconfigured backend runs on. A configured deployment overrides
            -- the receive count with the operator's ECLUSE_QUEUE__MAX_RECEIVE_COUNT.
            let cfg = defaultSqsConfig "https://sqs.example/q" "us-east-1"
            sqsQueueUrl cfg `shouldBe` "https://sqs.example/q"
            sqsRegion cfg `shouldBe` "us-east-1"
            sqsEndpoint cfg `shouldBe` Nothing
            sqsBatchSize cfg `shouldBe` 10
            sqsWaitSeconds cfg `shouldBe` 20
            sqsVisibilityTimeout cfg `shouldBe` Seconds 30
            sqsMaxReceiveCount cfg `shouldBe` defaultDeliveryBudget

    describe "deadLetterTerminusOf -- reading the queue's redrive policy" $
        for_ redrivePolicies $ \(label, policy, expected) ->
            it (toString label) $
                deadLetterTerminusOf policy `shouldBe` expected

    describe "liftReceivedMessages -- delivering a batch and logging poison drops" $ do
        it "delivers the well-formed sibling and drops each poison message in the batch" $ do
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl testLease poisonBatch
            -- Only the well-formed message is delivered. The three poison ones are dropped and
            -- left un-acked for redelivery or dead-lettering.
            map msgJob delivered `shouldBe` [npmJob]

        it "logs each drop at Debug with its reason and message id, never the body" $ do
            logEnv <- jsonLogEnv
            logged <- captureStdout $ do
                _ <- liftReceivedMessages logEnv mkRegistryUrl testLease poisonBatch
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
            delivered <- liftReceivedMessages logEnv mkRegistryUrl testLease (map deliveredWithCount [Just "1", Just "3", Just "17"])
            map msgReceiveCount delivered `shouldBe` [1, 3, 17]

        it "carries the poll's lease on every delivered message, so the worker can renew it" $ do
            -- Without it the worker has no deadline to renew against, and every job races the
            -- visibility window it was received under.
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl testLease (map deliveredWithCount [Just "1", Just "2"])
            map msgLease delivered `shouldBe` [Just testLease, Just testLease]

        it "reads a missing or unusable count as a first delivery" $ do
            -- Only evidence may put a message past its budget. SQS omits the attribute unless a
            -- request asks for it, and an unusable value says nothing, so neither retires a job.
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl testLease (map deliveredWithCount [Nothing, Just "", Just "not-a-number", Just "0", Just "-4"])
            map msgReceiveCount delivered `shouldBe` [1, 1, 1, 1, 1]

{- | One job per arm of the wire mapping: the two npm shapes, a second ecosystem, and a
namespaced non-npm name no ecosystem ships today.
-}
roundTripJobs :: [(Text, MirrorJob)]
roundTripJobs =
    [ ("round-trips an unscoped npm job", npmJob)
    , ("round-trips a scoped npm job (namespace and bare name both recovered)", scopedJob)
    , ("round-trips a PyPI job (ecosystem carried through)", pypiJob)
    , ("round-trips a namespaced non-npm job, so the namespace is not npm's alone", namespacedPypiJob)
    ]

{- | What each redrive-policy spelling says about the terminus. A policy the reader cannot
parse still reports a terminus, so the boot warning never fires for an operator who has one.
-}
redrivePolicies :: [(Text, Maybe Text, DeadLetterTerminus)]
redrivePolicies =
    [ ("reports no terminus when the queue carries no redrive policy", Nothing, TerminusAbsent)
    , ("reports no terminus for a blank policy value", Just "   ", TerminusAbsent)
    , ("reads the capture count from a policy that states it as a string", policyWith "\"10\"", TerminusAttached (Just (DeliveryBudget 10)))
    , ("reads the capture count from a policy that states it as a number", policyWith "4", TerminusAttached (Just (DeliveryBudget 4)))
    , ("refuses a hex capture count rather than read a number nobody wrote", policyWith "\"0x10\"", TerminusAttached Nothing)
    , ("refuses a padded capture count rather than read a number nobody wrote", policyWith "\" 10\"", TerminusAttached Nothing)
    , ("still reports a terminus when the policy states no count", Just ("{" <> targetArn <> "}"), TerminusAttached Nothing)
    , ("still reports a terminus when the policy is not JSON at all", Just "not json at all", TerminusAttached Nothing)
    ]

-- A redrive policy naming a dead-letter queue and stating the capture count verbatim.
policyWith :: Text -> Maybe Text
policyWith count = Just ("{" <> targetArn <> ",\"maxReceiveCount\":" <> count <> "}")

targetArn :: Text
targetArn = "\"deadLetterTargetArn\":\"arn:aws:sqs:us-east-1:123456789012:dlq\""

{- | The lease one poll's batch is delivered under: a thirty-second window from a fixed origin,
with SQS's twelve-hour ceiling on the receipt.
-}
testLease :: ReceiptLease
testLease = receiptLease (MonoTime 1000) (Seconds 30) (Seconds 43_200)

{- | A job body for @ecosystem@ naming @wireName@, split into the separate @namespace@ and @name@
fields 'encodeJob' writes. Every other field is well-formed.
-}
jobBodyFor :: Text -> Text -> Text
jobBodyFor ecosystem wireName =
    "{\"ecosystem\":"
        <> quoted ecosystem
        <> ",\"namespace\":"
        <> namespaceField
        <> ",\"name\":"
        <> quoted bare
        <> ",\"version\":\"1.0.0\""
        <> ",\"artifactUrl\":\"https://registry.npmjs.org/x/-/x-1.0.0.tgz\""
        <> ",\"filename\":\"x-1.0.0.tgz\"}"
  where
    (namespaceField, bare) = case T.breakOn "/" wireName of
        (namespacePart, rest)
            | Just basePart <- T.stripPrefix "/" rest ->
                (quoted (fromMaybe namespacePart (T.stripPrefix "@" namespacePart)), basePart)
        _ -> ("null", wireName)
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
