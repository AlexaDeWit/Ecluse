-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Queue.SqsSpec (spec) where

import Data.Text qualified as T
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (
    ColorStrategy (ColorLog),
    Environment (Environment),
    LogEnv,
    Namespace (Namespace),
    Severity (DebugS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    permitItem,
    registerScribe,
 )
import Katip.Scribes.Handle (jsonFormat, mkHandleScribeWithFormatter)
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (mkPackageName, mkScope)
import Ecluse.Core.Queue (MirrorJob (..), QueueMessage (..), RemoteSpanContext (..), Seconds (..))
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Runtime.Queue.Sqs (
    ReceivedMessage (..),
    SqsConfig (..),
    decodeJob,
    defaultSqsConfig,
    encodeJob,
    liftReceivedMessages,
 )
import Ecluse.Test.Package (unsafeRegistryUrl)
import Ecluse.Test.Support (newTestLogEnv)

-- | An unscoped npm job fixture.
npmJob :: MirrorJob
npmJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "lodash"
        , jobVersion = mkVersion Npm "4.17.21"
        , jobArtifactUrl = unsafeRegistryUrl "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
        , jobArtifactFilename = "lodash-4.17.21.tgz"
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
        , jobArtifactFilename = "core-7.24.0.tgz"
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
        , jobArtifactFilename = "flask-3.0.2.tar.gz"
        , jobTraceContext = Nothing
        }

{- | A job body carrying every required field and __no @traceContext@ key at all__
(a job enqueued with tracing off) -- the shape the optional-carrier decode must
accept, decoding it to a 'Nothing' carrier.
-}
noTraceContextBody :: Text
noTraceContextBody =
    "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"left-pad\",\
    \\"version\":\"1.3.0\",\"artifactUrl\":\"https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz\",\
    \\"filename\":\"left-pad-1.3.0.tgz\"}"

spec :: Spec
spec = do
    describe "encodeJob / decodeJob round-trip" $ do
        it "round-trips an unscoped npm job" $
            decodeJob mkRegistryUrl (encodeJob npmJob) `shouldBe` Right npmJob

        it "round-trips a scoped npm job (scope and bare name both recovered)" $
            decodeJob mkRegistryUrl (encodeJob scopedJob) `shouldBe` Right scopedJob

        it "round-trips a PyPI job (ecosystem carried through)" $
            decodeJob mkRegistryUrl (encodeJob pypiJob) `shouldBe` Right pypiJob

        it "carries every field through unchanged" $ do
            -- Field-by-field so a single mangled field is pinpointed, not lost in
            -- a whole-record comparison.
            case decodeJob mkRegistryUrl (encodeJob npmJob) of
                Left err -> expectationFailure (toString err)
                Right job -> do
                    jobPackage job `shouldBe` jobPackage npmJob
                    jobVersion job `shouldBe` jobVersion npmJob
                    jobArtifactUrl job `shouldBe` jobArtifactUrl npmJob
                    jobArtifactFilename job `shouldBe` jobArtifactFilename npmJob
                    jobTraceContext job `shouldBe` jobTraceContext npmJob

        it "decodes a job body with no traceContext key to a Nothing carrier" $
            -- A job enqueued with tracing off carries no "traceContext" key at all
            -- (not even a null). It must decode to a valid job with no carrier -- the
            -- '.:?'-absent path -- rather than fail, so an absent carrier costs
            -- nothing but the span link.
            case decodeJob mkRegistryUrl noTraceContextBody of
                Left err -> expectationFailure (toString err)
                Right job -> do
                    jobTraceContext job `shouldBe` Nothing
                    jobPackage job `shouldBe` mkPackageName Npm Nothing "left-pad"
                    jobVersion job `shouldBe` mkVersion Npm "1.3.0"

    describe "decodeJob rejects a malformed body" $ do
        it "rejects non-JSON" $
            decodeJob mkRegistryUrl "not json at all" `shouldSatisfy` isLeft

        it "rejects a JSON value that is not an object" $
            decodeJob mkRegistryUrl "[1,2,3]" `shouldSatisfy` isLeft

        it "rejects an object missing a required field" $
            -- No "artifactUrl".
            decodeJob
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"filename\":\"x-1.0.0.tgz\"}"
                `shouldSatisfy` isLeft

        it "rejects an unknown ecosystem, naming it in the error" $
            case decodeJob
                mkRegistryUrl
                "{\"ecosystem\":\"cargo\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"filename\":\"x-1.0.0.tgz\"}" of
                Left err -> err `shouldSatisfy` ("cargo" `T.isInfixOf`)
                Right job -> expectationFailure ("expected a decode error, got " <> show job)

        it "rejects a body with no filename" $
            -- The selection key is mandatory: without it the worker's ingest
            -- re-evaluation has no artifact to gate.
            decodeJob
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\"}"
                `shouldSatisfy` isLeft

        it "rejects a job with a malformed traceContext (missing traceparent)" $
            decodeJob
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\
                \\"filename\":\"x-1.0.0.tgz\",\
                \\"traceContext\":{\"tracestate\":\"ecluse=1\"}}"
                `shouldSatisfy` isLeft

        it "rejects a job with traceContext present but not an object" $
            decodeJob
                mkRegistryUrl
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
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

    describe "liftReceivedMessages -- delivering a batch and logging poison drops" $ do
        it "delivers the well-formed sibling and drops each poison message in the batch" $ do
            logEnv <- newTestLogEnv
            delivered <- liftReceivedMessages logEnv mkRegistryUrl poisonBatch
            -- Only the well-formed message is delivered; the three poison ones are dropped
            -- (omitted from the result and left un-acked for redelivery / dead-lettering).
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

{- | One well-formed message and one of each drop cause (missing body, missing
receipt, undecodable body), each with a distinct message id so the drop log's id
field is assertable. The well-formed and the missing-receipt entries carry valid
bodies, isolating the receipt check from the decode.
-}
poisonBatch :: [ReceivedMessage]
poisonBatch =
    [ ReceivedMessage{rmBody = Just (encodeJob npmJob), rmReceipt = Just "receipt-good", rmMessageId = Just "m-good"}
    , ReceivedMessage{rmBody = Nothing, rmReceipt = Just "receipt-1", rmMessageId = Just "m-no-body"}
    , ReceivedMessage{rmBody = Just (encodeJob scopedJob), rmReceipt = Nothing, rmMessageId = Just "m-no-receipt"}
    , ReceivedMessage{rmBody = Just "not-a-valid-body", rmReceipt = Just "receipt-3", rmMessageId = Just "m-bad-body"}
    ]

{- | A 'LogEnv' with a single stdout scribe in the compact one-line JSON form, every
severity admitted, so a drop line's serialised bytes are assertable through
'captureStdout'.
-}
jsonLogEnv :: IO LogEnv
jsonLogEnv = do
    scribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem DebugS) V2
    base <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    registerScribe "stdout" scribe defaultScribeSettings base

{- | Run an 'IO' action with 'stdout' redirected to a temporary file, returning what
was written, and restore 'stdout' on every exit path, so a test can capture what a
scribe emits without leaking it into the run.
-}
captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-sqs-log.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved
