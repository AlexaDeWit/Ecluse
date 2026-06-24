module Ecluse.Queue.SqsSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Package (Hash (Hash), HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI), mkPackageName, mkScope)
import Ecluse.Queue (MirrorArtifact (..), MirrorJob (..), Seconds (..))
import Ecluse.Queue.Sqs (
    SqsConfig (..),
    decodeJob,
    defaultSqsConfig,
    encodeJob,
 )
import Ecluse.Version (mkVersion)

{- | An unscoped npm job fixture, carrying both an SRI and a SHA-1 digest so the
multi-digest arm of the artifact wire mapping is exercised.
-}
npmJob :: MirrorJob
npmJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "lodash"
        , jobVersion = mkVersion Npm "4.17.21"
        , jobArtifactUrl = "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
        , jobMirrorTarget = "https://mirror.example/lodash/-/lodash-4.17.21.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "lodash-4.17.21.tgz"
                , maHashes = Hash SRI "sha512-abc" :| [Hash SHA1 "deadbeef"]
                , maSize = Just 1234
                }
        }

-- | A scoped npm job fixture, to exercise the scope arm of the wire mapping.
scopedJob :: MirrorJob
scopedJob =
    MirrorJob
        { jobPackage = mkPackageName Npm (Just (mkScope "babel")) "core"
        , jobVersion = mkVersion Npm "7.24.0"
        , jobArtifactUrl = "https://registry.npmjs.org/@babel/core/-/core-7.24.0.tgz"
        , jobMirrorTarget = "https://mirror.example/@babel/core/-/core-7.24.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "core-7.24.0.tgz"
                , maHashes = Hash SRI "sha512-xyz" :| []
                , maSize = Nothing
                }
        }

-- | A PyPI job fixture: a different ecosystem, no scope.
pypiJob :: MirrorJob
pypiJob =
    MirrorJob
        { jobPackage = mkPackageName PyPI Nothing "Flask"
        , jobVersion = mkVersion PyPI "3.0.2"
        , jobArtifactUrl = "https://files.pythonhosted.org/packages/flask-3.0.2.tar.gz"
        , jobMirrorTarget = "https://mirror.example/flask-3.0.2.tar.gz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "flask-3.0.2.tar.gz"
                , maHashes = Hash SHA1 "0a4d" :| []
                , maSize = Just 9001
                }
        }

spec :: Spec
spec = do
    describe "encodeJob / decodeJob round-trip" $ do
        it "round-trips an unscoped npm job" $
            decodeJob (encodeJob npmJob) `shouldBe` Right npmJob

        it "round-trips a scoped npm job (scope and bare name both recovered)" $
            decodeJob (encodeJob scopedJob) `shouldBe` Right scopedJob

        it "round-trips a PyPI job (ecosystem carried through)" $
            decodeJob (encodeJob pypiJob) `shouldBe` Right pypiJob

        it "carries every field through unchanged" $ do
            -- Field-by-field so a single mangled field is pinpointed, not lost in
            -- a whole-record comparison.
            case decodeJob (encodeJob npmJob) of
                Left err -> expectationFailure (toString err)
                Right job -> do
                    jobPackage job `shouldBe` jobPackage npmJob
                    jobVersion job `shouldBe` jobVersion npmJob
                    jobArtifactUrl job `shouldBe` jobArtifactUrl npmJob
                    jobMirrorTarget job `shouldBe` jobMirrorTarget npmJob
                    jobArtifact job `shouldBe` jobArtifact npmJob

        it "round-trips every hash algorithm's wire name (encode/decode are inverse over all algs)" $
            -- A job whose artifact carries one digest of EACH algorithm exercises both
            -- directions of the wire mapping (hashAlgName on encode, parseHashAlg on
            -- decode) for sha1/sha256/sha512/md5/blake2b/sri — so a future algorithm
            -- whose two halves disagree is caught here rather than silently dropping a
            -- digest the worker would later need to verify against.
            let allAlgsJob =
                    npmJob
                        { jobArtifact =
                            (jobArtifact npmJob)
                                { maHashes =
                                    Hash SHA1 "aa"
                                        :| [ Hash SHA256 "bb"
                                           , Hash SHA512 "cc"
                                           , Hash MD5 "dd"
                                           , Hash Blake2b "ee"
                                           , Hash SRI "sha512-ff"
                                           ]
                                }
                        }
             in decodeJob (encodeJob allAlgsJob) `shouldBe` Right allAlgsJob

    describe "decodeJob rejects a malformed body" $ do
        it "rejects non-JSON" $
            decodeJob "not json at all" `shouldSatisfy` isLeft

        it "rejects a JSON value that is not an object" $
            decodeJob "[1,2,3]" `shouldSatisfy` isLeft

        it "rejects an object missing a required field" $
            -- No "mirrorTarget".
            decodeJob
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\"}"
                `shouldSatisfy` isLeft

        it "rejects an unknown ecosystem, naming it in the error" $
            case decodeJob
                "{\"ecosystem\":\"cargo\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"mirrorTarget\":\"m\"}" of
                Left err -> err `shouldSatisfy` ("cargo" `T.isInfixOf`)
                Right job -> expectationFailure ("expected a decode error, got " <> show job)

        it "rejects a body with no artifact descriptor" $
            -- The serve-time-admitted digest is mandatory: a body without it has
            -- nothing to verify the fetched bytes against.
            decodeJob
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"mirrorTarget\":\"m\"}"
                `shouldSatisfy` isLeft

        it "rejects an artifact carrying an empty hash list (the NonEmpty invariant)" $
            decodeJob
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"mirrorTarget\":\"m\",\
                \\"artifact\":{\"filename\":\"x-1.0.0.tgz\",\"hashes\":[],\"size\":null}}"
                `shouldSatisfy` isLeft

        it "rejects an unknown hash algorithm, naming it in the error" $
            -- A digest whose algorithm the worker does not recognise cannot be used to
            -- verify the fetched bytes, so the job is rejected at decode rather than
            -- admitted with an unverifiable digest. The error names the offending alg.
            case decodeJob
                "{\"ecosystem\":\"npm\",\"scope\":null,\"name\":\"x\",\
                \\"version\":\"1.0.0\",\"artifactUrl\":\"u\",\"mirrorTarget\":\"m\",\
                \\"artifact\":{\"filename\":\"x-1.0.0.tgz\",\
                \\"hashes\":[{\"alg\":\"crc32\",\"value\":\"deadbeef\"}],\"size\":null}}" of
                Left err -> err `shouldSatisfy` ("crc32" `T.isInfixOf`)
                Right job -> expectationFailure ("expected a decode error, got " <> show job)

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
