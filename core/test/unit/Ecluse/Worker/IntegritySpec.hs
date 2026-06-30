{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}

module Ecluse.Worker.IntegritySpec (spec) where

import Crypto.Hash (Blake2b_512, Digest, SHA1, SHA256, SHA384, SHA512, hashlazy)
import Data.Aeson (Key, Value (Object, String), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, secondsToDiffTime)
import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Exception (throwString)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SRI),
    PackageDetails (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
 )
import Ecluse.Core.Package qualified as Pkg
import Ecluse.Core.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
    newInMemoryQueue,
 )
import Ecluse.Core.Registry (
    ParseError (ParseError),
    PublishError (PublishError),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (..),
    UrlFormationError (EmptyBaseUrl),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataUndecodable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Rules (PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence, prepResilience))
import Ecluse.Core.Rules.Types (RuleResult (Allow, Deny))
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Failed, Published))
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Core.Worker (
    IntegrityResult (IntegrityMismatch, IntegrityVerified),
    JobOutcome (Dropped, Retried, Succeeded),
    WorkerM,
    WorkerPolicies,
    WorkerPolicy (WorkerPolicy, wpNow, wpResolveVersion, wpRules),
    WorkerRuntime (WorkerRuntime, wrHeartbeat, wrInjectTraceContext, wrManager, wrMetrics, wrPolicies, wrQueue, wrRegistry, wrTracing),
    heartbeatHealthy,
    lastPoll,
    newWorkerHeartbeat,
    processBatch,
    processJob,
    runWorkerM,
    verifyIntegrity,
    workerHeartbeatStaleAfter,
    workerLoop,
 )
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughWorkerTracingPort, recordingWorkerMetricsPort)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "verifyIntegrity" $ do
        it "verifies a sha1-only artifact against its sha1 (no stronger digest present)" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha512)-only artifact against its sha512" $
            verifyIntegrity (unsafeHash SRI trueSri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies an SRI (sha384)-only artifact against its sha384 (the worker computes sha384)" $
            -- The whole point of modelling sha384: the worker must be able to RECOMPUTE it,
            -- so a sha384-admitted artifact is verifiable rather than admit-but-uncomputable.
            verifyIntegrity (unsafeHash SRI trueSha384Sri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies a raw SHA384-tagged digest against its hex sha384 (the tag arm, not SRI)" $
            -- A digest carried under the raw 'SHA384' tag (hex), distinct from the same hash
            -- inside an SRI string: the worker computes hex SHA-384 and matches it.
            verifyIntegrity (unsafeHash Pkg.SHA384 trueSha384Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha384 SRI that does not match the fetched bytes (tamper guard)" $
            -- The tamper direction for the new compute path: a real, well-formed sha384 that
            -- is the digest of OTHER bytes must fail closed, naming the algorithm.
            verifyIntegrity (unsafeHash SRI falseSha384Sri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha384 digest did not match the fetched bytes"

        it "prefers and verifies a co-present sha384 over a matching sha1 (strongest wins, and is computable)" $
            -- sha384 outranks sha1, so the gate selects it; because the worker can now compute
            -- sha384 it verifies against it rather than failing closed or downgrading to sha1.
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSha384Sri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "verifies a raw SHA512-tagged digest against its hex sha512 (the tag arm, not SRI)" $
            -- A digest carried under the raw 'SHA512' tag (hex), distinct from the same
            -- hash inside an SRI string: the worker computes hex SHA-512 and matches it,
            -- so this exercises the SHA512-tag compute arm rather than the SRI path.
            -- (Pkg.SHA512 is the HashAlg constructor; the bare SHA512 here is Crypto's.)
            verifyIntegrity (unsafeHash Pkg.SHA512 trueSha512Hex :| []) tarballBytes `shouldBe` IntegrityVerified

        it "verifies against the strongest digest when both sha512 and sha1 match" $
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI trueSri]) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS bytes that match the weak sha1 but fail the strong sha512 (tamper guard)" $
            -- The security crux of the most-authoritative-digest rule: a collision
            -- against the broken SHA-1 must NOT admit an artifact whose sha512 fails.
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash SRI falseSri]) tarballBytes
                `shouldSatisfy` isMismatch

        it "reports a mismatch when the sole digest does not match" $
            verifyIntegrity (unsafeHash SHA1 wrongSha1 :| []) tarballBytes
                `shouldSatisfy` isMismatch

        it "verifies a blake2b-only digest (the worker now computes blake2b-512)" $
            -- The #409 fix: blake2b ranks at the top tier and the worker now computes it,
            -- so a blake2b-only artifact verifies rather than failing closed as uncomputable.
            verifyIntegrity (unsafeHash Blake2b trueBlake2b :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a blake2b digest that does not match the fetched bytes (tamper guard, the new arm)" $
            -- The tamper direction for the new blake2b compute path: a real, well-formed
            -- blake2b of OTHER bytes must fail, naming the algorithm.
            verifyIntegrity (unsafeHash Blake2b someBlake2b :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the Blake2b digest did not match the fetched bytes"

        it "prefers and verifies a co-present blake2b over a matching sha1 (strongest wins, now computable)" $
            -- blake2b outranks sha1, so the gate selects it; it is now computable, so it
            -- verifies against it rather than failing closed or downgrading to the sha1.
            verifyIntegrity (unsafeHash SHA1 trueSha1 :| [unsafeHash Blake2b trueBlake2b]) tarballBytes
                `shouldBe` IntegrityVerified

        it "verifies a sha256-only digest (the worker now computes sha256, the default floor)" $
            -- The #409 fix on the default config: sha256 is the default public floor and is
            -- now computable, so a sha256-only admitted artifact verifies rather than being
            -- admitted then permanently dropped.
            verifyIntegrity (unsafeHash SHA256 trueSha256 :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha256 digest that does not match the fetched bytes (tamper guard)" $
            verifyIntegrity (unsafeHash SHA256 someSha256 :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SHA256 digest did not match the fetched bytes"

        it "fails closed on an md5-only digest (the worker will not verify a broken hash)" $
            -- MD5 is cryptographically broken, so the worker deliberately will not compute it:
            -- an md5-only artifact fails closed, never admitted on the strength of a forgeable
            -- hash. (MD5 is also below the public floor, so this never arises for an admitted
            -- public job; the fail-closed stance is belt-and-suspenders.)
            mismatchDetail (verifyIntegrity (unsafeHash MD5 someMd5 :| []) tarballBytes)
                `shouldBe` Just "the strongest admitted digest (MD5) is in an algorithm the worker cannot verify"

        it "verifies a sha256 SRI (the worker now computes the sha256 inner algorithm)" $
            verifyIntegrity (unsafeHash SRI trueSha256Sri :| []) tarballBytes `shouldBe` IntegrityVerified

        it "REJECTS a sha256 SRI that does not match the fetched bytes (tamper guard)" $
            verifyIntegrity (unsafeHash SRI someSha256Sri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha256 digest did not match the fetched bytes"

        it "does not downgrade to a matching sha1 when a co-present strong sha256 SRI fails" $
            -- The downgrade guard, multi-digest: a co-present strong sha256 SRI that does NOT
            -- match must OUTRANK a matching SHA-1 the attacker also controls, so the gate
            -- reports the strong digest's mismatch rather than admitting on the weak one.
            mismatchDetail (verifyIntegrity (unsafeHash SRI someSha256Sri :| [unsafeHash SHA1 trueSha1]) tarballBytes)
                `shouldBe` Just "the SRI sha256 digest did not match the fetched bytes"

        it "names the algorithm in a plain (computable) digest mismatch too" $
            -- The non-uncomputable mismatch branch: a sha512 SRI that simply does not
            -- match. The detail names the algorithm via the SRI 'describe' arm, so a
            -- genuine tamper is reported with its algorithm, not anonymously.
            mismatchDetail (verifyIntegrity (unsafeHash SRI falseSri :| []) tarballBytes)
                `shouldBe` Just "the SRI sha512 digest did not match the fetched bytes"

        it "is case-insensitive on the hex shasum" $
            verifyIntegrity (unsafeHash SHA1 (T.toUpper trueSha1) :| []) tarballBytes
                `shouldBe` IntegrityVerified

        it "REJECTS an SRI whose base64 body matches only after case-folding (base64 is case-sensitive)" $
            -- The hex arms fold case (hex is case-insensitive), but an SRI carries a
            -- base64 digest, which is case-sensitive: a body matching the bytes only after
            -- a case change must NOT verify, or the tamper gate is silently weakened.
            verifyIntegrity (unsafeHash SRI caseVariantSri :| []) tarballBytes
                `shouldBe` IntegrityMismatch "the SRI sha512 digest did not match the fetched bytes"
