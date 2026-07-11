-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans #-}

module Ecluse.Worker.LivenessSpec (spec) where

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
    MirrorJob (..),
    MirrorQueue (receive),
    QueueMessage (msgReceipt),
    ReceiptHandle,
    enqueue,
 )
import Ecluse.Core.Queue.Memory (newInMemoryQueue)
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
import Ecluse.Core.Rules.Types (RuleVerdict (Allow, Deny))
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
    describe "heartbeat" $ do
        it "advances the last-successful-poll once the loop has polled the queue" $
            withRuntime (Right ()) $ \runtime _queue _logRef -> do
                pollBefore <- lastPoll (wrHeartbeat runtime)
                pollBefore `shouldBe` Nothing
                -- Run the consume loop briefly against the (empty) queue, then cancel
                -- it. Even an empty long-poll is a healthy poll, so the heartbeat must
                -- have advanced from 'Nothing'.
                _ <- timeout 200000 (runWM runtime (workerLoop testSupervision))
                pollAfter <- lastPoll (wrHeartbeat runtime)
                pollAfter `shouldSatisfy` isJust
    describe "heartbeatHealthy (the /livez staleness rule)" $ do
        it "is healthy before the first poll (the worker is starting, not stalled)" $
            heartbeatHealthy epoch Nothing `shouldBe` True

        it "is healthy for a poll within the staleness window" $
            heartbeatHealthy (addUTCTime 10 epoch) (Just epoch) `shouldBe` True

        it "is unhealthy once the last poll is staler than the threshold" $
            heartbeatHealthy (addUTCTime (workerHeartbeatStaleAfter + 1) epoch) (Just epoch)
                `shouldBe` False
