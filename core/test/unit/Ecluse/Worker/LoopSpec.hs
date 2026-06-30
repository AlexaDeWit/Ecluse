{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}

module Ecluse.Worker.LoopSpec (spec) where

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
    describe "workerLoop -- supervision (one bad iteration must not kill the loop)" $
        it "survives a throwing receive: catches, backs off, and polls again" $ do
            -- A persistently-failing queue: every poll throws. The loop is wrapped in
            -- tryAny, so a throwing iteration must be caught, logged, and retried after a
            -- backoff -- never escape and tear the worker thread down. The witness is the
            -- receive count: more than one call across the window proves the loop polled
            -- AGAIN after the first throw (it recovered), rather than dying on it.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                -- The backoff after a failed iteration is ~1s, so a ~2.5s window admits a
                -- couple of attempts; assert at least a second poll occurred.
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)
