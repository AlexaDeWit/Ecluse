-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared worker fixture lifetimes, durable completion controls and measured job intervals.
module Ecluse.BenchLoad.Worker (
    NativeWorker (..),
    WorkerEnvironment (..),
    bootNativeWorker,
    runWorkerLoad,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Data.Time (nominalDay)
import GHC.Clock (getMonotonicTime)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Driver (DriveInProcess), LoadKnobs (lkDurationSeconds), Target (Target))
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Package (infoVersions, pkgArtifacts, pkgEcosystem)
import Ecluse.Core.Queue (MirrorJob (..), MirrorQueue (..), QueueMessage)
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Origin (OriginFor, Public, anonymousOrigin, controlPublicOrigin)
import Ecluse.Core.Registry.Public (newPublicControl)
import Ecluse.Core.Registry.Publish (MirrorPublish (mpProbeInventory))
import Ecluse.Core.Registry.Request (mkBaseRequest)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Rules qualified as Rules
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, charge, newChargeRegion, withMemoryEntry)
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Published))
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (wmpMirrorJobProcessed))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (WorkerMetadata))
import Ecluse.Core.Worker (JobOutcome (Retried), WorkerPolicy (wpPublish), WorkerRuntime (..), newWorkerHeartbeat, processBatch, processJob, runWorkerM)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Runtime.Mirror.State.S3 (mirrorStateForEnv)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort, noopWorkerMetricsPort, passthroughWorkerTracingPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Support (expectRight, testMemoryMeter)
import Ecluse.Test.Wai (localhost)

-- | Native providers retain their own archive expectations, upload roots and request assertions.
data NativeWorker = NativeWorker
    { nwSourceApp :: Application
    , nwTargetApp :: Application
    , nwStateApp :: Application
    , nwStateFormat :: MirrorStateFormat
    , nwPolicy :: WorkerEnvironment -> IO WorkerPolicy
    , nwJob :: MirrorJob
    , nwExpectedFiles :: Int
    -- ^ Independent whole-version membership, never derived from a production inventory.
    , nwReset :: IO ()
    , nwRejectUpload :: Bool -> IO ()
    , nwUploadAttempts :: IO Int
    , nwRemoveTrigger :: IO ()
    , nwMissingArtifact :: Text
    -- ^ Native artifact noun for the missing-trigger diagnostic.
    , nwVerifyCopy :: Int -> Int -> IO ()
    -- ^ Check exact native bytes and requests with the observed publication and acknowledgement counts.
    }

-- | The policy receives controlled source reads and durable state under the active memory meter.
data WorkerEnvironment = WorkerEnvironment
    { weClient :: ExchangeClient
    , weSourceOrigin :: OriginFor Public
    , weTargetReadRoot :: RegistryUrl
    -- ^ The native provider chooses its upload root separately.
    , weMirrorState :: WorkerMirrorState
    , weRules :: [PreparedRule]
    }

-- | Own all fixture services and preflight controls before exposing the measured in-process driver.
bootNativeWorker :: LoadKnobs -> NativeWorker -> (Target -> IO a) -> IO a
bootNativeWorker knobs native use =
    testWithApplication (pure (nwSourceApp native)) $ \sourcePort ->
        testWithApplication (pure (nwTargetApp native)) $ \targetPort ->
            testWithApplication (pure (nwStateApp native)) $ \statePort -> do
                env <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
                let endpoint = AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" statePort service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) env
                    target = stateTarget (localhost targetPort)
                withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    store <- mirrorStateForEnv (ecDeadlines client) defaultLimits (nwStateFormat native) "worker-load" "state" target (AWS.once endpoint{AWS.retryCheck = \_ _ -> False})
                    publicControl <- newPublicControl (\_ _ _ -> pass) (\_ _ -> pass)
                    meter <- testMemoryMeter
                    rules <- Rules.prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
                    let sourceOrigin = controlPublicOrigin publicControl (Just WorkerMetadata) (anonymousOrigin defaultLimits client (mkBaseRequest (loopbackRegistryUrl (localhost sourcePort))))
                        environment = WorkerEnvironment client sourceOrigin (loopbackRegistryUrl (localhost targetPort)) (WorkerMirrorState store target defaultLimits (withinWorkerBudget meter)) rules
                    policy <- nwPolicy native environment
                    successes <- newIORef (0 :: Int)
                    acknowledgements <- newIORef (0 :: Int)
                    queue <- newBoundedInMemoryQueue (defaultMemoryQueueConfig 16) (\n -> benchFail (workerLabel native <> " queue dropped a job: " <> show n))
                    heartbeat <- newWorkerHeartbeat
                    logEnv <- newTestLogEnv
                    let recordingQueue = queue{ack = ack queue >=> traverse (\() -> modifyIORef' acknowledgements (+ 1))}
                        metrics = noopWorkerMetricsPort{wmpMirrorJobProcessed = \result -> when (result == Published) (modifyIORef' successes (+ 1))}
                        runtime =
                            WorkerRuntime
                                { wrQueue = recordingQueue
                                , wrClient = client
                                , wrPublicControl = publicControl
                                , wrHeartbeat = heartbeat
                                , wrMetrics = metrics
                                , wrTracing = passthroughWorkerTracingPort
                                , wrInjectTraceContext = id
                                , wrPolicies = Map.singleton (pkgEcosystem (jobPackage (nwJob native))) policy
                                }
                        prepareBatch = do
                            nwReset native
                            writeIORef successes 0
                            writeIORef acknowledgements 0
                            enqueue recordingQueue (nwJob native) >>= expectRight
                            messages <- receive recordingQueue >>= expectRight
                            unless (length messages == 1) (benchFail (workerLabel native <> " did not receive exactly one version trigger"))
                            pure messages
                        runBatch = runWorkerM logEnv mempty runtime . processBatch
                        verifyBatch _ = do
                            published <- readIORef successes
                            acked <- readIORef acknowledgements
                            nwVerifyCopy native published acked
                    preflight native logEnv runtime store (wpPublish policy) prepareBatch runBatch verifyBatch
                    let measured = do
                            latencies <- runWorkerLoad knobs prepareBatch runBatch verifyBatch
                            TIO.hPutStrLn stderr (workerPrefix native <> "-completed-jobs " <> show (length latencies) <> " files-per-job " <> show (nwExpectedFiles native))
                            pure latencies
                    use (Target Nothing (DriveInProcess measured))

workerLabel :: NativeWorker -> Text
workerLabel native = show (pkgEcosystem (jobPackage (nwJob native))) <> " worker"

workerPrefix :: NativeWorker -> Text
workerPrefix native = ecosystemName (pkgEcosystem (jobPackage (nwJob native))) <> "-worker"

preflight :: NativeWorker -> LogEnv -> WorkerRuntime -> MirrorState -> MirrorPublish -> IO [QueueMessage] -> ([QueueMessage] -> IO ()) -> ([QueueMessage] -> IO ()) -> IO ()
preflight native logEnv runtime store publish prepareBatch runBatch verifyBatch = do
    nwRejectUpload native True
    failed <- runWorkerM logEnv mempty runtime (processJob (nwJob native))
    case failed of
        Retried{} -> pass
        _ -> benchFail (workerLabel native <> " preflight accepted a rejected upload: " <> show failed)
    attempts <- nwUploadAttempts native
    unless (attempts > 0) (benchFail (workerLabel native <> " negative preflight never reached a native upload"))
    incomplete <- completion native store publish
    when (incomplete == MirrorComplete) (benchFail (workerLabel native <> " completed a rejected upload"))
    nwRejectUpload native False
    messages <- prepareBatch
    runBatch messages
    verifyBatch messages
    complete <- completion native store publish
    unless (complete == MirrorComplete) (benchFail (workerLabel native <> " preflight lacked durable whole-version completion"))
    nwRemoveTrigger native
    missing <- completion native store publish
    when (missing == MirrorComplete) (benchFail (workerLabel native <> " preflight accepted a missing " <> nwMissingArtifact native))
    TIO.hPutStrLn stderr (workerPrefix native <> "-preflight rejected-upload=incomplete full-copy=complete missing-" <> nwMissingArtifact native <> "=incomplete")

completion :: NativeWorker -> MirrorState -> MirrorPublish -> IO MirrorCompletion
completion native store publish = do
    let job = nwJob native
    stored <- msReadProject store (const pass) (jobPackage job) Nothing >>= expectRight
    record <- case stored of
        StateLoaded _ present -> pure present
        _ -> benchFail (workerLabel native <> " preflight persisted no expected inventory")
    version <- maybe (benchFail (workerLabel native <> " preflight persisted no expected release")) pure (recordVersion (jobVersion job) record)
    unless (Map.size (versionFiles version) == nwExpectedFiles native) (benchFail (workerLabel native <> " preflight lost an expected distribution"))
    target <- mpProbeInventory publish (const pass) (jobPackage job) >>= expectRight
    pure (checkMirrorCompletion version (concatMap (toList . pkgArtifacts) (Map.elems (infoVersions target))))

-- Fixture budgets retain independent charge regions.
withinWorkerBudget :: MemoryMeter -> (WorkerStateBudget -> IO a) -> IO (Maybe a)
withinWorkerBudget meter action = fmap rightToMaybe . withMemoryEntry noopMetricsPort meter $ \ticket -> do
    budget <- WorkerStateBudget <$> region ticket <*> region ticket <*> region ticket <*> region ticket <*> region ticket <*> region ticket <*> region ticket
    action budget
  where
    region = newChargeRegion . charge

-- | Preparation and checks still consume the duration window, as enqueue and receive do for npm.
runWorkerLoad :: LoadKnobs -> IO a -> (a -> IO ()) -> (a -> IO ()) -> IO [Double]
runWorkerLoad knobs prepare work verify = do
    deadline <- (+ fromIntegral (lkDurationSeconds knobs)) <$> getMonotonicTime
    go deadline []
  where
    go deadline acc = do
        now <- getMonotonicTime
        if now >= deadline
            then pure (reverse acc)
            else do
                input <- prepare
                start <- getMonotonicTime
                work input
                end <- getMonotonicTime
                verify input
                go deadline ((end - start) : acc)
