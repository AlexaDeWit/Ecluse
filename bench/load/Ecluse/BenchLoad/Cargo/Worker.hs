-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Cargo whole-version worker throughput includes native metadata, archives, uploads, and durable completion.
module Ecluse.BenchLoad.Cargo.Worker (cargoWorkerScenario) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Data.Time (nominalDay)
import Katip (LogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Cargo.WorkerFixture
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (benchNow)
import Ecluse.BenchLoad.Harness (Driver (DriveInProcess), LoadKnobs (..), Scenario (..), Target (Target), scenario)
import Ecluse.BenchLoad.Worker (runWorkerLoad, withinWorkerBudget)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Package (infoVersions, pkgArtifacts)
import Ecluse.Core.Queue (MirrorJob (..), MirrorQueue (..), QueueMessage)
import Ecluse.Core.Queue.Memory (defaultMemoryQueueConfig, newBoundedInMemoryQueue)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Cargo.Execution (newCargoInstallEvidence)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublishCodec)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Metadata (versionEvaluation)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareVersion)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocChargeSelectedRead), anonymousOrigin, controlPublicOrigin, originClientOf)
import Ecluse.Core.Registry.Public (newPublicControl)
import Ecluse.Core.Registry.Publish (MirrorPublish (mpProbeInventory), MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Registry.Request (mkBaseRequest)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Telemetry.Metrics (MirrorResult (Published))
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (wmpMirrorJobProcessed))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (WorkerMetadata))
import Ecluse.Core.Worker (JobOutcome (Retried), WorkerPolicy (..), WorkerRuntime (..), newWorkerHeartbeat, processBatch, processJob, runWorkerM)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..))
import Ecluse.Runtime.Mirror.State.S3 (mirrorStateForEnv)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (defaultMinIntegrity, unsafeFilename)
import Ecluse.Test.Port (noopWorkerMetricsPort, passthroughTracingPort, passthroughWorkerTracingPort)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Support (expectRight, testMemoryMeter)
import Ecluse.Test.Wai (localhost)

-- | Matches npm's in-process worker boundary and unlimited-only pod scope.
cargoWorkerScenario :: Scenario
cargoWorkerScenario =
    (scenario "worker-mirroring" description boot){scenarioInProcess = True}
  where
    description = "Unlimited shape only. Measure the worker inside the harness, with an active, nonbinding memory meter budget of " <> show ((maxBound :: Int) `div` 2) <> " bytes. This does not measure a constrained worker process. Run whole-version worker re-admission, native metadata reads, archive extraction, native uploads, conditional S3 state and completion for one crate archive. Reset target and state together, enqueue, and verify outside each recorded job interval but inside the duration window. Archive payload size is uncompressed bytes per file."

boot :: LoadKnobs -> (Target -> IO a) -> IO a
boot knobs use = do
    adapter <- maybe (benchFail "Cargo worker requires measured native charge factors and a registered adapter") pure (adapterFor Cargo)
    let factors = metadataChargeFactors (adapterMetadata adapter)
    fixture <- newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
    testWithApplication (pure (sourceApp latency fixture)) $ \sourcePort ->
        testWithApplication (pure (targetApp latency fixture)) $ \targetPort ->
            testWithApplication (pure (stateApp latency fixture)) $ \statePort -> do
                env <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
                let endpoint = AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" statePort service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) env
                    target = stateTarget (localhost targetPort)
                withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    store <- mirrorStateForEnv (ecDeadlines client) defaultLimits "worker-load" "state" target (AWS.once endpoint{AWS.retryCheck = \_ _ -> False})
                    publicControl <- newPublicControl (\_ _ _ -> pass) (\_ _ -> pass)
                    meter <- testMemoryMeter
                    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
                    evidence <- newCargoInstallEvidence defaultLimits
                    successes <- newIORef (0 :: Int)
                    acknowledgements <- newIORef (0 :: Int)
                    queue <- newBoundedInMemoryQueue (defaultMemoryQueueConfig 16) (\n -> benchFail ("Cargo worker queue dropped a job: " <> show n))
                    heartbeat <- newWorkerHeartbeat
                    logEnv <- newTestLogEnv
                    let publish = newMirrorPublish (MirrorTransport client (pure Nothing) defaultLimits passthroughTracingPort) (loopbackRegistryUrl (localhost targetPort)) (loopbackRegistryUrl (localhost targetPort)) (cargoPublishCodec factors)
                        sourceOrigin = originClientOf (controlPublicOrigin publicControl (Just WorkerMetadata) (anonymousOrigin defaultLimits client (mkBaseRequest (loopbackRegistryUrl (localhost sourcePort)))))
                        policy =
                            WorkerPolicy
                                { wpFirstParty = const False
                                , wpResolveVersion = \pay name version -> versionEvaluation <$> prepareVersion prepareCargoRead passthroughTracingPort sourceOrigin{ocChargeFullRead = pay . scaleCharge (cfFullReadPermille factors), ocChargeSelectedRead = pay} name version
                                , wpInstallEvidence = Just evidence
                                , wpRules = rules
                                , wpMinIntegrity = defaultMinIntegrity
                                , wpArtifactHostHonoured = const True
                                , wpEgressUrl = Right . loopbackRegistryUrl
                                , wpArtifact = adapterArtifact adapter
                                , wpPublish = publish
                                , wpArtifactLimits = defaultLimits
                                , wpMirrorState = Just (WorkerMirrorState store target defaultLimits (withinWorkerBudget meter))
                                , wpNow = pure benchNow
                                }
                        recordingQueue = queue{ack = ack queue >=> traverse (\() -> modifyIORef' acknowledgements (+ 1))}
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
                                , wrPolicies = Map.singleton Cargo policy
                                }
                        prepareBatch = do
                            resetWorkerFixture fixture
                            writeIORef successes 0
                            writeIORef acknowledgements 0
                            enqueue recordingQueue job >>= expectRight
                            messages <- receive recordingQueue >>= expectRight
                            unless (length messages == 1) (benchFail "Cargo worker did not receive exactly one version trigger")
                            pure messages
                        runBatch = runWorkerM logEnv mempty runtime . processBatch
                        verifyBatch _ = verifyCopy fixture successes acknowledgements
                    preflight fixture logEnv runtime store publish prepareBatch runBatch verifyBatch
                    let measured = do
                            latencies <- runWorkerLoad knobs prepareBatch runBatch verifyBatch
                            TIO.hPutStrLn stderr ("cargo-worker-completed-jobs " <> show (length latencies) <> " files-per-job 1")
                            pure latencies
                    use (Target Nothing (DriveInProcess measured))

job :: MirrorJob
job = MirrorJob fixturePackage fixtureVersion (Just (unsafeFilename fixtureTrigger)) Nothing

verifyCopy :: WorkerFixture -> IORef Int -> IORef Int -> IO ()
verifyCopy fixture successes acknowledgements = do
    published <- readIORef successes
    acked <- readIORef acknowledgements
    target <- readIORef (wfTarget fixture)
    counts <- readIORef (wfCounts fixture)
    let count name = Map.findWithDefault 0 name counts
        files = Map.size (wfFiles fixture)
    unless (published == 1 && acked == 1 && target == wfFiles fixture && count "source-discovery" == 1 && count "source-index" == 1 && count "source-file" == files && count "upload" == files && count "target-discovery" > 0 && count "target-index" == 2 && count "state-write" > 0) $
        benchFail ("Cargo worker incomplete copy: published=" <> show published <> ", acknowledged=" <> show acked <> ", requests=" <> show counts)

preflight :: WorkerFixture -> LogEnv -> WorkerRuntime -> MirrorState -> MirrorPublish -> IO [QueueMessage] -> ([QueueMessage] -> IO ()) -> ([QueueMessage] -> IO ()) -> IO ()
preflight fixture logEnv runtime store publish prepareBatch runBatch verifyBatch = do
    writeIORef (wfReject fixture) True
    failed <- runWorkerM logEnv mempty runtime (processJob job)
    case failed of
        Retried{} -> pass
        _ -> benchFail ("Cargo worker preflight accepted a rejected upload: " <> show failed)
    counts <- readIORef (wfCounts fixture)
    unless (Map.findWithDefault 0 "upload-attempt" counts > 0) (benchFail "Cargo worker negative preflight never reached a native upload")
    incomplete <- completion store publish
    when (incomplete == MirrorComplete) (benchFail "Cargo worker completed a rejected upload")
    writeIORef (wfReject fixture) False
    messages <- prepareBatch
    runBatch messages
    verifyBatch messages
    complete <- completion store publish
    unless (complete == MirrorComplete) (benchFail "Cargo worker preflight lacked durable whole-version completion")
    modifyIORef' (wfTarget fixture) (Map.delete fixtureTrigger)
    missing <- completion store publish
    when (missing == MirrorComplete) (benchFail "Cargo worker preflight accepted a missing crate")
    TIO.hPutStrLn stderr "cargo-worker-preflight rejected-upload=incomplete full-copy=complete missing-crate=incomplete"

completion :: MirrorState -> MirrorPublish -> IO MirrorCompletion
completion store publish = do
    stored <- msReadProject store (const pass) fixturePackage Nothing >>= expectRight
    record <- case stored of
        StateLoaded _ present -> pure present
        _ -> benchFail "Cargo worker preflight persisted no expected inventory"
    version <- maybe (benchFail "Cargo worker preflight persisted no expected release") pure (recordVersion fixtureVersion record)
    unless (Map.size (versionFiles version) == 1) (benchFail "Cargo worker preflight lost an expected distribution")
    target <- mpProbeInventory publish (const pass) fixturePackage >>= expectRight
    pure (checkMirrorCompletion version (concatMap (toList . pkgArtifacts) (Map.elems (infoVersions target))))
