-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Cargo whole-version worker throughput includes native metadata, archives, uploads, and durable completion.
module Ecluse.BenchLoad.Cargo.Worker (cargoWorkerScenario) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Cargo.WorkerFixture
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (benchNow)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), Target, scenario)
import Ecluse.BenchLoad.Worker (NativeWorker (..), WorkerEnvironment (..), bootNativeWorker)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Cargo.Execution (newCargoInstallEvidence)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublishCodec)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Metadata (versionEvaluation)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareVersion)
import Ecluse.Core.Registry.Origin (chargingNativeReads, originClientOf)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker (WorkerPolicy (..))
import Ecluse.Test.Package (defaultMinIntegrity, unsafeFilename)
import Ecluse.Test.Port (passthroughTracingPort)

-- | Matches npm's in-process worker boundary and unlimited-only pod scope.
cargoWorkerScenario :: Scenario
cargoWorkerScenario =
    (scenario "worker-mirroring" description boot){scenarioInProcess = True}
  where
    description = "Unlimited shape only. Measure the worker inside the harness, with an active, nonbinding memory meter budget of " <> show ((maxBound :: Int) `div` 2) <> " bytes. This does not measure a constrained worker process. Run whole-version worker re-admission, native metadata reads, archive extraction, native uploads, conditional S3 state and completion for one crate archive. Reset target and state together, enqueue, and verify outside each recorded job interval but inside the duration window. Archive payload size is uncompressed bytes per file."

boot :: LoadKnobs -> (Target -> IO a) -> IO a
boot knobs use = do
    adapter <- maybe (benchFail "Cargo worker requires measured native charge factors and a registered adapter") pure (adapterFor Cargo)
    fixture <- newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
        native =
            NativeWorker
                { nwSourceApp = sourceApp latency fixture
                , nwTargetApp = targetApp latency fixture
                , nwStateApp = stateApp latency fixture
                , nwPolicy = cargoPolicy adapter
                , nwJob = MirrorJob fixturePackage fixtureVersion (Just (unsafeFilename fixtureTrigger)) Nothing
                , nwExpectedFiles = 1
                , nwReset = resetWorkerFixture fixture
                , nwRejectUpload = writeIORef (wtReject (wfState fixture))
                , nwUploadAttempts = Map.findWithDefault 0 "upload-attempt" <$> readIORef (wtCounts (wfState fixture))
                , nwRemoveTrigger = modifyIORef' (wtFiles (wfState fixture)) (Map.delete fixtureTrigger)
                , nwMissingArtifact = "crate"
                , nwVerifyCopy = verifyCopy fixture
                }
    bootNativeWorker knobs native use

cargoPolicy :: RegistryAdapter -> WorkerEnvironment -> IO WorkerPolicy
cargoPolicy adapter environment = do
    evidence <- newCargoInstallEvidence (ecDeadlines (weClient environment)) defaultLimits
    let factors = metadataChargeFactors (adapterMetadata adapter)
        targetReadRoot = weTargetReadRoot environment
        publish = newMirrorPublish (MirrorTransport (weClient environment) (pure Nothing) defaultLimits passthroughTracingPort) targetReadRoot targetReadRoot (cargoPublishCodec factors)
    pure
        WorkerPolicy
            { wpFirstParty = const False
            , wpResolveVersion = \pay name version -> chargingNativeReads factors pay (weSourceOrigin environment) >>= \origin -> versionEvaluation <$> prepareVersion prepareCargoRead passthroughTracingPort (originClientOf origin) name version
            , wpInstallEvidence = Just evidence
            , wpRules = weRules environment
            , wpMinIntegrity = defaultMinIntegrity
            , wpArtifactHostHonoured = const True
            , wpEgressUrl = Right . loopbackRegistryUrl
            , wpArtifact = adapterArtifact adapter
            , wpPublish = publish
            , wpArtifactLimits = defaultLimits
            , wpMirrorState = Just (weMirrorState environment)
            , wpNow = pure benchNow
            }

verifyCopy :: WorkerFixture -> Int -> Int -> IO ()
verifyCopy fixture published acked = do
    target <- readIORef (wtFiles (wfState fixture))
    counts <- readIORef (wtCounts (wfState fixture))
    let count name = Map.findWithDefault 0 name counts
        files = Map.size (wfFiles fixture)
    unless (published == 1 && acked == 1 && target == wfFiles fixture && count "source-discovery" == 1 && count "source-index" == 1 && count "source-file" == files && count "upload" == files && count "target-discovery" > 0 && count "target-index" == 2 && count "state-write" > 0) $
        benchFail ("Cargo worker incomplete copy: published=" <> show published <> ", acknowledged=" <> show acked <> ", requests=" <> show counts)
