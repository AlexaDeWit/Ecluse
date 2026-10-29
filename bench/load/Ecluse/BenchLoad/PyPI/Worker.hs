-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | PyPI whole-version worker throughput includes native metadata, archives, uploads, and durable completion.
module Ecluse.BenchLoad.PyPI.Worker (pypiWorkerScenario) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (benchNow)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), Target, scenario)
import Ecluse.BenchLoad.PyPI.WorkerFixture
import Ecluse.BenchLoad.Worker (NativeWorker (..), WorkerEnvironment (..), bootNativeWorker)
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact))
import Ecluse.Core.Registry.Metadata (versionEvaluation)
import Ecluse.Core.Registry.Metadata.Fetch (chargingSelectedReads, fetchVersion)
import Ecluse.Core.Registry.Origin (originClientOf)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.PyPI.Metadata (pypiMirrorRead)
import Ecluse.Core.Registry.PyPI.Publish (pypiPublishCodec)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge)
import Ecluse.Core.Worker (WorkerPolicy (..))
import Ecluse.Test.Package (defaultMinIntegrity, unsafeFilename)
import Ecluse.Test.Port (passthroughTracingPort)

-- | Matches npm's in-process worker boundary and unlimited-only pod scope.
pypiWorkerScenario :: Scenario
pypiWorkerScenario =
    (scenario "worker-mirroring" description boot){scenarioInProcess = True}
  where
    description = "Unlimited shape only. Measure the worker inside the harness, with an active, nonbinding memory meter budget of " <> show ((maxBound :: Int) `div` 2) <> " bytes. This does not measure a constrained worker process. Run whole-version worker re-admission, native metadata reads, archive extraction, multipart uploads, conditional S3 state and completion for two wheels and two sdists. Reset target and state together, enqueue, and verify outside each recorded job interval but inside the duration window. Archive payload size is uncompressed bytes per file."

boot :: LoadKnobs -> (Target -> IO a) -> IO a
boot knobs use = do
    fixture <- newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
        state = wfState fixture
        native =
            NativeWorker
                { nwSourceApp = sourceApp latency fixture
                , nwTargetApp = targetApp latency fixture
                , nwStateApp = stateApp latency fixture
                , nwPolicy = pypiPolicy
                , nwJob = MirrorJob fixturePackage fixtureVersion (Just (unsafeFilename fixtureTrigger)) Nothing
                , nwExpectedFiles = 4
                , nwReset = resetWorkerFixture fixture
                , nwRejectUpload = writeIORef (wtReject state)
                , nwUploadAttempts = Map.findWithDefault 0 "upload-attempt" <$> readIORef (wtCounts state)
                , nwRemoveTrigger = modifyIORef' (wtFiles state) (Map.delete fixtureTrigger)
                , nwMissingArtifact = "wheel"
                , nwVerifyCopy = verifyCopy fixture
                }
    bootNativeWorker knobs native use

pypiPolicy :: WorkerEnvironment -> IO WorkerPolicy
pypiPolicy environment =
    pure
        WorkerPolicy
            { wpFirstParty = const False
            , wpResolveVersion = \pay name version -> versionEvaluation <$> fetchVersion (chargingSelectedReads decodedDocumentCharge pay pypiMirrorRead) passthroughTracingPort sourceOrigin name version
            , wpInstallEvidence = Nothing
            , wpRules = weRules environment
            , wpMinIntegrity = defaultMinIntegrity
            , wpArtifactHostHonoured = const True
            , wpEgressUrl = Right . loopbackRegistryUrl
            , wpArtifact = adapterArtifact pypiAdapter
            , wpPublish = publish
            , wpArtifactLimits = defaultLimits
            , wpMirrorState = Just (weMirrorState environment)
            , wpNow = pure benchNow
            }
  where
    targetReadRoot = weTargetReadRoot environment
    uploadRoot = loopbackRegistryUrl (registryUrlText targetReadRoot <> "/upload")
    publish = newMirrorPublish (MirrorTransport (weClient environment) (pure Nothing) defaultLimits passthroughTracingPort) targetReadRoot uploadRoot pypiPublishCodec
    sourceOrigin = originClientOf (weSourceOrigin environment)

verifyCopy :: WorkerFixture -> Int -> Int -> IO ()
verifyCopy fixture published acked = do
    target <- readIORef (wtFiles (wfState fixture))
    counts <- readIORef (wtCounts (wfState fixture))
    let count name = Map.findWithDefault 0 name counts
        files = Map.size (wfFiles fixture)
    unless (published == 1 && acked == 1 && target == wfFiles fixture && count "source-index" == 1 && count "source-file" == files && count "upload" == files && count "target-index" == 2 && count "state-write" > 0) $
        benchFail ("PyPI worker incomplete copy: published=" <> show published <> ", acknowledged=" <> show acked <> ", requests=" <> show counts)
