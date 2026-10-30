-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native worker throughput includes every platform archive, original quick specification and durable completion.
module Ecluse.BenchLoad.RubyGems.Worker (rubyGemsWorkerScenario) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (benchNow)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), Target, scenario)
import Ecluse.BenchLoad.RubyGems.WorkerFixture
import Ecluse.BenchLoad.Worker (NativeWorker (..), WorkerEnvironment (..), bootNativeWorker)
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..))
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Queue (MirrorJob (..))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMetadata))
import Ecluse.Core.Registry.Metadata (versionEvaluation)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareVersion)
import Ecluse.Core.Registry.Origin (chargingNativeReads, originClientOf)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Registry.RubyGems.Metadata (prepareGemRead)
import Ecluse.Core.Registry.RubyGems.Publish (rubyPublishCodec)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker (WorkerPolicy (..))
import Ecluse.Test.Package (defaultMinIntegrity, unsafeFilename)
import Ecluse.Test.Port (passthroughTracingPort)

-- | Only the unlimited pod shape runs the in-process worker under its active memory meter.
rubyGemsWorkerScenario :: Scenario
rubyGemsWorkerScenario =
    (scenario "worker-mirroring" description boot){scenarioInProcess = True}
  where
    description = "Unlimited shape only. Measure the worker under the shared active memory meter. Copy four native platform archives for one version, including original quick specifications, verified extraction, target uploads, conditional S3 state and final whole-version completion. Ruby builds genuine archives before timing. The payload knob sets uncompressed lib/payload.bin bytes in each archive. Reset and verification stay outside each recorded job interval but inside the duration window."

boot :: LoadKnobs -> (Target -> IO a) -> IO a
boot knobs use = do
    adapter <- maybe (benchFail "RubyGems worker requires measured native charge factors and a registered adapter") pure (adapterFor RubyGems)
    fixture <- newWorkerFixture (lkPayloadBytes knobs)
    let latency = lkUpstreamLatencyMicros knobs
        native =
            NativeWorker
                { nwSourceApp = sourceApp latency fixture
                , nwTargetApp = targetApp latency fixture
                , nwStateApp = stateApp latency fixture
                , nwPolicy = rubyGemsPolicy adapter
                , nwJob = MirrorJob fixturePackage fixtureVersion (Just (unsafeFilename fixtureTrigger)) Nothing
                , nwExpectedFiles = 4
                , nwReset = resetWorkerFixture fixture
                , nwRejectUpload = writeIORef (wtReject (wfState fixture))
                , nwUploadAttempts = Map.findWithDefault 0 "upload-attempt" <$> readIORef (wtCounts (wfState fixture))
                , nwRemoveTrigger = modifyIORef' (wtFiles (wfState fixture)) (Map.delete fixtureTrigger)
                , nwMissingArtifact = "gem"
                , nwVerifyCopy = verifyCopy fixture
                }
    bootNativeWorker knobs native use

rubyGemsPolicy :: RegistryAdapter -> WorkerEnvironment -> IO WorkerPolicy
rubyGemsPolicy adapter environment = do
    let factors = metadataChargeFactors (adapterMetadata adapter)
        target = weTargetReadRoot environment
        publish = newMirrorPublish (MirrorTransport (weClient environment) (pure Nothing) defaultLimits passthroughTracingPort) target target (rubyPublishCodec factors)
    pure
        WorkerPolicy
            { wpFirstParty = const False
            , wpResolveVersion = \pay name version -> chargingNativeReads factors pay (weSourceOrigin environment) >>= \origin -> versionEvaluation <$> prepareVersion prepareGemRead passthroughTracingPort (originClientOf origin) name version
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
    let count label = Map.findWithDefault 0 label counts
    unless (published == 1 && acked == 1 && target == wfFiles fixture && Map.size target == 4 && count "source-info" == 1 && count "source-file" == 4 && count "source-quick" == 4 && count "upload" == 4 && count "target-info" >= 2 && count "state-write" > 0) $
        benchFail ("RubyGems worker incomplete copy: published=" <> show published <> ", acknowledged=" <> show acked <> ", requests=" <> show counts)
