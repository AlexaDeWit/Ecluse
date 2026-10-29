-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bind worker capabilities to each mount's source, policy, and mirror target.
The composition root owns adapter lookup and credential binding.
-}
module Ecluse.Composition.Worker (
    workerPoliciesWithState,
    mirrorTransportFor,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition (PublishTarget (ptBackendTag, ptCredentials, ptEcosystem, ptMirrorUrl, ptUploadUrl, ptUsername))
import Ecluse.Composition.MirrorState (MirrorStateBinding (msbStore, msbTarget))
import Ecluse.Config (StoreTag (TagCodeArtifact))
import Ecluse.Core.Credential (ClientCredential (ClientCredential), mintSecret)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems), parseEcosystem)
import Ecluse.Core.Registry.Adapter (adapterFor, adapterPublish)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataRead), AdapterPublish (publishCodec, publishMirrorRequirements), MirrorRequirements (mirrorWorkerRead), WorkerMetadataRead (..))
import Ecluse.Core.Registry.Metadata (fetchVersionDetails)
import Ecluse.Core.Registry.Metadata.Fetch (chargingSelectedReads)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), prepareRead)
import Ecluse.Core.Registry.Origin (anonymousOrigin, chargingNativeReads, controlPublicOrigin)
import Ecluse.Core.Registry.Publish (
    MirrorPublish,
    MirrorTransport (MirrorTransport, ptClient, ptLimits, ptMintCredential, ptTracing),
    newMirrorPublish,
 )
import Ecluse.Core.Registry.RubyGems.Credential (rubyCodeArtifactPublishCredential)
import Ecluse.Core.Registry.RubyGems.Publish (rubyPublishCodecWith)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), Origin (UntrustedOrigin), thgPublicHostPort)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Meter (charge, newChargeRegion, withMemoryEntry)
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Context (
    PackumentDeps,
    pdArtifact,
    pdEgressUrl,
    pdFirstParty,
    pdInstallEvidence,
    pdLimits,
    pdMetadata,
    pdMinIntegrity,
    pdNow,
    pdPublicBase,
    pdPublicBaseUrl,
    pdRules,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.InstallEvidence (newTicketInstallEvidenceFlightBudget)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, publicMetadataClient, uncachedPublicMetadataClient)
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (WorkerMetadata))
import Ecluse.Core.Worker (WorkerPolicies, WorkerPolicy (..))
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Runtime.Env (Env, envMemoryMeter, envMetadataCache, envMetrics, envPrivateClient, envPublicClient, envPublicControl, envTelemetry)
import Ecluse.Runtime.Server (MountBinding (bindingPackumentDeps, bindingPrefix))
import Ecluse.Runtime.Telemetry.Instruments (metricsPortOf)
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf)

-- | Hold one ticket across managed retries, with separate charges for overlapping state and byte regions.
workerPoliciesWithState :: Env -> Map Ecosystem MirrorStateBinding -> [MountBinding] -> [PublishTarget] -> Int -> WorkerPolicies
workerPoliciesWithState env states bindings targets artifactMaxBytes =
    Map.fromList
        [ (eco, (workerPolicyFor env requirements deps publish artifactMaxBytes){wpMirrorState = managedState eco deps})
        | binding <- bindings
        , let prefixHead :| _ = bindingPrefix binding
        , let deps = bindingPackumentDeps binding
        , Just eco <- [parseEcosystem prefixHead]
        , Just (publish, requirements) <- [mirrorPublishFor env deps targetsByEcosystem artifactMaxBytes eco]
        ]
  where
    managedState eco deps = do
        binding <- Map.lookup eco states
        let limits = (pdLimits deps){maxMirrorArtifactBytes = artifactMaxBytes}
        pure
            WorkerMirrorState
                { wmsStore = msbStore binding
                , wmsTarget = msbTarget binding
                , wmsLimits = limits
                , wmsWithinBudget = \action ->
                    fmap rightToMaybe . withMemoryEntry (metricsPortOf (envMetrics env)) (envMemoryMeter env) $ \ticket -> do
                        stateCharge <- newChargeRegion (charge ticket)
                        recordCharge <- newChargeRegion (charge ticket)
                        companionCharge <- newChargeRegion (charge ticket)
                        artifactCharge <- newChargeRegion (charge ticket)
                        archiveCharge <- newChargeRegion (charge ticket)
                        sourceCharge <- newChargeRegion (charge ticket)
                        targetCharge <- newChargeRegion (charge ticket)
                        evidenceFlight <- traverse (const (newTicketInstallEvidenceFlightBudget ticket)) (pdInstallEvidence deps)
                        action WorkerStateBudget{wsbState = stateCharge, wsbRecords = recordCharge, wsbCompanions = companionCharge, wsbArtifacts = artifactCharge, wsbArchive = archiveCharge, wsbSource = sourceCharge, wsbTargets = targetCharge, wsbEvidenceFlight = evidenceFlight}
                }
    targetsByEcosystem = Map.fromList [(ptEcosystem target, target) | target <- targets]

-- Resolve the publication bundle once, including its worker read obligations.
mirrorPublishFor :: Env -> PackumentDeps -> Map.Map Ecosystem PublishTarget -> Int -> Ecosystem -> Maybe (MirrorPublish, MirrorRequirements)
mirrorPublishFor env deps targets artifactMaxBytes eco = do
    target <- Map.lookup eco targets
    adapter <- adapterFor eco
    publish <- adapterPublish adapter
    let transport = mirrorTransportFor env deps target
        bounded = transport{ptLimits = (ptLimits transport){maxMirrorArtifactBytes = artifactMaxBytes}}
        codec = case (eco, ptBackendTag target) of
            (RubyGems, TagCodeArtifact) -> rubyPublishCodecWith rubyCodeArtifactPublishCredential (metadataChargeFactors (pdMetadata deps))
            _ -> publishCodec publish
    pure (newMirrorPublish bounded (ptMirrorUrl target) (ptUploadUrl target) codec, publishMirrorRequirements publish)

-- | Use the mount's metadata limits for mirror presence probes and publication.
mirrorTransportFor :: Env -> PackumentDeps -> PublishTarget -> MirrorTransport
mirrorTransportFor env deps target =
    MirrorTransport
        { ptClient = envPrivateClient env
        , ptMintCredential = Just . ClientCredential (ptUsername target) <$> mintSecret (ptCredentials target)
        , ptLimits = pdLimits deps
        , ptTracing = tracingPortOf (envTelemetry env)
        }

-- The public client is anonymous. Worker jobs own their logs and share the mount's artifact host gate.
workerPolicyFor :: Env -> MirrorRequirements -> PackumentDeps -> MirrorPublish -> Int -> WorkerPolicy
workerPolicyFor env requirements deps publish artifactMaxBytes =
    WorkerPolicy
        { wpFirstParty = pdFirstParty deps
        , wpResolveVersion = \pay name version -> client pay >>= \metadata -> fetchVersionDetails metadata name version
        , wpInstallEvidence = pdInstallEvidence deps
        , wpRules = pdRules deps
        , wpMinIntegrity = pdMinIntegrity deps
        , wpArtifactHostHonoured =
            tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps))
        , wpArtifact = pdArtifact deps
        , wpEgressUrl = pdEgressUrl deps
        , wpPublish = publish
        , wpArtifactLimits = (pdLimits deps){maxMirrorArtifactBytes = artifactMaxBytes}
        , wpMirrorState = Nothing
        , wpNow = pdNow deps
        }
  where
    client pay = case mirrorWorkerRead requirements of
        ChargedWorkerRead nativeRead ->
            pure . uncachedPublicMetadataClient . readCapability publicOrigin $
                PrepareRead $ \origin ->
                    fmap (chargingSelectedReads decodedDocumentCharge pay) <$> prepareRead nativeRead origin
        NativeChargedWorkerRead nativeRead ->
            (\origin -> uncachedPublicMetadataClient (readCapability origin nativeRead))
                <$> chargingNativeReads (metadataChargeFactors (pdMetadata deps)) pay publicOrigin
        CachedWorkerRead -> pure (publicMetadataClient (envMetadataCache env) (Source (registryUrlText publicBaseUrl)) (readCapability publicOrigin (metadataRead (pdMetadata deps))))
    readCapability origin ecosystemRead =
        ecosystemMetadataReads
            ecosystemRead
            (tracingPortOf (envTelemetry env))
            (metricsPortOf (envMetrics env))
            (\_ _ -> pure ())
            (\_ _ -> pure ())
            (\_ -> pure ())
            origin

    publicBaseUrl = pdPublicBaseUrl deps

    publicOrigin = controlPublicOrigin (envPublicControl env) (Just WorkerMetadata) (anonymousOrigin (pdLimits deps) (envPublicClient env) (pdPublicBase deps))
