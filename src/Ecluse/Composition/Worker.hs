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

import Ecluse.Composition (PublishTarget (ptCredentials, ptEcosystem, ptMirrorUrl, ptUploadUrl, ptUsername))
import Ecluse.Composition.MirrorState (MirrorStateBinding (msbStore, msbTarget))
import Ecluse.Core.Credential (ClientCredential (ClientCredential), mintSecret)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI), parseEcosystem)
import Ecluse.Core.Registry.Adapter (adapterFor, adapterPublish)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead), AdapterPublish (publishCodec))
import Ecluse.Core.Registry.Metadata (fetchVersionDetails)
import Ecluse.Core.Registry.Metadata.Fetch (chargingSelectedReads)
import Ecluse.Core.Registry.Origin (anonymousOrigin, controlPublicOrigin)
import Ecluse.Core.Registry.Publish (
    MirrorPublish,
    MirrorTransport (MirrorTransport, ptClient, ptLimits, ptMintCredential, ptTracing),
    newMirrorPublish,
 )
import Ecluse.Core.Registry.PyPI.Metadata (pypiMirrorRead)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), Origin (UntrustedOrigin), thgPublicHostPort)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Meter (charge, newChargeRegion, withMemoryEntry)
import Ecluse.Core.Server.Cache (Source (Source))
import Ecluse.Core.Server.Context (
    PackumentDeps,
    pdArtifact,
    pdEgressUrl,
    pdFirstParty,
    pdLimits,
    pdMetadata,
    pdMinIntegrity,
    pdNow,
    pdPublicBaseUrl,
    pdRules,
    pdTarballHostGate,
    tarballHostHonoured,
 )
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
        [ (eco, (workerPolicyFor env eco deps publish artifactMaxBytes){wpMirrorState = managedState eco deps})
        | binding <- bindings
        , let prefixHead :| _ = bindingPrefix binding
        , let deps = bindingPackumentDeps binding
        , Just eco <- [parseEcosystem prefixHead]
        , Just publish <- [mirrorPublishFor env deps targetsByEcosystem artifactMaxBytes eco]
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
                    withMemoryEntry (metricsPortOf (envMetrics env)) (envMemoryMeter env) $ \ticket -> do
                        stateCharge <- newChargeRegion (charge ticket)
                        recordCharge <- newChargeRegion (charge ticket)
                        companionCharge <- newChargeRegion (charge ticket)
                        artifactCharge <- newChargeRegion (charge ticket)
                        archiveCharge <- newChargeRegion (charge ticket)
                        sourceCharge <- newChargeRegion (charge ticket)
                        targetCharge <- newChargeRegion (charge ticket)
                        action WorkerStateBudget{wsbState = stateCharge, wsbRecords = recordCharge, wsbCompanions = companionCharge, wsbArtifacts = artifactCharge, wsbArchive = archiveCharge, wsbSource = sourceCharge, wsbTargets = targetCharge}
                }
    targetsByEcosystem = Map.fromList [(ptEcosystem target, target) | target <- targets]

{- Marry one ecosystem's mirror write to the shared publish transport. 'Nothing' when it
resolves no publish target or no adapter, so the caller wires no half-publish bundle. -}
mirrorPublishFor :: Env -> PackumentDeps -> Map.Map Ecosystem PublishTarget -> Int -> Ecosystem -> Maybe MirrorPublish
mirrorPublishFor env deps targets artifactMaxBytes eco = do
    target <- Map.lookup eco targets
    adapter <- adapterFor eco
    publish <- adapterPublish adapter
    let transport = mirrorTransportFor env deps target
        bounded = transport{ptLimits = (ptLimits transport){maxMirrorArtifactBytes = artifactMaxBytes}}
    pure (newMirrorPublish bounded (ptMirrorUrl target) (ptUploadUrl target) (publishCodec publish))

-- | Use the mount's metadata limits for mirror presence probes and publication.
mirrorTransportFor :: Env -> PackumentDeps -> PublishTarget -> MirrorTransport
mirrorTransportFor env deps target =
    MirrorTransport
        { ptClient = envPrivateClient env
        , ptMintCredential = Just . ClientCredential (ptUsername target) <$> mintSecret (ptCredentials target)
        , ptLimits = pdLimits deps
        , ptTracing = tracingPortOf (envTelemetry env)
        }

{- Build one mount's worker bundle. The metadata client is anonymous, so no client credential reaches
the public origin, pdTarballHostGate owns the artifact host gate, and the no-op logs defer to the worker's per-job log. -}
workerPolicyFor :: Env -> Ecosystem -> PackumentDeps -> MirrorPublish -> Int -> WorkerPolicy
workerPolicyFor env eco deps publish artifactMaxBytes =
    WorkerPolicy
        { -- The mount's own first-party predicate, so the worker refuses a name the
          -- deployment owns exactly as the serve and publish paths do.
          wpFirstParty = pdFirstParty deps
        , wpResolveVersion = fetchVersionDetails . client
        , wpRules = pdRules deps
        , wpMinIntegrity = pdMinIntegrity deps
        , wpArtifactHostHonoured =
            -- The same host gate the serve path applies before its public artifact fetch, closed
            -- against the public upstream authority.
            tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps))
        , -- The mount's own artifact capability, the adapter record the serve deps
          -- carry, so the worker fetches a job's bytes exactly as the serve path would.
          wpArtifact = pdArtifact deps
        , wpEgressUrl = pdEgressUrl deps
        , wpPublish = publish
        , wpArtifactLimits = (pdLimits deps){maxMirrorArtifactBytes = artifactMaxBytes}
        , wpMirrorState = Nothing
        , wpNow = pdNow deps
        }
  where
    client pay = case eco of
        PyPI -> uncachedPublicMetadataClient (readCapability (chargingSelectedReads decodedDocumentCharge pay pypiMirrorRead))
        _ -> publicMetadataClient (envMetadataCache env) (Source (registryUrlText publicBaseUrl)) (readCapability (metadataRead (pdMetadata deps)))
    readCapability ecosystemRead =
        ecosystemMetadataReads
            ecosystemRead
            (tracingPortOf (envTelemetry env))
            (metricsPortOf (envMetrics env))
            (\_ _ -> pure ())
            (\_ _ -> pure ())
            (\_ -> pure ())
            publicOrigin

    publicBaseUrl = pdPublicBaseUrl deps

    publicOrigin = controlPublicOrigin (envPublicControl env) (Just WorkerMetadata) (anonymousOrigin (pdLimits deps) (envPublicClient env) publicBaseUrl)
