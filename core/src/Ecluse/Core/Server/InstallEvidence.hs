-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Bind native install evidence to the mount's anonymous guarded public artifact reader.
module Ecluse.Core.Server.InstallEvidence (newPublicInstallEvidenceReads, newTicketInstallEvidenceFlightBudget) where

import UnliftIO.Exception (finally)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Package (PackageDetails, artUrl, pkgName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.InstallEvidence (readInstallArtifact)
import Ecluse.Core.Registry.Public (PublicControl, ReadControl (Controlled))
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Install
import Ecluse.Core.Rules.Types (Inability (EvaluationFailed, InstallSignalUnread), Transience (WontResolve))
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), Origin (UntrustedOrigin), hostPortAddress, thgPublicHostPort)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, awaitingFlight, charge, newChargeRegion, servingFlight)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))
import Ecluse.Core.Server.Context (PackumentDeps (..), pdTarballHostGate, tarballHostHonoured)
import Ecluse.Core.Telemetry.Metrics (CacheStore (VersionStore))
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead

-- | One factory holds the operation deadline and aggregate allowance for every selected subject.
newPublicInstallEvidenceReads :: PackumentDeps -> ExchangeClient -> PublicControl -> InstallEvidenceRole -> (Int -> IO ()) -> (Int -> IO ()) -> Maybe InstallEvidenceFlightBudget -> (ReadFault -> IO ()) -> IO (PackageDetails -> IO (Maybe InstallEvidenceRead))
newPublicInstallEvidenceReads deps client control role bytes inspection sharing diagnose = case pdInstallEvidence deps of
    Nothing -> pure (const (pure Nothing))
    Just provider -> do
        evidenceReads <-
            newInstallEvidenceReads
                provider
                InstallEvidenceContext
                    { ieLimits = pdLimits deps
                    , ieRole = role
                    , ieReadArtifact = readArtifact
                    , ieBudget = InstallEvidenceBudget (\action -> Just <$> action bytes inspection)
                    , ieFlightBudget = sharing
                    , ieDiagnostic = diagnose
                    }
        pure (\details -> if pdFirstParty deps (pkgName details) then pure Nothing else evidenceReads details)
  where
    readArtifact bounded artifact pay
        | not (tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps)) (hostPortAddress (artUrl artifact))) = pure (Left unavailable)
        | otherwise = case pdEgressUrl deps (artUrl artifact) of
            Left _ -> pure (Left unavailable)
            Right url -> readInstallArtifact ((pdLimits deps){maxMirrorArtifactBytes = min (maxMirrorArtifactBytes (pdLimits deps)) (maxMirrorArtifactBytes bounded)}) client (Controlled control kind) (artifactByUrl (pdArtifact deps) Nothing (registryUrlText url)) pay
    kind = if role == InstallWorkerReadmission then PublicRead.WorkerArtifact else PublicRead.Artifact
    unavailable = ReadFault WontResolve EvaluationFailed InstallSignalUnread

-- | Reuse charged regions while each native flight borrows every waiter's memory priority.
newTicketInstallEvidenceFlightBudget :: MemoryTicket -> IO InstallEvidenceFlightBudget
newTicketInstallEvidenceFlightBudget ticket = do
    active <- newIORef Nothing
    serial <- newMVar ()
    let pay bytes =
            readIORef active >>= \case
                Nothing -> charge ticket bytes
                Just flight -> charge (servingFlight flight ticket) bytes
    bytes <- newChargeRegion pay
    inspection <- newChargeRegion pay
    pure $ InstallEvidenceFlightBudget $ \key action -> do
        let flight = FlightKey VersionStore (toShort ("install-evidence:" <> key))
        awaitingFlight ticket flight . withMVar serial $ \() ->
            (atomicWriteIORef active (Just flight) >> action (InstallEvidenceBudget (\use -> Just <$> use bytes inspection)))
                `finally` atomicWriteIORef active Nothing
