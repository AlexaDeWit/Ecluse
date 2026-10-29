-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Bind native install evidence to the mount's anonymous guarded public artifact reader.
module Ecluse.Core.Server.InstallEvidence (newPublicInstallEvidenceReads, newTicketInstallEvidenceFlightBudget) where

import UnliftIO.Exception (finally)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Package (PackageDetails, artUrl, pkgName)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.InstallEvidence (anonymousInstallArtifactRead)
import Ecluse.Core.Registry.Public (PublicControl, ReadControl (Controlled))
import Ecluse.Core.Rules.Effectful (ReadFault)
import Ecluse.Core.Rules.Install
import Ecluse.Core.Security (Origin (UntrustedOrigin), hostPortAddress, thgPublicHostPort)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, awaitingFlight, charge, newChargeRegion, servingFlight)
import Ecluse.Core.Server.Admission.Types (FlightKey (FlightKey))
import Ecluse.Core.Server.Context (PackumentDeps (..), pdTarballHostGate, tarballHostHonoured)
import Ecluse.Core.Telemetry.Metrics (CacheStore (VersionStore))
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead

-- | Keep the ticket live while these readers share one operation deadline and cumulative allowance.
newPublicInstallEvidenceReads :: PackumentDeps -> ExchangeClient -> PublicControl -> InstallEvidenceRole -> MemoryTicket -> (ReadFault -> IO ()) -> IO (PackageDetails -> IO (Maybe InstallEvidenceRead))
newPublicInstallEvidenceReads deps client control role ticket diagnose = do
    evidenceReads <- installEvidenceReads (pdInstallEvidence deps) context
    pure (\details -> if pdFirstParty deps (pkgName details) then pure Nothing else evidenceReads details)
  where
    context = do
        bytes <- newChargeRegion (charge ticket)
        inspection <- newChargeRegion (charge ticket)
        sharing <- newTicketInstallEvidenceFlightBudget ticket
        pure
            InstallEvidenceContext
                { ieLimits = pdLimits deps
                , ieRole = role
                , ieReadArtifact = anonymousInstallArtifactRead (pdLimits deps) client (Controlled control kind) (pdArtifact deps) locate
                , ieBudget = InstallEvidenceBudget (\action -> Just <$> action bytes inspection)
                , ieFlightBudget = Just sharing
                , ieDiagnostic = diagnose
                }
    locate artifact =
        guard (tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps)) (hostPortAddress (artUrl artifact)))
            *> rightToMaybe (pdEgressUrl deps (artUrl artifact))
    kind = if role == InstallWorkerReadmission then PublicRead.WorkerArtifact else PublicRead.Artifact

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
