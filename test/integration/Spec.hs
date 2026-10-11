-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete integration contracts run without an OCI product image.
module Main (main) where

import Ecluse.AwsEndToEndIntegrationSpec qualified as AwsComposition
import Ecluse.Core.Registry.PyPI.AdapterIntegrationSpec qualified as PyPI
import Ecluse.Core.Security.EgressIntegrationSpec qualified as Egress
import Ecluse.Core.Server.Pipeline.OriginIntegrationSpec qualified as Origin
import Ecluse.Core.Server.Pipeline.PackumentIntegrationSpec qualified as NpmMetadata
import Ecluse.Core.Server.Pipeline.PublishIntegrationSpec qualified as Publication
import Ecluse.Core.Server.Pipeline.SharedIntegrationSpec qualified as PipelineFaults
import Ecluse.Core.Server.Pipeline.TarballIntegrationSpec qualified as NpmArtifact
import Ecluse.Core.Worker.LoopIntegrationSpec qualified as Worker
import Ecluse.PilotIntegrationSpec qualified as Pilot
import Ecluse.Runtime.Cve.SyncIntegrationSpec qualified as AdvisorySync
import Ecluse.Runtime.Pilot.ExportIntegrationSpec qualified as AdvisoryExport
import Ecluse.Runtime.Queue.SqsIntegrationSpec qualified as SqsQueue
import Ecluse.Runtime.ServerIntegrationSpec qualified as ServerShutdown
import Ecluse.Runtime.Telemetry.InstrumentsIntegrationSpec qualified as TelemetryMetrics
import Ecluse.Runtime.Telemetry.ScrapeIntegrationSpec qualified as TelemetryScrape
import Ecluse.Runtime.Telemetry.TracingIntegrationSpec qualified as TelemetryTraces
import Ecluse.Test.Contract.Integration.Catalogue qualified as Coverage
import Ecluse.Test.Contract.Runner (DeclaredGroup (DeclaredGroup), contractMain)

-- | Missing Docker fails during fixture acquisition and cannot become pending coverage.
main :: IO ()
main =
    contractMain
        Coverage.integrationCatalogue
        "integration"
        [ DeclaredGroup Coverage.awsComposition (const AwsComposition.spec)
        , DeclaredGroup Coverage.pypiHttp (const PyPI.spec)
        , DeclaredGroup Coverage.egressHttp (const Egress.spec)
        , DeclaredGroup Coverage.originHttp (const Origin.spec)
        , DeclaredGroup Coverage.npmMetadataHttp (const NpmMetadata.spec)
        , DeclaredGroup Coverage.publicationHttp (const Publication.spec)
        , DeclaredGroup Coverage.pipelineFaultsHttp (const PipelineFaults.spec)
        , DeclaredGroup Coverage.npmArtifactHttp (const NpmArtifact.spec)
        , DeclaredGroup Coverage.workerSqs (const Worker.spec)
        , DeclaredGroup Coverage.pilotS3 (const Pilot.spec)
        , DeclaredGroup Coverage.advisorySyncS3 (const AdvisorySync.spec)
        , DeclaredGroup Coverage.advisoryExportS3 (const AdvisoryExport.spec)
        , DeclaredGroup Coverage.queueSqs (const SqsQueue.spec)
        , DeclaredGroup Coverage.serverShutdownHttp (const ServerShutdown.spec)
        , DeclaredGroup Coverage.collectorMetrics (const TelemetryMetrics.spec)
        , DeclaredGroup Coverage.telemetryScrapeHttp (const TelemetryScrape.spec)
        , DeclaredGroup Coverage.collectorTraces (const TelemetryTraces.spec)
        ]
        (const (pure Nothing))
