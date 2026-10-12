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
import Ecluse.Test.Contract.Binding (allCaseIds)
import Ecluse.Test.Contract.Integration.Catalogue qualified as Coverage
import Ecluse.Test.Contract.Runner (DeclaredGroup (DeclaredGroup), contractMain)
import Ecluse.Test.Contract.Types (Case (Case), Group (groupCases, groupId))
import Test.Hspec (Spec)

-- | Missing Docker fails during fixture acquisition and cannot become pending coverage.
main :: IO ()
main = do
    declarations <-
        sequence
            [ discover Coverage.awsComposition AwsComposition.spec
            , discover Coverage.pypiHttp PyPI.spec
            , discover Coverage.egressHttp Egress.spec
            , discover Coverage.originHttp Origin.spec
            , discover Coverage.npmMetadataHttp NpmMetadata.spec
            , discover Coverage.publicationHttp Publication.spec
            , discover Coverage.pipelineFaultsHttp PipelineFaults.spec
            , discover Coverage.npmArtifactHttp NpmArtifact.spec
            , discover Coverage.workerSqs Worker.spec
            , discover Coverage.pilotS3 Pilot.spec
            , discover Coverage.advisorySyncS3 AdvisorySync.spec
            , discover Coverage.advisoryExportS3 AdvisoryExport.spec
            , discover Coverage.queueSqs SqsQueue.spec
            , discover Coverage.serverShutdownHttp ServerShutdown.spec
            , discover Coverage.collectorMetrics TelemetryMetrics.spec
            , discover Coverage.telemetryScrapeHttp TelemetryScrape.spec
            , discover Coverage.collectorTraces TelemetryTraces.spec
            ]
    contractMain Coverage.integrationCatalogue "integration" declarations (const (pure Nothing))

discover :: Group -> Spec -> IO DeclaredGroup
discover metadata spec = do
    identifiers <- allCaseIds spec
    let registration = metadata{groupCases = [Case identifier [groupId metadata] | identifier <- sort identifiers]}
    pure (DeclaredGroup registration (const spec))
