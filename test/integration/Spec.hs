-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Integration selection uses the shared catalogue runner without an OCI product image.
module Main (main) where

import Data.Map.Strict qualified as Map

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
import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Runner (contractMain)

-- | Missing Docker fails during fixture acquisition and cannot become pending coverage.
main :: IO ()
main =
    contractMain
        integrationCatalogue
        ( Map.fromList
            [ ("integration/aws-composition", const AwsComposition.spec)
            , ("integration/pypi", const PyPI.spec)
            , ("integration/egress", const Egress.spec)
            , ("integration/origin", const Origin.spec)
            , ("integration/npm-metadata", const NpmMetadata.spec)
            , ("integration/publication", const Publication.spec)
            , ("integration/pipeline-faults", const PipelineFaults.spec)
            , ("integration/npm-artifact", const NpmArtifact.spec)
            , ("integration/worker", const Worker.spec)
            , ("integration/pilot", const Pilot.spec)
            , ("integration/advisory-sync", const AdvisorySync.spec)
            , ("integration/advisory-export", const AdvisoryExport.spec)
            , ("integration/queue-sqs", const SqsQueue.spec)
            , ("integration/server-shutdown", const ServerShutdown.spec)
            , ("integration/telemetry-metrics", const TelemetryMetrics.spec)
            , ("integration/telemetry-scrape", const TelemetryScrape.spec)
            , ("integration/telemetry-traces", const TelemetryTraces.spec)
            ]
        )
        (const (pure Nothing))
