-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure integration groups declare all cases before any resource acquisition.
module Ecluse.Test.Contract.Integration.Catalogue (
    integrationCatalogue,
    awsComposition,
    pypiHttp,
    cargoHttp,
    rubyGemsHttp,
    mirrorViewHttp,
    egressHttp,
    originHttp,
    npmMetadataHttp,
    publicationHttp,
    pipelineFaultsHttp,
    npmArtifactHttp,
    workerSqs,
    pilotS3,
    advisorySyncS3,
    mirrorStateS3Internal,
    mirrorStateS3,
    advisoryExportS3,
    queueSqs,
    serverShutdownHttp,
    collectorMetrics,
    telemetryScrapeHttp,
    collectorTraces,
) where

import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Test.Contract.Integration.Obligations (integrationObligations)
import Ecluse.Test.Contract.Types

-- | Stable groups preserve the native module lifecycle and the full coverage denominator.
integrationCatalogue :: Catalogue
integrationCatalogue =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName (filter (isJust . adapterFor) universe)
        , catalogueProfiles =
            [ Profile "integration/aws-composition" ["npm"] ["ministack"] ["serving", "mirror-worker"]
            , Profile "integration/rubygems" ["rubygems"] ["wai"] ["metadata-read", "artifact-read", "serving"]
            , Profile "integration/cargo" ["cargo"] ["wai"] ["metadata-read", "artifact-read", "serving"]
            , Profile "integration/pypi" ["pypi"] ["wai"] ["metadata-read", "artifact-read", "serving"]
            , Profile "integration/mirror-view" ["pypi"] ["wai"] ["metadata-read", "artifact-read", "serving"]
            , Profile "integration/egress" [] ["wai"] ["egress"]
            , Profile "integration/origin" ["npm"] ["wai"] ["origin"]
            , Profile "integration/npm-metadata" ["npm"] ["wai"] ["metadata-read", "serving"]
            , Profile "integration/publication" ["npm", "pypi", "cargo"] ["wai"] ["publication"]
            , Profile "integration/pipeline-faults" ["npm"] ["wai"] ["pipeline-faults"]
            , Profile "integration/npm-artifact" ["npm"] ["wai"] ["artifact-read"]
            , Profile "integration/worker" ["npm"] ["ministack"] ["queue-lease", "mirror-worker"]
            , Profile "integration/pilot" ["npm", "pypi"] ["ministack"] ["advisory-compilation", "advisory-publication"]
            , Profile "integration/advisory-sync" [] ["ministack"] ["advisory-sync"]
            , Profile "integration/mirror-state-s3-internal" ["pypi"] ["ministack"] ["mirror-state"]
            , Profile "integration/mirror-state-s3" ["pypi"] ["ministack"] ["mirror-state"]
            , Profile "integration/advisory-export" ["npm", "pypi"] ["ministack"] ["advisory-publication", "advisory-last-good"]
            , Profile "integration/queue-sqs" ["npm"] ["ministack"] ["queue-payload", "queue-lease", "queue-redelivery"]
            , Profile "integration/server-shutdown" [] ["wai"] ["server-shutdown"]
            , Profile "integration/telemetry-metrics" [] ["collector"] ["telemetry-metrics"]
            , Profile "integration/telemetry-scrape" [] ["wai"] ["telemetry-scrape"]
            , Profile "integration/telemetry-traces" [] ["collector"] ["telemetry-traces"]
            ]
        , catalogueObligations = integrationObligations
        , catalogueGroups =
            [ awsComposition
            , cargoHttp
            , rubyGemsHttp
            , pypiHttp
            , mirrorViewHttp
            , egressHttp
            , originHttp
            , npmMetadataHttp
            , publicationHttp
            , pipelineFaultsHttp
            , npmArtifactHttp
            , workerSqs
            , pilotS3
            , advisorySyncS3
            , mirrorStateS3Internal
            , mirrorStateS3
            , advisoryExportS3
            , queueSqs
            , serverShutdownHttp
            , collectorMetrics
            , telemetryScrapeHttp
            , collectorTraces
            ]
        }

-- | Scheduling metadata for integration/aws-composition.
awsComposition :: Group
awsComposition =
    expectedGroup
        "integration/aws-composition"
        "aws-composition"
        ["serving", "mirror-worker"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/pypi.
pypiHttp :: Group
pypiHttp =
    expectedGroup
        "integration/pypi"
        "serving"
        ["metadata-read", "artifact-read", "serving"]
        []
        ["wai"]

-- | Native sparse HTTP and shared serving laws use local upstreams.
cargoHttp :: Group
cargoHttp = expectedGroup "integration/cargo" "serving" ["metadata-read", "artifact-read", "serving"] [] ["wai"]

-- | Named-gem native bytes run the same serving laws against independent local origins.
rubyGemsHttp :: Group
rubyGemsHttp = expectedGroup "integration/rubygems" "serving" ["metadata-read", "artifact-read", "serving"] [] ["wai"]

-- | Scheduling metadata for integration/mirror-view.
mirrorViewHttp :: Group
mirrorViewHttp =
    expectedGroup
        "integration/mirror-view"
        "mirror-view"
        ["metadata-read", "artifact-read", "serving"]
        []
        ["wai"]

-- | Scheduling metadata for integration/egress.
egressHttp :: Group
egressHttp =
    expectedGroup
        "integration/egress"
        "egress"
        ["egress"]
        []
        ["wai"]

-- | Scheduling metadata for integration/origin.
originHttp :: Group
originHttp =
    expectedGroup
        "integration/origin"
        "origin"
        ["origin"]
        []
        ["wai"]

-- | Scheduling metadata for integration/npm-metadata.
npmMetadataHttp :: Group
npmMetadataHttp =
    expectedGroup
        "integration/npm-metadata"
        "serving"
        ["metadata-read", "serving"]
        []
        ["wai"]

-- | Scheduling metadata for integration/publication.
publicationHttp :: Group
publicationHttp =
    expectedGroup
        "integration/publication"
        "publication"
        ["publication"]
        []
        ["wai"]

-- | Scheduling metadata for integration/pipeline-faults.
pipelineFaultsHttp :: Group
pipelineFaultsHttp =
    expectedGroup
        "integration/pipeline-faults"
        "pipeline-faults"
        ["pipeline-faults"]
        []
        ["wai"]

-- | Scheduling metadata for integration/npm-artifact.
npmArtifactHttp :: Group
npmArtifactHttp =
    expectedGroup
        "integration/npm-artifact"
        "npm-artifact"
        ["artifact-read"]
        []
        ["wai"]

-- | Scheduling metadata for integration/worker.
workerSqs :: Group
workerSqs =
    expectedGroup
        "integration/worker"
        "worker"
        ["queue-lease", "mirror-worker"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/pilot.
pilotS3 :: Group
pilotS3 =
    expectedGroup
        "integration/pilot"
        "pilot"
        ["advisory-compilation", "advisory-publication"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/advisory-sync.
advisorySyncS3 :: Group
advisorySyncS3 =
    expectedGroup
        "integration/advisory-sync"
        "advisory-sync"
        ["advisory-sync"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/mirror-state-s3-internal.
mirrorStateS3Internal :: Group
mirrorStateS3Internal =
    expectedGroup
        "integration/mirror-state-s3-internal"
        "mirror-state-s3-internal"
        ["mirror-state"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/mirror-state-s3.
mirrorStateS3 :: Group
mirrorStateS3 =
    expectedGroup
        "integration/mirror-state-s3"
        "mirror-state-s3"
        ["mirror-state"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/advisory-export.
advisoryExportS3 :: Group
advisoryExportS3 =
    expectedGroup
        "integration/advisory-export"
        "advisory"
        ["advisory-publication", "advisory-last-good"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/queue-sqs.
queueSqs :: Group
queueSqs =
    expectedGroup
        "integration/queue-sqs"
        "queue"
        ["queue-payload", "queue-lease", "queue-redelivery"]
        ["docker"]
        ["ministack"]

-- | Scheduling metadata for integration/server-shutdown.
serverShutdownHttp :: Group
serverShutdownHttp =
    expectedGroup
        "integration/server-shutdown"
        "server-shutdown"
        ["server-shutdown"]
        []
        ["wai"]

-- | Scheduling metadata for integration/telemetry-metrics.
collectorMetrics :: Group
collectorMetrics =
    expectedGroup
        "integration/telemetry-metrics"
        "telemetry-metrics"
        ["telemetry-metrics"]
        ["docker"]
        ["collector"]

-- | Scheduling metadata for integration/telemetry-scrape.
telemetryScrapeHttp :: Group
telemetryScrapeHttp =
    expectedGroup
        "integration/telemetry-scrape"
        "telemetry-scrape"
        ["telemetry-scrape"]
        []
        ["wai"]

-- | Scheduling metadata for integration/telemetry-traces.
collectorTraces :: Group
collectorTraces =
    expectedGroup
        "integration/telemetry-traces"
        "telemetry-traces"
        ["telemetry-traces"]
        ["docker"]
        ["collector"]

expectedGroup :: Text -> Text -> [Text] -> [Text] -> [Text] -> Group
expectedGroup identifier contract capabilities prerequisites resources =
    Group identifier contract identifier Integration "integration" cases capabilities prerequisites resources ["serial-environment"]
  where
    cases =
        sortOn
            caseId
            [ Case item [identifier]
            | obligation <- integrationObligations
            , obligationId obligation == identifier
            , item <- obligationCases obligation
            ]
