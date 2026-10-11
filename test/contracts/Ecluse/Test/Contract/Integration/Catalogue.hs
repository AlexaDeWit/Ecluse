-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure integration groups declare all cases before any resource acquisition.
module Ecluse.Test.Contract.Integration.Catalogue (
    integrationCatalogue,
    awsComposition,
    pypiHttp,
    egressHttp,
    originHttp,
    npmMetadataHttp,
    publicationHttp,
    pipelineFaultsHttp,
    npmArtifactHttp,
    workerSqs,
    pilotS3,
    advisorySyncS3,
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
            , Profile "integration/pypi" ["pypi"] ["wai"] ["metadata-read", "artifact-read", "serving"]
            , Profile "integration/egress" [] ["wai"] ["egress"]
            , Profile "integration/origin" ["npm"] ["wai"] ["origin"]
            , Profile "integration/npm-metadata" ["npm"] ["wai"] ["metadata-read", "serving"]
            , Profile "integration/publication" ["npm"] ["wai"] ["publication"]
            , Profile "integration/pipeline-faults" ["npm"] ["wai"] ["pipeline-faults"]
            , Profile "integration/npm-artifact" ["npm"] ["wai"] ["artifact-read"]
            , Profile "integration/worker" ["npm"] ["ministack"] ["queue-lease", "mirror-worker"]
            , Profile "integration/pilot" ["npm", "pypi"] ["ministack"] ["advisory-compilation", "advisory-publication"]
            , Profile "integration/advisory-sync" [] ["ministack"] ["advisory-sync"]
            , Profile "integration/advisory-export" [] ["ministack"] ["advisory-publication", "advisory-last-good"]
            , Profile "integration/queue-sqs" ["npm"] ["ministack"] ["queue-payload", "queue-lease", "queue-redelivery"]
            , Profile "integration/server-shutdown" [] ["wai"] ["server-shutdown"]
            , Profile "integration/telemetry-metrics" [] ["collector"] ["telemetry-metrics"]
            , Profile "integration/telemetry-scrape" [] ["wai"] ["telemetry-scrape"]
            , Profile "integration/telemetry-traces" [] ["collector"] ["telemetry-traces"]
            ]
        , catalogueObligations = integrationObligations
        , catalogueGroups =
            [ awsComposition
            , pypiHttp
            , egressHttp
            , originHttp
            , npmMetadataHttp
            , publicationHttp
            , pipelineFaultsHttp
            , npmArtifactHttp
            , workerSqs
            , pilotS3
            , advisorySyncS3
            , advisoryExportS3
            , queueSqs
            , serverShutdownHttp
            , collectorMetrics
            , telemetryScrapeHttp
            , collectorTraces
            ]
        }

-- | Declared coverage for integration/aws-composition.
awsComposition :: Group
awsComposition =
    registered
        "integration/aws-composition"
        "aws-composition"
        ["serving", "mirror-worker"]
        ["docker"]
        ["ministack"]
        [ "integration/aws-composition/native-001"
        , "integration/aws-composition/native-002"
        ]

-- | Declared coverage for integration/pypi.
pypiHttp :: Group
pypiHttp =
    registered
        "integration/pypi"
        "serving"
        ["metadata-read", "artifact-read", "serving"]
        []
        ["wai"]
        [ "integration/pypi/native-001"
        , "integration/pypi/native-002"
        , "integration/pypi/native-003"
        , "integration/pypi/native-004"
        , "integration/pypi/native-005/core-metadata"
        , "integration/pypi/native-005/dist-info-metadata"
        , "integration/pypi/native-005/data-dist-info-metadata"
        , "integration/pypi/native-006"
        , "integration/pypi/native-007"
        , "integration/pypi/native-008"
        , "integration/pypi/native-009/public"
        , "integration/pypi/native-009/trusted"
        , "integration/pypi/native-010"
        , "integration/pypi/native-011"
        , "integration/pypi/native-012"
        , "integration/pypi/native-013"
        , "integration/pypi/native-014"
        , "integration/pypi/native-015"
        , "integration/pypi/native-016/404"
        , "integration/pypi/native-016/503"
        , "integration/pypi/native-017/401"
        , "integration/pypi/native-017/403"
        , "integration/pypi/native-018"
        , "integration/pypi/native-019"
        , "integration/pypi/native-020/available"
        , "integration/pypi/native-020/all-yanked"
        , "integration/pypi/native-021"
        , "integration.serving.pypi.default-denial"
        , "integration.serving.pypi.identity-precedence"
        , "integration.serving.pypi.authorised-union"
        , "integration.serving.pypi.quarantine-private-authority"
        , "integration.serving.pypi.private-divergence"
        , "integration.serving.pypi.public-integrity"
        , "integration.serving.pypi.trusted-integrity"
        , "integration.serving.pypi.artifact-integrity"
        , "integration.serving.pypi.conditional-coherence"
        ]

-- | Declared coverage for integration/egress.
egressHttp :: Group
egressHttp =
    registered
        "integration/egress"
        "egress"
        ["egress"]
        []
        ["wai"]
        [ "integration/egress/native-001"
        , "integration/egress/native-002"
        , "integration/egress/native-003"
        ]

-- | Declared coverage for integration/origin.
originHttp :: Group
originHttp =
    registered
        "integration/origin"
        "origin"
        ["origin"]
        []
        ["wai"]
        [ "integration/origin/native-001/401"
        , "integration/origin/native-001/403"
        , "integration/origin/native-002/401"
        , "integration/origin/native-002/403"
        , "integration/origin/native-003/401/merged"
        , "integration/origin/native-003/401/first-party"
        , "integration/origin/native-003/403/merged"
        , "integration/origin/native-003/403/first-party"
        , "integration/origin/native-004/401"
        , "integration/origin/native-004/403"
        , "integration/origin/native-005/401"
        , "integration/origin/native-005/403"
        , "integration/origin/native-006/merged/absent"
        , "integration/origin/native-006/merged/unavailable"
        , "integration/origin/native-006/merged/malformed"
        , "integration/origin/native-006/first-party/absent"
        , "integration/origin/native-006/first-party/unavailable"
        , "integration/origin/native-006/first-party/malformed"
        , "integration/origin/native-007"
        , "integration/origin/native-008"
        , "integration/origin/native-009"
        , "integration/origin/native-010"
        , "integration/origin/native-011"
        , "integration/origin/native-012"
        , "integration/origin/native-013"
        , "integration/origin/native-014"
        , "integration/origin/native-015"
        , "integration/origin/native-016"
        , "integration/origin/native-017"
        ]

-- | Declared coverage for integration/npm-metadata.
npmMetadataHttp :: Group
npmMetadataHttp =
    registered
        "integration/npm-metadata"
        "serving"
        ["metadata-read", "serving"]
        []
        ["wai"]
        [ "integration/npm-metadata/native-001"
        , "integration/npm-metadata/native-002"
        , "integration/npm-metadata/native-003"
        , "integration/npm-metadata/native-004"
        , "integration/npm-metadata/native-005"
        , "integration/npm-metadata/native-006"
        , "integration/npm-metadata/native-007"
        , "integration/npm-metadata/native-008"
        , "integration/npm-metadata/native-009"
        , "integration/npm-metadata/native-010"
        , "integration/npm-metadata/native-011"
        , "integration/npm-metadata/native-012"
        , "integration/npm-metadata/native-013"
        , "integration/npm-metadata/native-014"
        , "integration/npm-metadata/native-015"
        , "integration/npm-metadata/native-016"
        , "integration/npm-metadata/native-017"
        , "integration/npm-metadata/native-018"
        , "integration/npm-metadata/native-019"
        , "integration/npm-metadata/native-020"
        , "integration/npm-metadata/native-021"
        , "integration/npm-metadata/native-022"
        , "integration/npm-metadata/native-023"
        , "integration/npm-metadata/native-024"
        , "integration/npm-metadata/native-025"
        , "integration/npm-metadata/native-026"
        , "integration/npm-metadata/native-027"
        , "integration/npm-metadata/native-028"
        , "integration/npm-metadata/native-029"
        , "integration/npm-metadata/native-030"
        , "integration/npm-metadata/native-031"
        , "integration/npm-metadata/native-032"
        , "integration/npm-metadata/native-033"
        , "integration/npm-metadata/native-034"
        , "integration/npm-metadata/native-035/denied"
        , "integration/npm-metadata/native-035/allowed"
        , "integration/npm-metadata/native-035/stored"
        , "integration.serving.npm.default-denial"
        , "integration.serving.npm.identity-precedence"
        , "integration.serving.npm.authorised-union"
        , "integration.serving.npm.quarantine-private-authority"
        , "integration.serving.npm.private-divergence"
        , "integration.serving.npm.public-integrity"
        , "integration.serving.npm.trusted-integrity"
        , "integration.serving.npm.artifact-integrity"
        , "integration.serving.npm.conditional-coherence"
        ]

-- | Declared coverage for integration/publication.
publicationHttp :: Group
publicationHttp =
    registered
        "integration/publication"
        "publication"
        ["publication"]
        []
        ["wai"]
        [ "integration/publication/native-001"
        , "integration/publication/native-002"
        , "integration/publication/native-003"
        , "integration/publication/native-004"
        , "integration/publication/native-005/route-1"
        , "integration/publication/native-005/route-2"
        , "integration/publication/native-005/route-3"
        , "integration/publication/native-005/route-4"
        , "integration/publication/native-006/forwarded/absent"
        , "integration/publication/native-006/forwarded/wrong"
        , "integration/publication/native-006/static/absent"
        , "integration/publication/native-006/static/wrong"
        , "integration/publication/native-007"
        , "integration/publication/native-008"
        , "integration/publication/native-009"
        , "integration/publication/native-010"
        , "integration/publication/native-011"
        , "integration/publication/native-012"
        , "integration/publication/native-013"
        , "integration/publication/native-014"
        , "integration/publication/native-015"
        , "integration/publication/native-016"
        , "integration/publication/native-017"
        , "integration/publication/native-018"
        , "integration/publication/native-019"
        ]

-- | Declared coverage for integration/pipeline-faults.
pipelineFaultsHttp :: Group
pipelineFaultsHttp =
    registered
        "integration/pipeline-faults"
        "pipeline-faults"
        ["pipeline-faults"]
        []
        ["wai"]
        [ "integration/pipeline-faults/native-001"
        , "integration/pipeline-faults/native-002"
        , "integration/pipeline-faults/native-003"
        , "integration/pipeline-faults/native-004"
        , "integration/pipeline-faults/native-005"
        , "integration/pipeline-faults/native-006"
        , "integration/pipeline-faults/native-007"
        , "integration/pipeline-faults/native-008"
        , "integration/pipeline-faults/native-009"
        , "integration/pipeline-faults/native-010"
        , "integration/pipeline-faults/native-011"
        , "integration/pipeline-faults/native-012"
        , "integration/pipeline-faults/native-013"
        , "integration/pipeline-faults/native-014"
        , "integration/pipeline-faults/native-015"
        , "integration/pipeline-faults/native-016"
        , "integration/pipeline-faults/native-017"
        , "integration/pipeline-faults/native-018"
        , "integration/pipeline-faults/native-019"
        ]

-- | Declared coverage for integration/npm-artifact.
npmArtifactHttp :: Group
npmArtifactHttp =
    registered
        "integration/npm-artifact"
        "npm-artifact"
        ["artifact-read"]
        []
        ["wai"]
        [ "integration/npm-artifact/native-001"
        , "integration/npm-artifact/native-002/401/merged"
        , "integration/npm-artifact/native-002/401/first-party"
        , "integration/npm-artifact/native-002/403/merged"
        , "integration/npm-artifact/native-002/403/first-party"
        , "integration/npm-artifact/native-003/401"
        , "integration/npm-artifact/native-003/403"
        , "integration/npm-artifact/native-004/404/merged"
        , "integration/npm-artifact/native-004/404/first-party"
        , "integration/npm-artifact/native-004/503/merged"
        , "integration/npm-artifact/native-004/503/first-party"
        , "integration/npm-artifact/native-005"
        , "integration/npm-artifact/native-006"
        , "integration/npm-artifact/native-007"
        , "integration/npm-artifact/native-008"
        , "integration/npm-artifact/native-009"
        , "integration/npm-artifact/native-010"
        , "integration/npm-artifact/native-011"
        , "integration/npm-artifact/native-012"
        , "integration/npm-artifact/native-013"
        , "integration/npm-artifact/native-014"
        , "integration/npm-artifact/native-015"
        , "integration/npm-artifact/native-016"
        , "integration/npm-artifact/native-017"
        , "integration/npm-artifact/native-018"
        , "integration/npm-artifact/native-019"
        , "integration/npm-artifact/native-020"
        , "integration/npm-artifact/native-021"
        , "integration/npm-artifact/native-022"
        , "integration/npm-artifact/native-023"
        , "integration/npm-artifact/native-024"
        , "integration/npm-artifact/native-025"
        , "integration/npm-artifact/native-026"
        , "integration/npm-artifact/native-027"
        , "integration/npm-artifact/native-028"
        , "integration/npm-artifact/native-029"
        , "integration/npm-artifact/native-030"
        , "integration/npm-artifact/native-031"
        , "integration/npm-artifact/native-032"
        , "integration/npm-artifact/native-033"
        , "integration/npm-artifact/native-034"
        , "integration/npm-artifact/native-035"
        , "integration/npm-artifact/native-036"
        , "integration/npm-artifact/native-037"
        , "integration/npm-artifact/native-038"
        , "integration/npm-artifact/native-039"
        , "integration/npm-artifact/native-040"
        , "integration/npm-artifact/native-041"
        , "integration/npm-artifact/native-042"
        , "integration/npm-artifact/native-043/denied"
        , "integration/npm-artifact/native-043/allowed"
        ]

-- | Declared coverage for integration/worker.
workerSqs :: Group
workerSqs =
    registered
        "integration/worker"
        "worker"
        ["queue-lease", "mirror-worker"]
        ["docker"]
        ["ministack"]
        [ "integration/worker/native-001"
        , "integration/worker/native-002"
        , "integration/worker/native-003"
        , "integration/worker/native-004"
        , "integration/worker/native-005"
        , "integration/worker/native-006"
        , "integration/worker/native-007"
        , "integration/worker/native-008"
        ]

-- | Declared coverage for integration/pilot.
pilotS3 :: Group
pilotS3 =
    registered
        "integration/pilot"
        "pilot"
        ["advisory-compilation", "advisory-publication"]
        ["docker"]
        ["ministack"]
        [ "integration/pilot/native-001/empty/first"
        , "integration/pilot/native-001/empty/replacement"
        , "integration/pilot/native-001/wrong-ecosystem/first"
        , "integration/pilot/native-001/wrong-ecosystem/replacement"
        , "integration/pilot/native-002/epss-required"
        , "integration/pilot/native-002/epss-unused"
        , "integration/pilot/native-003/feed-up"
        , "integration/pilot/native-003/feed-down"
        ]

-- | Declared coverage for integration/advisory-sync.
advisorySyncS3 :: Group
advisorySyncS3 =
    registered
        "integration/advisory-sync"
        "advisory-sync"
        ["advisory-sync"]
        ["docker"]
        ["ministack"]
        [ "integration/advisory-sync/native-001"
        ]

-- | Declared coverage for integration/advisory-export.
advisoryExportS3 :: Group
advisoryExportS3 =
    registered
        "integration/advisory-export"
        "advisory"
        ["advisory-publication", "advisory-last-good"]
        ["docker"]
        ["ministack"]
        [ "integration/advisory-export/native-001"
        , "integration/advisory-export/native-002"
        , "advisory/s3/last-good"
        ]

-- | Declared coverage for integration/queue-sqs.
queueSqs :: Group
queueSqs =
    registered
        "integration/queue-sqs"
        "queue"
        ["queue-payload", "queue-lease", "queue-redelivery"]
        ["docker"]
        ["ministack"]
        [ "integration/queue-sqs/native-001"
        , "integration/queue-sqs/native-002"
        , "integration/queue-sqs/native-003"
        , "integration/queue-sqs/native-004"
        , "integration/queue-sqs/native-005"
        , "integration/queue-sqs/native-006"
        , "queue/sqs/payload"
        , "queue/sqs/redelivery"
        ]

-- | Declared coverage for integration/server-shutdown.
serverShutdownHttp :: Group
serverShutdownHttp =
    registered
        "integration/server-shutdown"
        "server-shutdown"
        ["server-shutdown"]
        []
        ["wai"]
        [ "integration/server-shutdown/native-001"
        , "integration/server-shutdown/native-002"
        ]

-- | Declared coverage for integration/telemetry-metrics.
collectorMetrics :: Group
collectorMetrics =
    registered
        "integration/telemetry-metrics"
        "telemetry-metrics"
        ["telemetry-metrics"]
        ["docker"]
        ["collector"]
        [ "integration/telemetry-metrics/native-001"
        , "integration/telemetry-metrics/native-002"
        ]

-- | Declared coverage for integration/telemetry-scrape.
telemetryScrapeHttp :: Group
telemetryScrapeHttp =
    registered
        "integration/telemetry-scrape"
        "telemetry-scrape"
        ["telemetry-scrape"]
        []
        ["wai"]
        [ "integration/telemetry-scrape/native-001"
        , "integration/telemetry-scrape/native-002"
        , "integration/telemetry-scrape/native-003"
        , "integration/telemetry-scrape/native-004"
        , "integration/telemetry-scrape/native-005"
        , "integration/telemetry-scrape/native-006"
        ]

-- | Declared coverage for integration/telemetry-traces.
collectorTraces :: Group
collectorTraces =
    registered
        "integration/telemetry-traces"
        "telemetry-traces"
        ["telemetry-traces"]
        ["docker"]
        ["collector"]
        [ "integration/telemetry-traces/native-001"
        , "integration/telemetry-traces/native-002"
        , "integration/telemetry-traces/native-003"
        ]

registered :: Text -> Text -> [Text] -> [Text] -> [Text] -> [Text] -> Group
registered identifier contract capabilities prerequisites resources identifiers =
    Group identifier contract identifier Integration "integration" [Case item [identifier] | item <- identifiers] capabilities prerequisites resources ["serial-environment"]
