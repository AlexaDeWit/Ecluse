-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | E2E scheduling metadata is independent of Hspec construction and container acquisition.
module Ecluse.E2E.Catalogue (catalogue, groupSpecs, prerequisites) where

import Prelude hiding (universe)

import Data.Map.Strict qualified as Map
import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterPublish), adapterFor)
import Ecluse.E2E.Harness.Availability (e2ePrerequisites)
import Ecluse.E2E.MixedEcosystemE2ESpec qualified as Mixed
import Ecluse.E2E.Npm.DredgerE2ESpec qualified as NpmDredger
import Ecluse.E2E.Npm.InstallE2ESpec qualified as NpmInstall
import Ecluse.E2E.Npm.MirrorInstallE2ESpec qualified as NpmMirror
import Ecluse.E2E.Npm.PublishE2ESpec qualified as NpmPublish
import Ecluse.E2E.PyPI.AdvisoriesE2ESpec qualified as PyPIAdvisories
import Ecluse.E2E.PyPI.InstallE2ESpec qualified as PyPIInstall
import Ecluse.E2E.Requirements (obligations)
import Ecluse.E2E.TelemetryE2ESpec qualified as Telemetry
import Ecluse.Test.Contract.Runner (GroupAction)
import Ecluse.Test.Contract.Types

-- | Production adapter names, independent obligations, and concrete groups meet here.
catalogue :: Catalogue
catalogue =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName $ filter (isJust . adapterFor) universe
        , catalogueProfiles = profiles
        , catalogueObligations = map activateSupportedWrites obligations
        , catalogueGroups = groups
        }

profiles :: [Profile]
profiles =
    map
        withProductionFacts
        [ Profile "npm/registry" ["npm"] ["verdaccio", "memory"] ["read", "mirror", "publication", "client-install"]
        , Profile "npm/maintenance" ["npm"] ["verdaccio", "sqs", "s3"] ["read", "mirror", "maintenance", "advisories"]
        , Profile "pypi/read" ["pypi"] ["http"] ["read", "client-install"]
        , Profile "pypi/advisories" ["pypi"] ["http", "s3"] ["read", "advisories", "client-install"]
        , Profile "mixed/registry" ["npm", "pypi"] ["verdaccio", "memory"] ["read", "mirror", "client-install"]
        , Profile "npm/telemetry" ["npm"] ["verdaccio", "memory", "collector"] ["read", "mirror", "telemetry", "client-install"]
        ]

-- | Specifications remain lazy values and are evaluated only for selected groups.
groupSpecs :: Map Text GroupAction
groupSpecs =
    Map.fromList
        [ ("npm/install", NpmInstall.specWith)
        , ("npm/mirror", NpmMirror.specWith)
        , ("npm/publish", NpmPublish.specWith)
        , ("npm/dredger", NpmDredger.specWith)
        , ("pypi/install", PyPIInstall.specWith)
        , ("pypi/advisories", PyPIAdvisories.specWith)
        , ("telemetry", Telemetry.specWith)
        , ("mixed", Mixed.specWith)
        ]

-- | Missing tools fail setup without replacing declared examples with a pending placeholder.
prerequisites :: Group -> IO (Maybe Text)
prerequisites = fmap (fmap toText) . e2ePrerequisites . map toString . groupPrerequisites

groups :: [Group]
groups =
    [ Group
        { groupId = "npm/install"
        , groupContractId = "npm/install"
        , groupProfileId = "npm/registry"
        , groupTier = E2E
        , groupPartition = "npm-serving"
        , groupCases =
            [ Case "npm/install/default-denial" ["npm/install"]
            , Case "npm/install/lifecycle-script-control" ["npm/install"]
            , Case "npm/install/deprecation-relay" ["npm/install"]
            , Case "npm/install/deprecation-negative-control" ["npm/install"]
            , Case "npm/install/integrity-refusal" ["npm/install"]
            , Case "npm/install/head-no-effect" ["npm/install"]
            , Case "npm/install/metadata-preservation" ["npm/install"]
            , Case "npm/install/latest-preservation" ["npm/install"]
            , Case "npm/install/publication-unconfigured" ["npm/install"]
            ]
        , groupCapabilities = ["read", "mirror", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
        , groupResources = ["proxy", "nginx", "verdaccio"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "npm/mirror"
        , groupContractId = "npm/mirror"
        , groupProfileId = "npm/registry"
        , groupTier = E2E
        , groupPartition = "npm-writes"
        , groupCases =
            [ Case "npm/mirror/dependency-graph" ["npm/mirror"]
            , Case "npm/mirror/module-exports" ["npm/mirror"]
            , Case "npm/mirror/linked-executable" ["npm/mirror"]
            , Case "npm/mirror/origin-evidence" ["npm/mirror"]
            , Case "npm/mirror/installed-tree" ["npm/mirror"]
            , Case "npm/mirror/empty-store-control" ["npm/mirror"]
            ]
        , groupCapabilities = ["read", "mirror", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
        , groupResources = ["proxy", "nginx", "verdaccio"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "npm/publish"
        , groupContractId = "npm/publish"
        , groupProfileId = "npm/registry"
        , groupTier = E2E
        , groupPartition = "npm-writes"
        , groupCases =
            [ Case "npm/publish/publish-install" ["npm/publish"]
            , Case "npm/publish/scope-refusal" ["npm/publish"]
            ]
        , groupCapabilities = ["read", "publication", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
        , groupResources = ["proxy", "nginx", "verdaccio"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "npm/dredger"
        , groupContractId = "npm/dredger"
        , groupProfileId = "npm/maintenance"
        , groupTier = E2E
        , groupPartition = "npm-writes"
        , groupCases =
            [ Case "npm/dredger/inventory" ["npm/dredger"]
            , Case "npm/dredger/admission-only" ["npm/dredger"]
            , Case "npm/dredger/denied-versions" ["npm/dredger"]
            , Case "npm/dredger/dry-run" ["npm/dredger"]
            , Case "npm/dredger/consent-refusal" ["npm/dredger"]
            , Case "npm/dredger/first-party" ["npm/dredger"]
            , Case "npm/dredger/two-stores" ["npm/dredger"]
            , Case "npm/dredger/advisory-revocation" ["npm/dredger"]
            , Case "npm/dredger/denied-install" ["npm/dredger"]
            , Case "npm/dredger/delete-recovery" ["npm/dredger"]
            , Case "npm/dredger/refill" ["npm/dredger"]
            , Case "npm/dredger/missing-source" ["npm/dredger"]
            , Case "npm/dredger/late-copy" ["npm/dredger"]
            , Case "npm/dredger/old-worker" ["npm/dredger"]
            , Case "npm/dredger/old-dredger-refill" ["npm/dredger"]
            , Case "npm/dredger/old-dredger-loss" ["npm/dredger"]
            ]
        , groupCapabilities = ["read", "mirror", "maintenance", "advisories"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
        , groupResources = ["proxy", "nginx", "verdaccio", "ministack"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "pypi/install"
        , groupContractId = "pypi/install"
        , groupProfileId = "pypi/read"
        , groupTier = E2E
        , groupPartition = "pypi"
        , groupCases =
            [ Case "pypi/install/wheel" ["pypi/install"]
            , Case "pypi/install/yanked-pin" ["pypi/install"]
            , Case "pypi/install/yank-negative-control" ["pypi/install"]
            , Case "pypi/install/sdist" ["pypi/install"]
            , Case "pypi/install/outage" ["pypi/install"]
            ]
        , groupCapabilities = ["read", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "python3", "pip"]
        , groupResources = ["proxy", "nginx"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "pypi/advisories"
        , groupContractId = "pypi/advisories"
        , groupProfileId = "pypi/advisories"
        , groupTier = E2E
        , groupPartition = "pypi"
        , groupCases =
            [ Case "pypi/advisories/exact-fix" ["pypi/advisories"]
            , Case "pypi/advisories/canonical-fix" ["pypi/advisories"]
            , Case "pypi/advisories/deny-cve" ["pypi/advisories"]
            , Case "pypi/advisories/deny-epss" ["pypi/advisories"]
            ]
        , groupCapabilities = ["read", "advisories", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "python3", "pip"]
        , groupResources = ["proxy", "nginx", "ministack"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "telemetry"
        , groupContractId = "telemetry"
        , groupProfileId = "npm/telemetry"
        , groupTier = E2E
        , groupPartition = "shared-system"
        , groupCases =
            [ Case "telemetry/signals" ["telemetry"]
            , Case "telemetry/domain-spans" ["telemetry"]
            , Case "telemetry/private-fetch" ["telemetry"]
            , Case "telemetry/disabled" ["telemetry"]
            , Case "telemetry/export-failure" ["telemetry"]
            , Case "telemetry/datadog" ["telemetry"]
            ]
        , groupCapabilities = ["read", "mirror", "telemetry", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
        , groupResources = ["proxy", "nginx", "verdaccio", "collector"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    , Group
        { groupId = "mixed"
        , groupContractId = "mixed"
        , groupProfileId = "mixed/registry"
        , groupTier = E2E
        , groupPartition = "shared-system"
        , groupCases =
            [ Case "mixed/alternating-clients" ["mixed"]
            ]
        , groupCapabilities = ["read", "mirror", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node", "python3", "pip"]
        , groupResources = ["proxy", "nginx", "verdaccio"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }
    ]

withProductionFacts :: Profile -> Profile
withProductionFacts profile = profile{profileCapabilities = filter supported (profileCapabilities profile)}
  where
    adapters = map (parseEcosystem >=> adapterFor) (profileEcosystems profile)
    supported capability = case capability of
        "read" -> all isJust adapters
        "mirror" -> any (maybe False (isJust . adapterPublish)) adapters
        "publication" -> any (maybe False (isJust . adapterPublish)) adapters
        _ -> True

activateSupportedWrites :: Obligation -> Obligation
activateSupportedWrites obligation
    | obligationId obligation `elem` ["pypi/mirror", "pypi/publication"]
    , Just adapter <- parseEcosystem "pypi" >>= adapterFor
    , isJust (adapterPublish adapter) =
        obligation{obligationSupport = Supported}
    | otherwise = obligation
