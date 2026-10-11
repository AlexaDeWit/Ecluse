-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete topology acquisition and case declarations for E2E contracts.
module Ecluse.E2E.Fixtures.Contracts (
    ConcreteFixture (..),
    npmRegistryFixture,
    npmMirrorFixture,
    npmPublicationFixture,
    npmDredgerFixture,
    pypiPublicFixture,
    pypiPilotFixture,
    npmTelemetryFixture,
    mixedRegistryFixture,
) where

import Ecluse.E2E.Harness
import Ecluse.Test.Contract.Types

-- | Each acquisition owns the complete data plane for one lifecycle.
data ConcreteFixture = ConcreteFixture
    { fixtureGroup :: Group
    , withFixture :: (GlobalDataPlane -> IO ()) -> IO ()
    }

-- | npm reads and mirror effects through Verdaccio and the in-memory queue.
npmRegistryFixture :: ConcreteFixture
npmRegistryFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureGroup =
            Group
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
        }

-- | Fresh public and mirror-only npm installs through Verdaccio.
npmMirrorFixture :: ConcreteFixture
npmMirrorFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureGroup =
            Group
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
        }

-- | First-party npm publication through the declared Verdaccio target.
npmPublicationFixture :: ConcreteFixture
npmPublicationFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureGroup =
            Group
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
        }

-- | npm maintenance with Verdaccio stores and the advisory and queue emulators.
npmDredgerFixture :: ConcreteFixture
npmDredgerFixture =
    ConcreteFixture
        { withFixture = withDataPlane dredgerTopology
        , fixtureGroup =
            Group
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
        }

-- | Wheel-only pip installations through a public PyPI mount.
pypiPublicFixture :: ConcreteFixture
pypiPublicFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiTopology
        , fixtureGroup =
            Group
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
        }

-- | Pilot advisory publication and pip policy observations through a PyPI mount.
pypiPilotFixture :: ConcreteFixture
pypiPilotFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiAdvisoryTopology
        , fixtureGroup =
            Group
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
        }

-- | npm traffic with per-case collector and exporter configurations.
npmTelemetryFixture :: ConcreteFixture
npmTelemetryFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureGroup =
            Group
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
        }

-- | Alternating npm and pip clients behind one proxy.
mixedRegistryFixture :: ConcreteFixture
mixedRegistryFixture =
    ConcreteFixture
        { withFixture = withDataPlane mixedTopology
        , fixtureGroup =
            Group
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
        }
