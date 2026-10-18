-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Concrete E2E acquisition, canonical schedules, and native registrations.
module Ecluse.E2E.Fixtures.Contracts (
    ConcreteFixture (..),
    concreteFamily,
    npmRegistryFixture,
    npmInstallationFixture,
    pypiInstallationFixture,
    pypiTelemetryFixture,
    pypiMirrorFixture,
    pypiMirrorNativeFixture,
    pypiPublicationFixture,
    pypiPublicationNativeFixture,
    pypiMirrorTelemetryFixture,
    npmMirrorFixture,
    npmPublicationFixture,
    npmDredgerFixture,
    pypiPublicFixture,
    pypiPilotFixture,
    npmPilotFixture,
    pypiAdvisoryNativeFixture,
    npmMirrorTelemetryFixture,
    npmTelemetryFixture,
    mixedRegistryFixture,
) where

import Ecluse.E2E.Harness (
    GlobalDataPlane,
    Topology (..),
    dredgerTopology,
    mixedTopology,
    npmTopology,
    pypiAdvisoryTopology,
    pypiTopology,
    withDataPlane,
    withGlobalDataPlane,
 )
import Ecluse.Test.Contract.Ecosystem (FamilyFixture (..), FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Types

-- | Each acquisition owns the complete data plane for one lifecycle.
data ConcreteFixture metadata = ConcreteFixture
    { fixtureMetadata :: metadata
    , withFixture :: (GlobalDataPlane -> IO ()) -> IO ()
    }

-- | npm reads and mirror effects through Verdaccio and the in-memory queue.
npmRegistryFixture :: ConcreteFixture Group
npmRegistryFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureMetadata =
            Group
                { groupId = "npm/install"
                , groupContractId = "npm/install"
                , groupProfileId = "npm/registry"
                , groupTier = E2E
                , groupPartition = "npm"
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
                    , Case "npm/install/mal-forced-source-required" ["npm/install"]
                    , Case "npm/install/mal-manual-source-required" ["npm/install"]
                    ]
                , groupCapabilities = ["read", "mirror", "client-install"]
                , groupPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                , groupResources = ["proxy", "nginx", "verdaccio"]
                , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | Fresh public and mirror-only npm installs through Verdaccio.
npmMirrorFixture :: ConcreteFixture FamilyMetadata
npmMirrorFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "npm/mirror"
                , familyProfile = "npm/registry"
                , familyCapabilities = ["read", "mirror", "client-install"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                , familyResources = ["proxy", "nginx", "verdaccio"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | First-party npm publication through the declared Verdaccio target.
npmPublicationFixture :: ConcreteFixture FamilyMetadata
npmPublicationFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "npm/publish"
                , familyProfile = "npm/registry"
                , familyCapabilities = ["read", "publication", "client-install"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                , familyResources = ["proxy", "nginx", "verdaccio"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | npm maintenance with Verdaccio stores and the advisory and queue emulators.
npmDredgerFixture :: ConcreteFixture FamilyMetadata
npmDredgerFixture =
    ConcreteFixture
        { withFixture = withDataPlane dredgerTopology
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "npm/dredger"
                , familyProfile = "npm/maintenance"
                , familyCapabilities = ["read", "mirror", "maintenance", "advisories"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                , familyResources = ["proxy", "nginx", "verdaccio", "ministack"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | Wheel-only pip installations through a public PyPI mount.
pypiPublicFixture :: ConcreteFixture Group
pypiPublicFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiTopology
        , fixtureMetadata =
            Group
                { groupId = "pypi/install"
                , groupContractId = "pypi/install"
                , groupProfileId = "pypi/read"
                , groupTier = E2E
                , groupPartition = "pypi"
                , groupCases =
                    [ Case "pypi/install/wheel" ["pypi/install"]
                    , Case "pypi/install/metadata-companion" ["pypi/install"]
                    , Case "pypi/install/tampered-companion" ["pypi/install"]
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
pypiPilotFixture :: ConcreteFixture FamilyMetadata
pypiPilotFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiAdvisoryTopology
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "pypi/advisories"
                , familyProfile = "pypi/advisories"
                , familyCapabilities = ["read", "advisories", "client-install"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "python3", "pip"]
                , familyResources = ["proxy", "nginx", "ministack"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | npm traffic with per-case collector and exporter configurations.
npmTelemetryFixture :: ConcreteFixture FamilyMetadata
npmTelemetryFixture =
    ConcreteFixture
        { withFixture = withGlobalDataPlane
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "npm/telemetry-read"
                , familyProfile = "npm/telemetry"
                , familyCapabilities = ["read", "mirror", "telemetry", "client-install"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                , familyResources = ["proxy", "nginx", "verdaccio", "collector"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | Alternating npm and pip clients behind one proxy.
mixedRegistryFixture :: ConcreteFixture Group
mixedRegistryFixture =
    ConcreteFixture
        { withFixture = withDataPlane mixedTopology
        , fixtureMetadata =
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

-- | npm read contracts have no private copy that can bypass public admission.
npmInstallationFixture :: ConcreteFixture FamilyMetadata
npmInstallationFixture =
    ConcreteFixture
        (installationMetadata "npm" "npm/read" ["npm", "node"])
        (withDataPlane npmTopology{topologyStore = False})

-- | PyPI uses the same required read scenarios with a real pip client.
pypiInstallationFixture :: ConcreteFixture FamilyMetadata
pypiInstallationFixture =
    ConcreteFixture
        (installationMetadata "pypi" "pypi/read" ["python3", "pip"])
        (withDataPlane pypiTopology)

installationMetadata :: Text -> Text -> [Text] -> FamilyMetadata
installationMetadata ecosystem profile tools =
    FamilyMetadata
        { familyId = ecosystem <> "/installation"
        , familyProfile = profile
        , familyCapabilities = ["read", "client-install"]
        , familyPrerequisites = ["docker", "image", "openssl", "tar"] <> tools
        , familyResources = ["proxy", "nginx"]
        , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }

-- | PyPI read traffic exercises telemetry without a mirror target.
pypiTelemetryFixture :: ConcreteFixture FamilyMetadata
pypiTelemetryFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiTopology
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "pypi/telemetry"
                , familyProfile = "pypi/telemetry"
                , familyCapabilities = ["read", "telemetry", "client-install"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "python3", "pip"]
                , familyResources = ["proxy", "nginx", "collector"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | Worker and private-fetch telemetry retains its own mutable mirror lifetime.
npmMirrorTelemetryFixture :: ConcreteFixture FamilyMetadata
npmMirrorTelemetryFixture =
    npmTelemetryFixture
        { fixtureMetadata =
            (fixtureMetadata npmTelemetryFixture)
                { familyId = "npm/telemetry-write"
                }
        }

-- | Scored sessions avoid trusted reads. The MAL session independently acquires its mirror store.
npmPilotFixture :: ConcreteFixture FamilyMetadata
npmPilotFixture =
    ConcreteFixture
        { withFixture = withDataPlane npmTopology{topologyStore = False, topologyAws = True}
        , fixtureMetadata =
            (fixtureMetadata pypiPilotFixture)
                { familyId = "npm/advisories"
                , familyProfile = "npm/advisories"
                , familyCapabilities = ["read", "mirror", "advisories", "client-install"]
                , familyResources = ["proxy", "nginx", "verdaccio", "ministack"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "npm", "node"]
                }
        }

-- | PEP 440 canonicalisation remains explicit beside the common advisory laws.
pypiAdvisoryNativeFixture :: ConcreteFixture Group
pypiAdvisoryNativeFixture = nativeFixture "pypi/advisory-native" ["pypi/advisories/canonical-fix"] pypiPilotFixture

-- | Acquire native observations inside the canonical family's measured data-plane lifetime.
concreteFamily :: ConcreteFixture FamilyMetadata -> (PhaseRecorder -> (a -> IO ()) -> GlobalDataPlane -> IO ()) -> FamilyFixture a
concreteFamily fixture acquire =
    FamilyFixture
        { familyMetadata = fixtureMetadata fixture
        , familyAcquire = \recorder action -> timedFixture recorder (withFixture fixture) (acquire recorder action)
        }

-- | Native PyPI mirroring uses SQS, S3, and the upload/read fixture.
pypiMirrorFixture :: ConcreteFixture FamilyMetadata
pypiMirrorFixture =
    ConcreteFixture
        { withFixture = withDataPlane pypiAdvisoryTopology
        , fixtureMetadata =
            FamilyMetadata
                { familyId = "pypi/mirror"
                , familyProfile = "pypi/write"
                , familyCapabilities = ["read", "mirror", "client-install", "telemetry"]
                , familyPrerequisites = ["docker", "image", "openssl", "tar", "python3", "pip"]
                , familyResources = ["proxy", "nginx", "native-target", "ministack"]
                , familyIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
                }
        }

-- | Native field preservation, restart, lost-response and mirrored-yank cases retain separate identities.
pypiMirrorNativeFixture :: ConcreteFixture Group
pypiMirrorNativeFixture = nativeFixture "pypi/mirror-native" ["pypi/mirror-native/" <> name | name <- ["read-roundtrip", "lost-reply", "interruption", "yanked-greatest", "yank-negative-control"]] pypiMirrorFixture

-- | First-party publication needs native HTTP storage and a fresh pip client, without AWS.
pypiPublicationFixture :: ConcreteFixture FamilyMetadata
pypiPublicationFixture =
    pypiMirrorFixture
        { withFixture = withDataPlane pypiTopology
        , fixtureMetadata =
            (fixtureMetadata pypiMirrorFixture)
                { familyId = "pypi/publish"
                , familyProfile = "pypi/publication"
                , familyCapabilities = ["read", "publication", "client-install"]
                , familyResources = ["proxy", "nginx", "native-target"]
                }
        }

-- | Multipart identity conflicts, hidden parts and a disabled route remain required native controls.
pypiPublicationNativeFixture :: ConcreteFixture Group
pypiPublicationNativeFixture = nativeFixture "pypi/publish-native" ["pypi/publish-native/" <> name | name <- ["conflicting-names", "hidden-parts", "unconfigured"]] pypiPublicationFixture

-- | Independent native projects provide worker and private-fetch telemetry.
pypiMirrorTelemetryFixture :: ConcreteFixture FamilyMetadata
pypiMirrorTelemetryFixture =
    pypiMirrorFixture
        { fixtureMetadata =
            (fixtureMetadata pypiMirrorFixture)
                { familyId = "pypi/telemetry-write"
                , familyProfile = "pypi/write-telemetry"
                , familyResources = ["proxy", "nginx", "native-target", "ministack", "collector"]
                }
        }

nativeFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata -> ConcreteFixture Group
nativeFixture identifier cases fixture =
    ConcreteFixture
        { withFixture = withFixture fixture
        , fixtureMetadata =
            Group
                { groupId = identifier
                , groupContractId = identifier
                , groupProfileId = familyProfile metadata
                , groupTier = E2E
                , groupPartition = "pypi"
                , groupCases = [Case name [identifier] | name <- cases]
                , groupCapabilities = familyCapabilities metadata
                , groupPrerequisites = familyPrerequisites metadata
                , groupResources = familyResources metadata
                , groupIsolation = familyIsolation metadata
                }
        }
  where
    metadata = fixtureMetadata fixture
