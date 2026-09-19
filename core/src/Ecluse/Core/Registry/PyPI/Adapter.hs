-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI's entry in the ecosystem adapter registry, assembled from the
@Ecluse.Core.Registry.PyPI.*@ modules with no protocol logic of its own.

'adapterPublish' is 'Nothing', so the upload route answers its documented @405@ and the
composition root refuses a write destination on the mount. 'AdapterMaintenance' carries neither
verb, because PyPI spells no public wire endpoint for listing a store's projects or deleting a
release, so @ecluse dredger@ refuses such a store and names the missing verb. The alphabet is
declared regardless: a store with a control plane of its own is still walked in buckets.
-}
module Ecluse.Core.Registry.PyPI.Adapter (
    pypiAdapter,
) where

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact (..),
    AdapterMaintenance (..),
    AdapterMetadata (..),
 )
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken), originBaseUrl)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Filter (assembleSimpleDocument, serialiseSimpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (fetchPyPIManifest, newPyPIMetadataReads)
import Ecluse.Core.Registry.PyPI.Project (projectName, pypiNameLeadChars)
import Ecluse.Core.Registry.PyPI.Request qualified as PyPIRequest
import Ecluse.Core.Registry.PyPI.Route qualified as PyPIRoute

-- | PyPI's capability record.
pypiAdapter :: RegistryAdapter
pypiAdapter =
    RegistryAdapter
        { adapterEcosystem = PyPI
        , adapterServe =
            AdapterServe
                { serveRouter = PyPIRoute.pypiRouter
                , serveRoutes = PyPIRoute.pypiRouteSpecs
                , serveCredential = pypiCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataNewReads = newPyPIMetadataReads
                , metadataAssemble = assembleSimpleDocument
                , metadataSerialise = serialiseSimpleDocument
                , metadataFetchManifest = fetchPyPIManifest
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \origin -> PyPIRequest.artifactRequestByFile (originBaseUrl origin) (ocToken origin)
                , artifactByUrl = PyPIRequest.artifactRequestByUrl
                , artifactHosts = PyPIRequest.pypiArtifactHosts
                }
        , adapterProjectName = rightToMaybe . projectName
        , adapterPublish = Nothing
        , adapterMaintenance =
            AdapterMaintenance
                { maintenanceListing = Nothing
                , maintenanceVersionDelete = Nothing
                , maintenanceAlphabet = mkNameAlphabet pypiNameLeadChars
                }
        }
