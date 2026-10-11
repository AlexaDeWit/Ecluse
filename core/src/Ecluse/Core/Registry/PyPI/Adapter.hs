-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI's read-only adapter and the rule types its ecosystem cannot support.
Its protocol exposes no store listing or deletion. A separate control plane can supply those verbs.
-}
module Ecluse.Core.Registry.PyPI.Adapter (
    pypiAdapter,
) where

import Data.Map.Strict qualified as Map

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
import Ecluse.Core.Registry.PyPI.Metadata (pypiChargeFactors, pypiRead)
import Ecluse.Core.Registry.PyPI.Project (projectName, pypiNameLeadChars)
import Ecluse.Core.Registry.PyPI.Request qualified as PyPIRequest
import Ecluse.Core.Registry.PyPI.Route qualified as PyPIRoute

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyInstallTimeExecutionType))

-- | PyPI's capability record.
pypiAdapter :: RegistryAdapter
pypiAdapter =
    RegistryAdapter
        { adapterEcosystem = PyPI
        , adapterRuleBlocks =
            Map.fromList
                [ (DenyInstallTimeExecutionType, "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release.")
                , (AllowScopeType, "A PyPI project name has no scope, so this rule can never match one.")
                ]
        , adapterServe =
            AdapterServe
                { serveRouter = PyPIRoute.pypiRouter
                , serveRoutes = PyPIRoute.pypiRouteSpecs
                , serveCredential = pypiCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataRead = pypiRead
                , metadataAssemble = assembleSimpleDocument
                , metadataSerialise = serialiseSimpleDocument
                , metadataChargeFactors = pypiChargeFactors
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
