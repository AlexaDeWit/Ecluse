-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's native read capabilities require measured metadata charge factors.
module Ecluse.Core.Registry.Cargo.Adapter (
    cargoAdapter,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (..), AdapterMaintenance (..), AdapterMetadata (..), PrivateArtifactAddressing (AdmittedIndexedArtifactLocation))
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (..), RegistryAdapter (..))
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Execution (newCargoInstallEvidence)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Project (cargoNameChars, projectName)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublish)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts, cargoArtifactRequest)
import Ecluse.Core.Registry.Cargo.Route (cargoRouteSpecs, cargoRouter)
import Ecluse.Core.Registry.Cargo.Serving (assembleCargoDocument, serialiseCargoDocument)
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType))
import Ecluse.Core.Server.Admission.Types (ChargeFactors)

-- | Supply native measurements before registering the adapter in the runtime.
cargoAdapter :: ChargeFactors -> RegistryAdapter
cargoAdapter factors =
    RegistryAdapter
        { adapterEcosystem = Cargo
        , adapterRuleBlocks =
            Map.fromList
                [ (AllowScopeType, "Cargo package names have no scope.")
                , (DenyIfDeprecatedType, "Cargo's sparse index supplies no deprecation flag.")
                ]
        , adapterServe =
            AdapterServe
                { serveRouter = cargoRouter
                , serveRoutes = cargoRouteSpecs
                , serveCredential = cargoCredential
                }
        , adapterMetadata =
            AdapterMetadata
                { metadataRead = prepareCargoRead
                , metadataAssemble = assembleCargoDocument
                , metadataSerialise = serialiseCargoDocument
                , metadataChargeFactors = factors
                }
        , adapterArtifact =
            AdapterArtifact
                { artifactByFile = \_ _ _ -> Left (UnparseableUrl "Cargo archives require an index-selected location")
                , artifactByUrl = cargoArtifactRequest
                , artifactHosts = cargoArtifactHosts
                , artifactPrivateAddressing = AdmittedIndexedArtifactLocation
                }
        , adapterProjectName = rightToMaybe . projectName
        , adapterPublish = Just (cargoPublish factors)
        , adapterMaintenance =
            AdapterMaintenance
                { maintenanceListing = Nothing
                , maintenanceVersionDelete = Nothing
                , maintenanceAlphabet = mkNameAlphabet cargoNameChars
                }
        , adapterInstallEvidence = Just newCargoInstallEvidence
        }
