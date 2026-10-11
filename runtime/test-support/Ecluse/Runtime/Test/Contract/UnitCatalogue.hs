-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Required backend coverage follows the runtime's production maintenance capability.
module Ecluse.Runtime.Test.Contract.UnitCatalogue (runtimeUnitCatalogue) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (codeArtifactFormat)
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.UnitCatalogue (unitObligation)

-- | Backend obligations remain required when the runtime cannot construct its maintenance format.
runtimeUnitCatalogue :: [Group] -> Catalogue
runtimeUnitCatalogue groups =
    Catalogue
        { catalogueProductionEcosystems = []
        , catalogueProfiles =
            [ Profile
                "unit/codeartifact"
                []
                ["codeartifact"]
                (["token"] <> [capability | maintenanceAvailable, capability <- ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"]])
            , Profile "unit/advisory-local" [] ["local"] ["last-good-generation"]
            ]
        , catalogueObligations =
            [ unitObligation "runtime/credential/codeartifact" "credential" "unit/codeartifact" ["token"] ["credential/codeartifact-stub/token"]
            , unitObligation
                "runtime/maintenance/codeartifact"
                "maintenance"
                "unit/codeartifact"
                ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"]
                [ "maintenance/codeartifact/facts/identity-ceiling"
                , "maintenance/codeartifact/facts/completion-refill"
                , "maintenance/codeartifact/facts/inventory-capability"
                , "maintenance/codeartifact/consented/consent"
                , "maintenance/codeartifact/inventory/packages"
                , "maintenance/codeartifact/inventory/versions"
                , "maintenance/codeartifact/delete/removed"
                ]
            , unitObligation "runtime/advisory/local" "advisory" "unit/advisory-local" ["last-good-generation"] ["advisory/local/last-good"]
            ]
        , catalogueGroups = groups
        }
  where
    maintenanceAvailable = isJust (codeArtifactFormat Npm)
