-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit backend registrations while leaving stub servers and advisory stores unopened.
module Ecluse.Runtime.Test.Contract.UnitCatalogueSpec (spec, unitCatalogue) where

import Ecluse.Runtime.Credential.CodeArtifactSpec qualified as Credential
import Ecluse.Runtime.Cve.Sync.StepSpec qualified as Advisory
import Ecluse.Runtime.Maintenance.CodeArtifactSpec qualified as Maintenance
import Ecluse.Runtime.Test.Contract.UnitCatalogue (runtimeUnitCatalogue)
import Ecluse.Test.Contract.Binding (labelledCaseIds)
import Ecluse.Test.Contract.Types (Catalogue, Group)
import Ecluse.Test.Contract.UnitCatalogue
import Test.Hspec

-- | The inspected specifications acquire their resources only inside examples and hooks.
spec :: Spec
spec =
    describe "runtime unit contract coverage" $
        unitCoverageSpec unitCatalogue

-- | The receipt hook shares the same independently audited registration source.
unitCatalogue :: IO Catalogue
unitCatalogue = runtimeUnitCatalogue <$> registrations

registrations :: IO [Group]
registrations =
    sequence
        [ unitRegistration "runtime/credential/codeartifact" "credential" "unit/codeartifact" ["token"] <$> labelledCaseIds Credential.spec
        , unitRegistration "runtime/maintenance/codeartifact" "maintenance" "unit/codeartifact" ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"] <$> labelledCaseIds Maintenance.spec
        , unitRegistration "runtime/advisory/local" "advisory" "unit/advisory-local" ["last-good-generation"] <$> labelledCaseIds Advisory.spec
        ]
