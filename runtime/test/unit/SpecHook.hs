-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Keep automatic discovery while reporting actual canonical unit outcomes when requested.
module SpecHook (hook) where

import Test.Hspec (Spec)

import Ecluse.Runtime.Credential.CodeArtifactSpec qualified as Credential
import Ecluse.Runtime.Cve.Sync.StepSpec qualified as Advisory
import Ecluse.Runtime.Maintenance.CodeArtifactSpec qualified as Maintenance
import Ecluse.Runtime.Test.Contract.UnitCatalogue (runtimeUnitCatalogue)
import Ecluse.Runtime.Test.PackageStore (codeArtifactCatalogue)
import Ecluse.Test.Contract.Binding (labelledCaseIds)
import Ecluse.Test.Contract.Catalogue (combineCatalogues)
import Ecluse.Test.Contract.Types (Group)
import Ecluse.Test.Contract.UnitCatalogue (unitRegistration)
import Ecluse.Test.Contract.UnitReport (unitReportHook)

-- | Local runs retain Hspec options without requiring report identities or artifact paths.
hook :: Spec -> Spec
hook = unitReportHook "runtime-unit" $ do
    groups <- registrations
    pure $ combineCatalogues [runtimeUnitCatalogue groups, codeArtifactCatalogue]

-- The labelled examples of each registered spec, read without running its examples or hooks.
registrations :: IO [Group]
registrations =
    sequence
        [ unitRegistration "runtime/credential/codeartifact" "credential" "unit/codeartifact" ["token"] <$> labelledCaseIds Credential.spec
        , unitRegistration "runtime/maintenance/codeartifact" "maintenance" "unit/codeartifact" ["maintenance-facts", "maintenance-consent", "maintenance-inventory", "maintenance-delete"] <$> labelledCaseIds Maintenance.spec
        , unitRegistration "runtime/advisory/local" "advisory" "unit/advisory-local" ["last-good-generation"] <$> labelledCaseIds Advisory.spec
        ]
