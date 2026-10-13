-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Keep automatic discovery while reporting actual canonical unit outcomes when requested.
module SpecHook (hook) where

import Test.Hspec (Spec)

import Ecluse.Runtime.Test.Contract.UnitCatalogueSpec (unitCatalogue)
import Ecluse.Runtime.Test.PackageStore (codeArtifactCatalogue)
import Ecluse.Test.Contract.Catalogue (combineCatalogues)
import Ecluse.Test.Contract.UnitReport (unitReportHook)

-- | Local runs retain Hspec options without requiring report identities or artifact paths.
hook :: Spec -> Spec
hook = unitReportHook "runtime-unit" $ do
    catalogue <- unitCatalogue
    pure $ combineCatalogues [catalogue, codeArtifactCatalogue]
