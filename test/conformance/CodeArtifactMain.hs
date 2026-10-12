-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed codeartifact adapter conformance, with deterministic collaborators.
module Main (main) where

import Ecluse.Runtime.Test.PackageStore (codeArtifactDriver)
import Ecluse.Test.Contract.PackageStore (fullPackageStoreContract, packageStoreCatalogue, validatePackageStore)
import Ecluse.Test.Contract.Runner (contractMain)

-- | Every required case must execute and pass.
main :: IO ()
main = do
    validatePackageStore "codeartifact"
    contractMain (packageStoreCatalogue "codeartifact") "package-store-codeartifact" (fullPackageStoreContract codeArtifactDriver) (const (pure Nothing))
