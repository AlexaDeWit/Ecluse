-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed codeartifact adapter conformance, with deterministic collaborators.
module Main (main) where

import Ecluse.Runtime.Test.PackageStore (codeArtifactCatalogue, codeArtifactDriver, codeArtifactPyPIDriver, codeArtifactSuiteGroups, validateCodeArtifact)
import Ecluse.Test.Contract.PackageStore (fullPackageStoreContract)
import Ecluse.Test.Contract.Runner (contractMain)

-- | Every required case must execute and pass.
main :: IO ()
main = do
    validateCodeArtifact
    contractMain codeArtifactCatalogue "package-store-codeartifact" (codeArtifactSuiteGroups (fullPackageStoreContract codeArtifactDriver <> fullPackageStoreContract codeArtifactPyPIDriver)) (const (pure Nothing))
