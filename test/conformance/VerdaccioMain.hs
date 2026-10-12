-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed verdaccio adapter conformance, with deterministic collaborators.
module Main (main) where

import Ecluse.Test.Contract.PackageStore (fullPackageStoreContract, packageStoreCatalogue, validatePackageStore)
import Ecluse.Test.Contract.PackageStore.Protocol (verdaccioDriver)
import Ecluse.Test.Contract.Runner (contractMain)

-- | Every required case must execute and pass.
main :: IO ()
main = do
    validatePackageStore "verdaccio"
    contractMain (packageStoreCatalogue "verdaccio") "package-store-verdaccio" (fullPackageStoreContract verdaccioDriver) (const (pure Nothing))
