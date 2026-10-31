-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The full package-store laws over 'maintenanceFor' in every CodeArtifact format: npm, PyPI and Cargo.
module Ecluse.Runtime.Maintenance.CodeArtifact.InternalSpec (spec) where

import Test.Hspec

import Ecluse.Runtime.Test.PackageStore (codeArtifactCargoDriver, codeArtifactDriver, codeArtifactPyPIDriver)
import Ecluse.Test.Contract.PackageStore (packageStoreSpec)

-- | No AWS service, credentials, IAM, or provisioning participates in these unit laws.
spec :: Spec
spec = do
    packageStoreSpec codeArtifactDriver
    packageStoreSpec codeArtifactPyPIDriver
    packageStoreSpec codeArtifactCargoDriver
