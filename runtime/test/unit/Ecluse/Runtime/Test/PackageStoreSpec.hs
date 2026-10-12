-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact's production adapter executes the deterministic persistence laws.
module Ecluse.Runtime.Test.PackageStoreSpec (spec) where

import Test.Hspec (Spec)

import Ecluse.Runtime.Test.PackageStore (codeArtifactDriver)
import Ecluse.Test.Contract.PackageStore (packageStoreSpec)

-- | No AWS service, credentials, IAM, or provisioning participates in these unit laws.
spec :: Spec
spec = packageStoreSpec codeArtifactDriver
