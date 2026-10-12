-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The explicitly read-only PyPI foundation, pending the separate full-parity activation.
module Ecluse.E2E.Suites.PyPI (declarations) where

import Ecluse.E2E.Drivers.PyPI (pypiDriver)
import Ecluse.Test.Contract.Ecosystem (readOnlyEcosystemFoundation)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | This foundation does not certify full ecosystem conformance.
declarations :: [DeclaredGroup]
declarations = readOnlyEcosystemFoundation pypiDriver
