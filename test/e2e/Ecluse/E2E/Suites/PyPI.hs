-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The full PyPI conformance subject, including named failures for missing drivers.
module Ecluse.E2E.Suites.PyPI (declarations) where

import Ecluse.E2E.Drivers.PyPI (pypiDriver)
import Ecluse.Test.Contract.Ecosystem (fullEcosystemContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | Missing required behaviour fails without stopping unrelated contracts.
declarations :: [DeclaredGroup]
declarations = fullEcosystemContract pypiDriver
