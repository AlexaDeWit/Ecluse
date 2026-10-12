-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The complete PyPI ecosystem contract and its explicit native extensions.
module Ecluse.E2E.Suites.PyPI (declarations) where

import Ecluse.E2E.Drivers.PyPI (pypiDriver)
import Ecluse.Test.Contract.Ecosystem (fullEcosystemContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | Every required family remains part of the executable suite.
declarations :: [DeclaredGroup]
declarations = fullEcosystemContract pypiDriver
