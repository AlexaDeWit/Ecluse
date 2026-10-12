-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The full npm ecosystem contract.
module Ecluse.E2E.Suites.Npm (declarations) where

import Ecluse.E2E.Drivers.Npm (npmDriver)
import Ecluse.Test.Contract.Ecosystem (fullEcosystemContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | Every required family uses the canonical composition.
declarations :: [DeclaredGroup]
declarations = fullEcosystemContract npmDriver
