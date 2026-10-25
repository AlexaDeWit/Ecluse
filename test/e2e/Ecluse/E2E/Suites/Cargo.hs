-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The fixed Cargo subject composes every full ecosystem family and native obligation.
module Ecluse.E2E.Suites.Cargo (declarations) where

import Ecluse.E2E.Drivers.Cargo (cargoDriver)
import Ecluse.Test.Contract.Ecosystem (fullEcosystemContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | Missing providers and cases remain execution failures in the normal Cargo CI subject.
declarations :: [DeclaredGroup]
declarations = fullEcosystemContract cargoDriver
