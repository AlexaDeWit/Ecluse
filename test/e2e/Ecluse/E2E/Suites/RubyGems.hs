-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The fixed RubyGems subject requires the full composition and its independent native obligations.
module Ecluse.E2E.Suites.RubyGems (declarations) where

import Ecluse.E2E.Drivers.RubyGems (rubyDriver)
import Ecluse.E2E.RubyGems.RegistrationE2ESpec (rubyRegistrationContract)
import Ecluse.Test.Contract.Ecosystem (fullEcosystemContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | Missing providers, tools and observations remain failures in the normal fixed CI subject.
declarations :: [DeclaredGroup]
declarations = families <> [rubyRegistrationContract families]
  where
    families = fullEcosystemContract rubyDriver
