-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed npm-serving E2E contracts and concrete fixtures.
module Ecluse.E2E.Suites.NpmServing (declarations) where

import Ecluse.E2E.Fixtures.Contracts (npmRegistryFixture)
import Ecluse.E2E.Npm.InstallE2ESpec (installationContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | The complete executable contract list for this suite.
declarations :: [DeclaredGroup]
declarations =
    [ installationContract npmRegistryFixture
    ]
