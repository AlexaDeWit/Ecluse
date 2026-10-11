-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed pypi E2E contracts and concrete fixtures.
module Ecluse.E2E.Suites.PyPI (declarations) where

import Ecluse.E2E.Fixtures.Contracts (pypiPilotFixture, pypiPublicFixture)
import Ecluse.E2E.PyPI.AdvisoriesE2ESpec (advisoryContract)
import Ecluse.E2E.PyPI.InstallE2ESpec (installationContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | The complete executable contract list for this suite.
declarations :: [DeclaredGroup]
declarations =
    [ installationContract pypiPublicFixture
    , advisoryContract pypiPilotFixture
    ]
