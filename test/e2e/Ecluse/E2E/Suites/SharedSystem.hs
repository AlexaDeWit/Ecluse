-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed shared-system E2E contracts and concrete fixtures.
module Ecluse.E2E.Suites.SharedSystem (declarations) where

import Ecluse.E2E.Fixtures.Contracts (mixedRegistryFixture)
import Ecluse.E2E.MixedEcosystemE2ESpec (mixedMountContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | The complete executable contract list for this suite.
declarations :: [DeclaredGroup]
declarations =
    [ mixedMountContract mixedRegistryFixture
    ]
