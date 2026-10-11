-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed npm-writes E2E contracts and concrete fixtures.
module Ecluse.E2E.Suites.NpmWrites (declarations) where

import Ecluse.E2E.Fixtures.Contracts (npmDredgerFixture, npmMirrorFixture, npmPublicationFixture)
import Ecluse.E2E.Npm.DredgerE2ESpec (dredgerContract)
import Ecluse.E2E.Npm.MirrorInstallE2ESpec (mirrorContract)
import Ecluse.E2E.Npm.PublishE2ESpec (publicationContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup)

-- | The complete executable contract list for this suite.
declarations :: [DeclaredGroup]
declarations =
    [ mirrorContract npmMirrorFixture
    , publicationContract npmPublicationFixture
    , dredgerContract npmDredgerFixture
    ]
