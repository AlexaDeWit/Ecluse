-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Phase measurement for resource hooks that receive an outer environment.
module Ecluse.E2E.Harness.Lifecycle (timedFixtureWith) where

import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)

-- | Record acquisition and teardown without charging the nested action twice.
timedFixtureWith :: PhaseRecorder -> ((a -> IO ()) -> b -> IO ()) -> (a -> IO ()) -> b -> IO ()
timedFixtureWith recorder fixture action outer = timedFixture recorder (`fixture` outer) action
