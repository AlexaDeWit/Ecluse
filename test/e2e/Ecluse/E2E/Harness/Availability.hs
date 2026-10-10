-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The guard every end-to-end spec module opens with.
module Ecluse.E2E.Harness.Availability (whenE2EAvailable) where

import Test.Hspec (Spec, it, pendingWith, runIO)

import Ecluse.E2E.Harness.Docker (e2eUnavailable)

{- | The scenarios when the tier can run. Otherwise one pending case that names the missing
prerequisite, so a bare @cabal test@ never fails on a machine without the setup.
-}
whenE2EAvailable :: Spec -> Spec
whenE2EAvailable scenarios =
    runIO e2eUnavailable >>= \case
        Just reason -> it "end-to-end suite (environment unavailable)" (pendingWith reason)
        Nothing -> scenarios
