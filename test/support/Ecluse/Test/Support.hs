-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared test-support library for Écluse's suites.

This internal library is the common home for helpers, fixtures, and specs that
more than one suite needs, so the unit, integration, smoke, and end-to-end
suites can draw on a single source instead of each carrying its own copy. It is
deliberately minimal for now: 'supportLinkageSpec' is a linkage probe that every
suite imports and runs, proving the library is built, links against the library
under test, and is reachable from each suite's discovered specs.
-}
module Ecluse.Test.Support (supportLinkageSpec, testServeAdmission) where

import Ecluse.Core.Package (HashAlg (SHA256), renderHashAlg)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Test.Hspec (Spec, describe, it, shouldBe)

{- | A trivial spec that touches a stable export of the library under test, so a
suite that runs it has genuinely compiled and linked against both this
support library and @ecluse@.
-}
supportLinkageSpec :: Spec
supportLinkageSpec =
    describe "ecluse-test-support" $
        it "is linked into the suite and can see the library under test" $
            renderHashAlg SHA256 `shouldBe` "sha256"

{- | A generously-bounded serve admission for suites whose subject is unrelated to
overload: the capacity is set far above any test's concurrent in-flight load, so
admission admits every request and never sheds. It stands in for the process-wide
bounded admission the boot path sizes from @ECLUSE_SERVE_MAX_IN_FLIGHT@.
-}
testServeAdmission :: IO ServeAdmission
testServeAdmission = newServeAdmission 1_000_000
