-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared test-support library for Écluse's suites.

The unit, integration, smoke, and end-to-end suites draw helpers, fixtures, and
specs from here instead of each carrying its own copy. 'supportLinkageSpec' is a
linkage probe every suite imports and runs. It proves the library is built, links
against the library under test, and is reachable from each suite's discovered
specs.
-}
module Ecluse.Test.Support (supportLinkageSpec, testServeAdmission, newTestLogEnv) where

import Ecluse.Core.Package (HashAlg (SHA256), renderHashAlg)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Test.Hspec (Spec, describe, it, shouldBe)

{- | A trivial spec that touches a stable export of the library under test. A suite
that runs this spec compiled and linked against both this support library and
@ecluse@.
-}
supportLinkageSpec :: Spec
supportLinkageSpec =
    describe "ecluse-test-support" $
        it "is linked into the suite and can see the library under test" $
            renderHashAlg SHA256 `shouldBe` "sha256"

{- | A serve admission for suites that do not test overload. The capacity sits far
above any test's concurrent in-flight load, so admission admits every request and
never sheds. It stands in for the process-wide bounded admission the boot path
sizes from @ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT@.
-}
testServeAdmission :: IO ServeAdmission
testServeAdmission = newServeAdmission 1_000_000

-- | A scribe-free LogEnv (no stdout output during the test run).
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")
