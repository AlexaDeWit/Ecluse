{- | Shared test-support library for Écluse's suites.

This internal library is the common home for helpers, fixtures, and specs that
more than one suite needs, so the unit, integration, smoke, and end-to-end
suites can draw on a single source instead of each carrying its own copy. It is
deliberately minimal for now: 'supportLinkageSpec' is a linkage probe that every
suite imports and runs, proving the library is built, links against the library
under test, and is reachable from each suite's discovered specs.
-}
module Ecluse.Test.Support (supportLinkageSpec) where

import Ecluse.Package (HashAlg (SHA256), renderHashAlg)
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
