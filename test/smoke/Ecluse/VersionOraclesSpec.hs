module Ecluse.VersionOraclesSpec (spec) where

import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

{- | Smoke test: the committed version-ordering fixture is still byte-identical
to what the /live/ reference oracles produce. Combined with the gating unit test
(which checks 'Ecluse.Package.compareVersions' against that same fixture), this
gives us — transitively — "our comparator agrees with node-semver, Python
@packaging@, and Ruby @Gem::Version@", validated against the live tools.

Non-gating by design (the smoke tier): if the oracles are not on @PATH@ (the Nix
dev shell provides them via the version-ordering inputs; a bare checkout may not),
the test pends rather than fails.
-}
spec :: Spec
spec =
    describe "version-ordering fixtures vs the live reference oracles" $
        it "regenerating from node-semver / packaging / Gem::Version reproduces the committed fixture" $ do
            tmpDir <- getTemporaryDirectory
            let regenerated = tmpDir <> "/ecluse-version-fixtures-smoke.txt"
            (code, _out, _err) <-
                readProcessWithExitCode "bash" [generatorScript, regenerated] ""
            case code of
                ExitFailure _ ->
                    pendingWith
                        "reference oracles unavailable; run via `nix develop` (node-semver / packaging / ruby)"
                ExitSuccess -> do
                    fresh <- readFileBS regenerated
                    baked <- readFileBS committedFixture
                    fresh `shouldBe` baked
  where
    generatorScript = "scripts/gen-version-fixtures.sh"
    committedFixture = "test/unit/fixtures/version-ordering.txt"
