{- | Labelling and reaping scope for the Docker containers the integration and
end-to-end suites spin up.

Integration ('TestContainers.Hspec.withContainers') and e2e (the raw @docker@
harness in "Ecluse.E2E.Harness.Docker") both stamp every container they create with two
labels, so a killed or interrupted run can be swept up afterwards rather than left to
accumulate:

  * @com.ecluse.test@ names the suite (@integration@ or @e2e@); and
  * @com.ecluse.test.scope@ carries a per-worktree id (from @ECLUSE_TEST_SCOPE@, set by
    the @task test-*@ targets) so a scoped reap only ever removes /this/ worktree's
    containers and never a sibling worktree's live ones.

@scripts\/test-containers.sh@ is the reaper that reads these labels; this module is the
matching writer, so the two cannot drift on the label spelling. See @docs\/testing.md@
-> "Tests and Docker".
-}
module Ecluse.Test.Containers (
    testScope,
    testContainerLabels,
    dockerLabelArgs,
) where

-- 'lookupEnv', 'Text', 'toText', 'toString', and '<&>' all come from the relude prelude.

{- | The reaping scope for the current run: @ECLUSE_TEST_SCOPE@ when the @task test-*@
targets have pinned it (to this worktree's id), else @local@ for a bare @cabal test@
invocation.
-}
testScope :: IO Text
testScope =
    lookupEnv "ECLUSE_TEST_SCOPE" <&> \case
        Just s | not (null s) -> toText s
        _ -> "local"

{- | The label pairs every test container carries: the suite marker keyed by
@com.ecluse.test@ and the reaping scope keyed by @com.ecluse.test.scope@. Shaped for
testcontainers' 'TestContainers.Docker.withLabels'.
-}
testContainerLabels :: Text -> IO [(Text, Text)]
testContainerLabels suite = do
    scope <- testScope
    pure [("com.ecluse.test", suite), ("com.ecluse.test.scope", scope)]

{- | 'testContainerLabels' rendered as @docker run@ \/ @docker network create@ arguments
(@--label k=v@ pairs), for the raw-@docker@ e2e harness.
-}
dockerLabelArgs :: Text -> IO [String]
dockerLabelArgs suite =
    concatMap (\(k, v) -> ["--label", toString (k <> "=" <> v)]) <$> testContainerLabels suite
