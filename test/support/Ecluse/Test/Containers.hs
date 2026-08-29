-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Labelling and reaping scope for the Docker containers the integration and
end-to-end suites spin up.

Both harnesses stamp every container with @com.ecluse.test@ (the suite) and
@com.ecluse.test.scope@ (a per-worktree id from @ECLTEST_SCOPE@, which the
container-running @task@ targets pin). A scoped reap therefore removes only this
worktree's containers, never a sibling's. @scripts\/test-containers.sh@ is the matching
reader, so the two cannot drift on the spelling. See @docs\/testing.md@.
-}
module Ecluse.Test.Containers (
    testContainerLabels,
    dockerLabelArgs,
) where

-- 'lookupEnv', 'Text', 'toText', 'toString', and '<&>' all come from the relude prelude.

{- | The reaping scope for the current run. It is @ECLTEST_SCOPE@ when a container-running
@task@ target pins that variable to this worktree's id, and @local@ otherwise.
-}
testScope :: IO Text
testScope =
    lookupEnv "ECLTEST_SCOPE" <&> \case
        Just s | not (null s) -> toText s
        _ -> "local"

{- | The label pairs every test container carries: the suite marker (@integration@ or @e2e@)
keyed by @com.ecluse.test@, and the reaping scope keyed by @com.ecluse.test.scope@.
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
