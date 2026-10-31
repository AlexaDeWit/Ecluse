-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Container ownership shared by the harnesses and the invocation reaper.
module Ecluse.Test.Containers (
    ContainerOwnershipMissing (..),
    testContainerLabels,
    dockerLabelArgs,
) where

import UnliftIO.Exception (throwIO)

-- | A container caller bypassed the task wrapper that owns the daemon lock.
data ContainerOwnershipMissing = ContainerOwnershipMissing
    deriving stock (Eq, Show)

instance Exception ContainerOwnershipMissing where
    displayException _ = "Container ownership is missing. Use task test-integration or task test-e2e."

-- Labels identify a worktree, invocation and shard without merging their ownership.
containerLabels :: Text -> Text -> Text -> Text -> [(Text, Text)]
containerLabels suite scope run shard =
    [ ("com.ecluse.test", suite)
    , ("com.ecluse.test.scope", scope)
    , ("com.ecluse.test.run", run)
    , ("com.ecluse.test.shard", shard)
    ]

-- | Require the task wrapper's owned lifetime before creating a Docker resource.
testContainerLabels :: Text -> IO [(Text, Text)]
testContainerLabels suite = do
    scope <- lookupEnv "ECLTEST_SCOPE"
    run <- lookupEnv "ECLTEST_RUN_ID"
    shard <- lookupEnv "ECLTEST_SHARD"
    lock <- lookupEnv "ECLTEST_CONTAINER_LOCK"
    case (scope, run, shard, lock) of
        (Just s, Just r, Just p, Just "held")
            | not (any null [s, r, p]) -> pure (containerLabels suite (toText s) (toText r) (toText p))
        _ -> throwIO ContainerOwnershipMissing

-- | Render ownership labels for raw Docker commands.
dockerLabelArgs :: Text -> IO [String]
dockerLabelArgs suite =
    concatMap (\(k, v) -> ["--label", toString (k <> "=" <> v)]) <$> testContainerLabels suite
