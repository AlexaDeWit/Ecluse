-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ownership checks need no Docker daemon.
module Ecluse.Test.ContainersSpec (spec) where

import Ecluse.Test.Containers
import Ecluse.Test.Env (withEnvVars)
import Test.Hspec

spec :: Spec
spec = describe "container ownership" $ do
    it "keeps worktree, invocation and shard labels distinct" $
        containerLabels "e2e" "tree" "run" "npm-serving"
            `shouldBe` [ ("com.ecluse.test", "e2e")
                       , ("com.ecluse.test.scope", "tree")
                       , ("com.ecluse.test.run", "run")
                       , ("com.ecluse.test.shard", "npm-serving")
                       ]
    it "refuses an unowned container lifetime" $
        withEnvVars keys [] $
            testContainerLabels "e2e" `shouldThrow` (== ContainerOwnershipMissing)
    it "refuses an incomplete owner despite a lock marker" $
        withEnvVars keys [("ECLTEST_CONTAINER_LOCK", "held"), ("ECLTEST_SCOPE", "tree")] $
            testContainerLabels "e2e" `shouldThrow` (== ContainerOwnershipMissing)
    it "passes all ownership labels to Docker" $
        withEnvVars keys (zip keys ["tree", "run", "pypi", "held"]) $
            dockerLabelArgs "e2e"
                `shouldReturn` [ "--label"
                               , "com.ecluse.test=e2e"
                               , "--label"
                               , "com.ecluse.test.scope=tree"
                               , "--label"
                               , "com.ecluse.test.run=run"
                               , "--label"
                               , "com.ecluse.test.shard=pypi"
                               ]
  where
    keys = ["ECLTEST_SCOPE", "ECLTEST_RUN_ID", "ECLTEST_SHARD", "ECLTEST_CONTAINER_LOCK"]
