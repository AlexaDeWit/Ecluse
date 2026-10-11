-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}

-- | Shared observations for deletion, inventory readiness and denied client reads.
module Ecluse.Test.Contract.Sweep (listed, refusedNext, inventoriesEmpty) where

import Ecluse.Test.Contract.Install (shouldFail)
import Ecluse.Test.Contract.Sweep.Types
import Test.Hspec

-- | A listing timeout remains a failed observation, never an empty inventory.
listed :: StoreOps store -> store -> [Text] -> IO ()
listed StoreOps{..} store names =
    for_ names $ \name -> do
        unlisted <- storeAwaitListed store name
        whenJust unlisted (expectationFailure . toString)

-- | Neither artifact access nor a fresh client may retrieve the denied version.
refusedNext :: ClientOps store project -> store -> Text -> Text -> IO ()
refusedNext ClientOps{..} proxy name version = do
    (status, _) <- clientArtifact proxy name version
    status `shouldBe` 403
    void $ clientWithProject proxy (\project -> clientInstallIn project name (Just version)) >>= shouldFail

-- | Inspect both stores by name without discarding unreadable-store failures.
inventoriesEmpty :: StoreOps store -> store -> store -> Text -> Expectation
inventoriesEmpty StoreOps{..} mirror cache name = do
    storeVersions mirror name `shouldReturn` []
    storeVersions cache name `shouldReturn` []
