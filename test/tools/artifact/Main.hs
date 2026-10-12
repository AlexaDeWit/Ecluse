-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed filesystem operations for Task's artifact lifecycle.
module Main (main) where

import Ecluse.Test.Contract.Artifact.Attempt
import Ecluse.Test.Contract.Artifact.Identity
import Ecluse.Test.Contract.Manifest (failRunner)

main :: IO ()
main =
    getArgs >>= \case
        ["prepare-begin", root] -> beginPreparation root
        ["prepare-finish", root, image] -> finishPreparation root (toText image)
        ["verify-plan", root] -> void (verifyPlan root)
        ["verify", root] -> void (verifyPreparation root Nothing)
        ["verify", root, image] -> void (verifyPreparation root (Just (toText image)))
        ["attempt-begin", root, shard, attempt] -> beginAttempt root (toText shard) attempt
        ["attempt-finish", root, shard, attempt, status]
            | Just code <- readMaybe status -> finishAttempt root (toText shard) attempt code >>= exitWith
        ["attempt-continue", root, shard, attempt] -> continueAttempt root (toText shard) attempt
        _ -> failRunner "usage: ecluse-artifact prepare-begin ROOT | prepare-finish ROOT IMAGE | verify ROOT [IMAGE] | verify-plan ROOT | attempt-begin ROOT SHARD ATTEMPT | attempt-finish ROOT SHARD ATTEMPT STATUS | attempt-continue ROOT SHARD ATTEMPT"
