-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Main (main) where

import Data.Text qualified as T

import Ecluse.E2E.Catalogue (catalogue)
import Ecluse.Test.Contract.Manifest (auditFiles, failRunner)
import Ecluse.Test.Contract.Result (makePlan)

main :: IO ()
main = do
    compiled <- either (failRunner . T.unlines) pure $ makePlan catalogue
    getArgs >>= \case
        plan : reports@(_ : _) -> auditFiles compiled plan reports
        _ -> failRunner "usage: ecluse-e2e-audit PLAN REPORT..."
