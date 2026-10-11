-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Main (main) where

import Data.Text qualified as T

import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Manifest (auditFiles, failRunner)
import Ecluse.Test.Contract.Result (makePlan)

main :: IO ()
main = do
    compiled <- either (failRunner . T.unlines) pure $ makePlan integrationCatalogue
    getArgs >>= \case
        [plan, report] -> auditFiles compiled plan [report]
        _ -> failRunner "usage: ecluse-integration-audit PLAN REPORT"
