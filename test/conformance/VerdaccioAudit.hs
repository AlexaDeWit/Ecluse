-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit the complete verdaccio report against its independent plan.
module Main (main) where

import Data.Text qualified as T

import Ecluse.Test.Contract.Manifest (auditFiles, failRunner)
import Ecluse.Test.Contract.PackageStore (packageStoreCatalogue, validatePackageStore)
import Ecluse.Test.Contract.Result (makePlan)

-- | A missing, failed, pending, or stale result cannot establish conformance.
main :: IO ()
main = do
    validatePackageStore "verdaccio"
    compiled <- either (failRunner . T.unlines) pure $ makePlan (packageStoreCatalogue "verdaccio")
    getArgs >>= \case
        [plan, report] -> auditFiles compiled plan [report]
        _ -> failRunner "usage: ecluse-conformance-verdaccio-audit PLAN REPORT"
