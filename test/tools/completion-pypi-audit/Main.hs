-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit the fixed pypi completion definition against unit, integration and E2E receipts.
module Main (main) where

import Data.Text qualified as T

import Ecluse.E2E.Catalogue (catalogue)
import Ecluse.E2E.Drivers.PyPI (pypiDriver)
import Ecluse.Runtime.Test.Contract.UnitCatalogue (runtimeUnitCatalogue)
import Ecluse.Test.Contract.Catalogue (combineCatalogues)
import Ecluse.Test.Contract.Completion (CompletionScope (FullEcosystem))
import Ecluse.Test.Contract.Ecosystem (ecosystemCompletion)
import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Manifest (auditCompletionFiles, failRunner)
import Ecluse.Test.Contract.PackageStore (packageStoreCatalogue)
import Ecluse.Test.Contract.Result (makePlan)
import Ecluse.Test.Contract.UnitCatalogue (coreUnitCatalogue, expectedUnitCatalogue)
import Ecluse.Test.Contract.UnitReport (unitCatalogueFor)

main :: IO ()
main = do
    core <-
        either (failRunner . T.unlines) pure $
            makePlan (unitCatalogueFor "core-unit" (combineCatalogues [expectedUnitCatalogue coreUnitCatalogue, packageStoreCatalogue "verdaccio"]))
    runtime <-
        either (failRunner . T.unlines) pure $
            makePlan (unitCatalogueFor "runtime-unit" (combineCatalogues [expectedUnitCatalogue runtimeUnitCatalogue, packageStoreCatalogue "codeartifact"]))
    integration <- either (failRunner . T.unlines) pure (makePlan integrationCatalogue)
    e2e <- either (failRunner . T.unlines) pure (makePlan catalogue)
    getArgs >>= \case
        [corePlan, coreReport, runtimePlan, runtimeReport, integrationPlan, integrationReport, e2ePlan, subjectReport] ->
            auditCompletionFiles
                (ecosystemCompletion FullEcosystem pypiDriver)
                [ (core, corePlan, [coreReport])
                , (runtime, runtimePlan, [runtimeReport])
                , (integration, integrationPlan, [integrationReport])
                , (e2e, e2ePlan, [subjectReport])
                ]
        _ -> failRunner "usage: ecluse-completion-pypi-audit CORE_UNIT_PLAN CORE_UNIT_REPORT RUNTIME_UNIT_PLAN RUNTIME_UNIT_REPORT INTEGRATION_PLAN INTEGRATION_REPORT E2E_PLAN SUBJECT_REPORT"
