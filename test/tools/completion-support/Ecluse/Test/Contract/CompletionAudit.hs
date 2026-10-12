-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Assemble provider plans for fixed ecosystem completion audits.
module Ecluse.Test.Contract.CompletionAudit (completionAuditMain) where

import Data.Text qualified as T

import Ecluse.E2E.Catalogue (catalogue)
import Ecluse.Runtime.Test.Contract.UnitCatalogue (runtimeUnitCatalogue)
import Ecluse.Runtime.Test.PackageStore (codeArtifactCatalogue)
import Ecluse.Test.Contract.Catalogue (combineCatalogues)
import Ecluse.Test.Contract.Completion (Completion)
import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Manifest (auditCompletionFiles, failRunner)
import Ecluse.Test.Contract.PackageStore (packageStoreCatalogue)
import Ecluse.Test.Contract.Result (makePlan)
import Ecluse.Test.Contract.UnitCatalogue (coreUnitCatalogue, expectedUnitCatalogue)
import Ecluse.Test.Contract.UnitReport (unitCatalogueFor)

-- | Audit eight receipt paths against an explicit completion definition and compiled provider plans.
completionAuditMain :: Text -> Completion -> IO ()
completionAuditMain executable completion = do
    core <-
        either (failRunner . T.unlines) pure $
            makePlan (unitCatalogueFor "core-unit" (combineCatalogues [expectedUnitCatalogue coreUnitCatalogue, packageStoreCatalogue "verdaccio"]))
    runtime <-
        either (failRunner . T.unlines) pure $
            makePlan (unitCatalogueFor "runtime-unit" (combineCatalogues [expectedUnitCatalogue runtimeUnitCatalogue, codeArtifactCatalogue]))
    integration <- either (failRunner . T.unlines) pure (makePlan integrationCatalogue)
    e2e <- either (failRunner . T.unlines) pure (makePlan catalogue)
    getArgs >>= \case
        [corePlan, coreReport, runtimePlan, runtimeReport, integrationPlan, integrationReport, e2ePlan, subjectReport] ->
            auditCompletionFiles
                completion
                [ (core, corePlan, [coreReport])
                , (runtime, runtimePlan, [runtimeReport])
                , (integration, integrationPlan, [integrationReport])
                , (e2e, e2ePlan, [subjectReport])
                ]
        _ -> failRunner $ "usage: " <> executable <> " CORE_UNIT_PLAN CORE_UNIT_REPORT RUNTIME_UNIT_PLAN RUNTIME_UNIT_REPORT INTEGRATION_PLAN INTEGRATION_REPORT E2E_PLAN SUBJECT_REPORT"
