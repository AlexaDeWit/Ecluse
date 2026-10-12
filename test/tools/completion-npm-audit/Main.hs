-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit the fixed npm completion definition against receipts from both tiers.
module Main (main) where

import Data.Text qualified as T

import Ecluse.E2E.Catalogue (catalogue)
import Ecluse.E2E.Drivers.Npm (npmDriver)
import Ecluse.Test.Contract.Completion (CompletionScope (FullEcosystem))
import Ecluse.Test.Contract.Ecosystem (ecosystemCompletion)
import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Manifest (auditCompletionFiles, failRunner)
import Ecluse.Test.Contract.Result (makePlan)

main :: IO ()
main = do
    integration <- either (failRunner . T.unlines) pure (makePlan integrationCatalogue)
    e2e <- either (failRunner . T.unlines) pure (makePlan catalogue)
    getArgs >>= \case
        [integrationPlan, integrationReport, e2ePlan, subjectReport] ->
            auditCompletionFiles
                (ecosystemCompletion FullEcosystem npmDriver)
                [(integration, integrationPlan, [integrationReport]), (e2e, e2ePlan, [subjectReport])]
        _ -> failRunner "usage: ecluse-completion-npm-audit INTEGRATION_PLAN INTEGRATION_REPORT E2E_PLAN SUBJECT_REPORT"
