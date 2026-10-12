-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit the fixed pypi completion definition against receipts from all required tiers.
module Main (main) where

import Ecluse.E2E.Drivers.PyPI (pypiDriver)
import Ecluse.Test.Contract.Completion (CompletionScope (FullEcosystem))
import Ecluse.Test.Contract.CompletionAudit (completionAuditMain)
import Ecluse.Test.Contract.Ecosystem (ecosystemCompletion)

main :: IO ()
main = completionAuditMain "ecluse-completion-pypi-audit" (ecosystemCompletion FullEcosystem pypiDriver)
