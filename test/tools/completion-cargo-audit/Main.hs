-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Audit native Cargo completion against execution receipts from every required tier.
module Main (main) where

import Ecluse.E2E.Drivers.Cargo (cargoDriver)
import Ecluse.Test.Contract.Completion (CompletionScope (FullEcosystem))
import Ecluse.Test.Contract.CompletionAudit (completionAuditMain)
import Ecluse.Test.Contract.Ecosystem (ecosystemCompletion)

main :: IO ()
main = completionAuditMain "ecluse-completion-cargo-audit" (ecosystemCompletion FullEcosystem cargoDriver)
