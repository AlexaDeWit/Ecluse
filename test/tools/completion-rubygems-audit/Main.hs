-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Require actual native RubyGems execution receipts from each applicable tier.
module Main (main) where

import Ecluse.E2E.Drivers.RubyGems (rubyDriver)
import Ecluse.Test.Contract.Completion (CompletionScope (FullEcosystem))
import Ecluse.Test.Contract.CompletionAudit (completionAuditMain)
import Ecluse.Test.Contract.Ecosystem (ecosystemCompletion)

main :: IO ()
main = completionAuditMain "ecluse-completion-rubygems-audit" (ecosystemCompletion FullEcosystem rubyDriver)
