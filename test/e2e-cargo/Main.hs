-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run the fixed Cargo E2E subject.
module Main (main) where

import Ecluse.E2E.Catalogue (catalogue, prerequisites)
import Ecluse.E2E.Suites.Cargo (declarations)
import Ecluse.Test.Contract.Runner (contractMain)

main :: IO ()
main = contractMain catalogue "cargo" declarations prerequisites
