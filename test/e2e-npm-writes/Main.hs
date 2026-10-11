-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run the fixed npm-writes E2E suite.
module Main (main) where

import Ecluse.E2E.Catalogue (catalogue, prerequisites)
import Ecluse.E2E.Suites.NpmWrites (declarations)
import Ecluse.Test.Contract.Runner (contractMain)

main :: IO ()
main = contractMain catalogue "npm-writes" declarations prerequisites
