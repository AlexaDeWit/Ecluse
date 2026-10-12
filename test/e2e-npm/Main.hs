-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run the npm ecosystem's full E2E contract.
module Main (main) where

import Ecluse.E2E.Catalogue (catalogue, prerequisites)
import Ecluse.E2E.Suites.Npm (declarations)
import Ecluse.Test.Contract.Runner (contractMain)

main :: IO ()
main = contractMain catalogue "npm" declarations prerequisites
