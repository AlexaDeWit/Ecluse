-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure catalogue discovery precedes E2E prerequisite checks and resource acquisition.
module Main (main) where

import Ecluse.E2E.Catalogue (catalogue, groupSpecs, prerequisites)
import Ecluse.Test.Contract.Runner (contractMain)

main :: IO ()
main = contractMain catalogue groupSpecs prerequisites
