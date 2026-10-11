-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Main (main) where

import Ecluse.E2E.Catalogue (catalogue)
import Ecluse.E2E.Suites.NpmServing qualified as NpmServing
import Ecluse.E2E.Suites.NpmWrites qualified as NpmWrites
import Ecluse.E2E.Suites.PyPI qualified as PyPI
import Ecluse.E2E.Suites.SharedSystem qualified as SharedSystem
import Ecluse.Test.Contract.Manifest (failRunner, writePlan)
import Ecluse.Test.Contract.Runner (validateDeclaredGroups)

main :: IO ()
main = do
    arguments <- getArgs
    unless (null arguments) $ failRunner "usage: ecluse-e2e-manifest"
    validateDeclaredGroups catalogue "npm-serving" NpmServing.declarations
    validateDeclaredGroups catalogue "npm-writes" NpmWrites.declarations
    validateDeclaredGroups catalogue "pypi" PyPI.declarations
    validateDeclaredGroups catalogue "shared-system" SharedSystem.declarations
    writePlan catalogue
