-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure verdaccio conformance discovery without service acquisition.
module Main (main) where

import Ecluse.Test.Contract.Manifest (failRunner, writePlan)
import Ecluse.Test.Contract.PackageStore (packageStoreCatalogue, validatePackageStore)

-- | This fixed entrypoint accepts no selector arguments.
main :: IO ()
main = do
    validatePackageStore "verdaccio"
    arguments <- getArgs
    unless (null arguments) $ failRunner "usage: ecluse-conformance-verdaccio-manifest"
    writePlan (packageStoreCatalogue "verdaccio")
